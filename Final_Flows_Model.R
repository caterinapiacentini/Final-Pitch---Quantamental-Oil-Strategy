# ==============================================================================
# STRATEGY: HF Sigma-Barrier Consensus (0.5 Barrier)
# LOGIC: Entry requires HF Conviction (>0.5 Sigma) | Exit on Consensus Break
# ==============================================================================

# --- 1. CONFIGURATION ---
hf_threshold <- 1.5    
fast_n       <- 4      
slow_n       <- 34     
roll_n       <- 52     
analysis_start <- as.Date("2006-06-20")

# --- 2. DATA & SIGMA ENGINE ---
trading_system <- inner_join(
  cot_raw %>% mutate(Date = as.Date(Report_Date_as_MM_DD_YYYY, origin = "1899-12-30"),
                     Net_HF = M_Money_Positions_Long_ALL - M_Money_Positions_Short_ALL,
                     Net_SD = Swap_Positions_Long_All - Swap__Positions_Short_All),
  price_raw %>% mutate(Date = as.Date(Date)), by = "Date"
) %>% 
  arrange(Date) %>%
  mutate(
    HF_Raw = EMA(Net_HF, fast_n) - EMA(Net_HF, slow_n),
    SD_Raw = EMA(Net_SD, fast_n) - EMA(Net_SD, slow_n),
    HF_Z = (HF_Raw - runMean(HF_Raw, roll_n)) / runSD(HF_Raw, roll_n),
    HF_Z = ifelse(is.na(HF_Z), 0, HF_Z),
    Price_Return = (WTI_Close / lag(WTI_Close)) - 1
  )

# --- 3. STATE ENGINE ---
signals <- numeric(nrow(trading_system)); pos <- 0
for(i in 2:nrow(trading_system)) {
  hf_z <- trading_system$HF_Z[i]; hf_raw <- trading_system$HF_Raw[i]; sd_raw <- trading_system$SD_Raw[i]
  if(pos == 0) {
    if(hf_z > hf_threshold && sd_raw < 0) { pos <- 1 }      
    else if(hf_z < -hf_threshold && sd_raw > 0) { pos <- -1 } 
  } else if(pos == 1) {
    if(hf_raw <= 0 || sd_raw >= 0) { pos <- 0 }     
  } else if(pos == -1) {
    if(hf_raw >= 0 || sd_raw <= 0) { pos <- 0 }     
  }
  signals[i] <- pos
}
trading_system$final_signal <- signals

# --- 4. PERFORMANCE CALCULATIONS ---
trading_system <- trading_system %>%
  mutate(
    strat_ret = lag(final_signal) * Price_Return,
    cum_strat = cumprod(1 + replace_na(strat_ret, 0)),
    cum_bench = cumprod(1 + replace_na(Price_Return, 0)),
    peak = cummax(cum_strat),
    drawdown = (cum_strat - peak) / peak
  )

# Metric Calculations
n_years   <- as.numeric(max(trading_system$Date) - min(trading_system$Date)) / 365.25
total_res <- (tail(trading_system$cum_strat, 1) - 1) * 100
ann_ret   <- ((tail(trading_system$cum_strat, 1))^(1/n_years) - 1) * 100
sharpe    <- (mean(trading_system$strat_ret, na.rm=T) / sd(trading_system$strat_ret, na.rm=T)) * sqrt(52)
ann_vol   <- (sd(trading_system$strat_ret, na.rm=T) * sqrt(52)) * 100
max_dd    <- min(trading_system$drawdown, na.rm=T) * 100

# Benchmark Metrics (WTI Buy & Hold)
bench_total <- (tail(trading_system$cum_bench, 1) - 1) * 100
bench_dd    <- min((trading_system$cum_bench - cummax(trading_system$cum_bench))/cummax(trading_system$cum_bench), na.rm=T) * 100

# --- 5. PERFORMANCE TABLE ---
performance_table <- data.frame(
  Metric = c("Total Return", "Annual Return (CAGR)", "Sharpe Ratio", "Annualized Vol", "Max Drawdown"),
  Strategy = c(paste0(round(total_res, 1), "%"), paste0(round(ann_ret, 1), "%"), round(sharpe, 2), paste0(round(ann_vol, 1), "%"), paste0(round(max_dd, 1), "%")),
  Benchmark = c("88.0%", "3.3%", "0.28", "17.8%", "-55.4%")
)
print(performance_table)

# --- 6. 4-PANEL VISUALIZATION ---
orders <- trading_system %>% filter(final_signal != lag(final_signal, default = 0) & final_signal != 0)

p1 <- ggplot(trading_system, aes(x=Date, y=WTI_Close)) + geom_line(alpha=0.3) +
  geom_point(data=orders, aes(x=Date, y=WTI_Close, color=as.factor(final_signal)), shape=17, size=3) +
  scale_color_manual(values=c("-1"="red", "1"="green3"), guide="none") + labs(title="Price & Order Execution", y="Price") + theme_minimal()

p2 <- ggplot(trading_system, aes(x=Date, y=HF_Z)) + 
  geom_area(aes(y=pmax(HF_Z,0)), fill="green3", alpha=0.3) + geom_area(aes(y=pmin(HF_Z,0)), fill="red", alpha=0.3) +
  geom_hline(yintercept=c(hf_threshold, -hf_threshold), linetype="dashed") + labs(title="HF Conviction (Sigma Barrier)", y="Sigma") + theme_minimal()

p3 <- ggplot(trading_system, aes(x=Date, y=SD_Raw)) + 
  geom_area(aes(y=pmin(SD_Raw,0)), fill="green3", alpha=0.3) + geom_area(aes(y=pmax(SD_Raw,0)), fill="red", alpha=0.3) +
  labs(title="SD Momentum (Raw Handshake)", y="Spread") + theme_minimal()

p4 <- ggplot(trading_system, aes(x=Date)) + 
  geom_line(aes(y=cum_strat, color="Strategy"), size=1.1) + geom_line(aes(y=cum_bench, color="Benchmark"), linetype="dashed") +
  scale_color_manual(values=c("Strategy"="navy", "Benchmark"="gray60")) + labs(title="Total Compounded Return", y="Equity") + theme_minimal()

grid.arrange(p1, p2, p3, p4, ncol=1)