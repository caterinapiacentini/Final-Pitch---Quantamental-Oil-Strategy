# ==============================================================================
# STRATEGY: HF Sigma-Barrier Consensus (0.5 Barrier)
# LOGIC: Entry requires HF Conviction (>0.5 Sigma) | Exit on Consensus Break
# ==============================================================================

# load required libraries for the following code
library(dplyr)
library(ggplot2)
library(gridExtra)
library(TTR)
# Assume cot_raw and price_raw data frames are already loaded in the environment
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

library(scales)

# 1) Scala X comune (stessi anni su tutti)
x_min <- min(trading_system$Date, na.rm = TRUE)
x_max <- max(trading_system$Date, na.rm = TRUE)

x_common <- scale_x_date(
  limits  = c(x_min, x_max),
  breaks  = pretty_breaks(n = 6),   # oppure date_breaks("3 years")
  labels  = date_format("%Y"),
  expand  = c(0, 0)
)

# 2) Applica la scala comune a tutti e tre
p1 <- p1 + x_common
p2 <- p2 + x_common
p3 <- p3 + x_common

# 3) Rimuovi asse X da p1 e p2 (lascia solo sotto in p3)
p1 <- p1 + theme(
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  axis.ticks.x = element_blank()
)

p2 <- p2 + theme(
  axis.title.x = element_blank(),
  axis.text.x  = element_blank(),
  axis.ticks.x = element_blank()
)

# (Opzionale) anche su p3: se non vuoi il titolo "Date" ma solo gli anni
p3 <- p3 + theme(axis.title.x = element_blank())

library(ggplot2)
library(gridExtra)
library(grid)
library(ragg)

# 1) Remove plot titles (and subtitles)
remove_titles <- theme(
  plot.title    = element_blank(),
  plot.subtitle = element_blank()
)

p1 <- p1 + remove_titles
p2 <- p2 + remove_titles
p3 <- p3 + remove_titles

# (Opzionale) Se vuoi anche togliere i titoli degli assi Y
# p1 <- p1 + theme(axis.title.y = element_blank())
# p2 <- p2 + theme(axis.title.y = element_blank())
# p3 <- p3 + theme(axis.title.y = element_blank())

# 2) Rebuild the grob AFTER modifications (this is the key)
g_3panel <- arrangeGrob(
  p1, p2, p3,
  ncol = 1,
  heights = c(1, 1, 1),
  padding = unit(0, "pt")
)

# 3) Preview in RStudio (optional)
grid.newpage()
grid.draw(g_3panel)

# 4) Export exactly 31.01cm x 5.72cm @ 600 dpi
W_CM <- 31.01
H_CM <- 5.72

ragg::agg_png(
  filename = "cftc_3panel_31.01x5.72cm_notitles.png",
  width = W_CM, height = H_CM, units = "cm",
  res = 600
)
grid::grid.newpage()
grid::grid.draw(g_3panel)
dev.off()

