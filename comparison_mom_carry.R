# ==============================================================================
# STRATEGY COMPARISON: WTI MOMENTUM VS CARRY (Rebased to 2006-10-02)
# ==============================================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(gridExtra)
library(zoo)
library(TTR)
if (!require("writexl")) install.packages("writexl")
library(writexl)

# -----------------------------
# INPUTS
# -----------------------------
FILE_MOMENTUM <- "Super Puper Oil.xlsx"  # Sheet 1, Headers in Row 2
FILE_CARRY    <- "WTI_TS.xlsx"           # Carry data

# Parameters
TARGET_VOL <- 0.15
VOL_WINDOW <- 60
MA_WINDOW  <- 20
LEV_CAP    <- 1.0

# ** NEW: Date to start the Series / Plot **
PLOT_START_DATE <- as.Date("2006-10-02")

# -----------------------------
# HELPER: Date Parsing
# -----------------------------
parse_excel_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  suppressWarnings(as.Date(as.character(x)))
}

# ==============================================================================
# 1) DATA LOADING & PROCESSING
# ==============================================================================

# --- A. MOMENTUM DATA (Super Puper Oil) ---
tryCatch({
  # Skip row 1 (titles), read headers from row 2
  mom_raw <- read_excel(FILE_MOMENTUM, skip = 1) %>% 
    select(1, 2) %>% 
    set_names(c("date", "price_front")) %>% 
    mutate(date = parse_excel_date(date)) %>% 
    filter(!is.na(date), !is.na(price_front)) %>% 
    arrange(date)
  
}, error = function(e) {
  stop("Error reading Momentum file. Ensure 'Super Puper Oil.xlsx' exists.\nDetails: ", e$message)
})

# Process Momentum
mom_processed <- mom_raw %>%
  mutate(
    price_safe      = pmax(price_front, 0.01), 
    daily_price_ret = log(price_safe / lag(price_safe)),
    ma_20           = SMA(price_front, n = MA_WINDOW),
    mom_raw         = ifelse(price_front >= ma_20, 1, -1)
  ) %>%
  # Remove NAs created by Lag/SMA *before* Vol calc
  drop_na(daily_price_ret, mom_raw) %>%
  mutate(
    vol_mom = runSD(daily_price_ret, n = VOL_WINDOW) * sqrt(252),
    vol_mom = lag(vol_mom) 
  ) %>%
  select(date, mom_ret = daily_price_ret, mom_raw, vol_mom) %>%
  drop_na()


# --- B. CARRY DATA (WTI_TS.xlsx) ---
tryCatch({
  carry_raw <- read_excel(FILE_CARRY) %>%
    mutate(date = parse_excel_date(Date)) %>%
    select(date, F1_Price, F13_Price) %>%
    # Remove any rows with missing prices immediately
    drop_na(F1_Price, F13_Price) %>%
    arrange(date)
  
}, error = function(e) {
  stop("Error reading Carry file. Please ensure 'WTI_TS.xlsx' exists.\nDetails: ", e$message)
})

# Process Carry
carry_processed <- carry_raw %>%
  mutate(
    # 1. Returns
    carry_underlying_ret = log(F1_Price / lag(F1_Price)),
    
    # 2. Signal (Long Backwardation, Short Contango)
    carry_spread     = F1_Price - F13_Price,
    carry_signal_raw = sign(carry_spread),
    carry_signal_raw = ifelse(carry_signal_raw == 0, 1, carry_signal_raw)
  ) %>%
  # Drop first NA from lag()
  drop_na(carry_underlying_ret) %>%
  mutate(
    # 3. Volatility
    vol_carry = runSD(carry_underlying_ret, n = VOL_WINDOW) * sqrt(252),
    vol_carry = lag(vol_carry)
  ) %>%
  select(date, carry_ret = carry_underlying_ret, carry_signal_raw, vol_carry) %>%
  drop_na()


# ==============================================================================
# 2) MERGE & REBALANCE (Monthly)
# ==============================================================================
full_data <- inner_join(mom_processed, carry_processed, by = "date") %>%
  mutate(month_id = floor_date(date, "month"))

monthly_rebal <- full_data %>%
  group_by(month_id) %>%
  slice_tail(n = 1) %>% 
  ungroup() %>%
  mutate(
    scalar_mom   = pmin(TARGET_VOL / vol_mom, LEV_CAP),
    scalar_carry = pmin(TARGET_VOL / vol_carry, LEV_CAP),
    
    pos_momentum = mom_raw * scalar_mom,
    pos_carry    = carry_signal_raw * scalar_carry,
    
    join_month_id = month_id %m+% months(1)
  ) %>%
  select(join_month_id, pos_momentum, pos_carry) %>%
  filter(is.finite(pos_momentum), is.finite(pos_carry))

# ==============================================================================
# 3) PERFORMANCE & PLOTTING (Strict Rebase to 0 at Start Date)
# ==============================================================================
strategy_wide <- full_data %>%
  left_join(monthly_rebal, by = c("month_id" = "join_month_id")) %>%
  arrange(date) %>%
  tidyr::fill(pos_momentum, pos_carry, .direction = "down") %>%
  filter(!is.na(pos_momentum), !is.na(pos_carry)) %>%
  mutate(
    # 1. Calculate Daily Strategy Returns
    ret_mom_strat   = pos_momentum * mom_ret,
    ret_carry_strat = pos_carry * carry_ret,
    
    # 2. Calculate Cumulative Index (starting from beginning of data)
    idx_mom   = cumsum(ret_mom_strat),
    idx_carry = cumsum(ret_carry_strat)
  ) %>%
  
  # --- [CRITICAL FIX] Filter Date Range & Rebase to 0 ---
  filter(date >= PLOT_START_DATE) %>%
  mutate(
    # Subtract the value of the FIRST row in this filtered set to force start at 0
    cum_mom   = exp(idx_mom - first(idx_mom)) - 1,
    cum_carry = exp(idx_carry - first(idx_carry)) - 1
  ) %>%
  select(date, ret_mom = ret_mom_strat, ret_carry = ret_carry_strat, cum_mom, cum_carry)

strategy_long <- strategy_wide %>%
  select(date, cum_mom, cum_carry) %>%
  pivot_longer(cols = c("cum_mom", "cum_carry"), names_to = "Strategy", values_to = "Return")

# --- PLOTS ---
cols   <- c("cum_mom" = "#E74C3C", "cum_carry" = "#2E5FA1")
labels <- c("cum_mom" = "Momentum (MA20)", "cum_carry" = "Carry (F1 - F13)")

final_plot <- ggplot(strategy_long, aes(x = date, y = Return, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = cols, labels = labels) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = paste("Strategy Comparison (Rebased to 0 at", PLOT_START_DATE, ")"), 
       y = "Cumulative Return", x = "") +
  theme_minimal() + theme(legend.position = "top", legend.title = element_blank())

gridExtra::grid.arrange(final_plot) # Just print the combined plot
print(final_plot)

# --- STATS ---
ann_stats <- function(x) {
  mu  <- mean(x, na.rm = TRUE) * 252
  vol <- sd(x, na.rm = TRUE) * sqrt(252)
  sr  <- ifelse(vol > 0, mu / vol, NA_real_)
  c(mu = mu, vol = vol, sharpe = sr)
}
mom_stats   <- ann_stats(strategy_wide$ret_mom)
carry_stats <- ann_stats(strategy_wide$ret_carry)
cor_mc      <- cor(strategy_wide$ret_mom, strategy_wide$ret_carry, use = "complete.obs")

cat("\n=== COMPARISON STATS (Since 2006-10-02) ===\n")
cat("Correlation: ", round(cor_mc, 3), "\n\n")
cat("Momentum (Original):\n")
cat("  Total Return:", percent(last(strategy_wide$cum_mom)), "\n")
cat("  Sharpe:      ", round(mom_stats["sharpe"], 2), "\n\n")
cat("Carry (New WTI_TS):\n")
cat("  Total Return:", percent(last(strategy_wide$cum_carry)), "\n")
cat("  Sharpe:      ", round(carry_stats["sharpe"], 2), "\n")

# ==============================================================================
# 4) EXPORT TO EXCEL
# ==============================================================================
# Create a clean dataframe for export
export_data <- strategy_wide %>%
  select(date, cum_mom, cum_carry) %>%
  rename(Date = date, 
         Momentum_Cumulative = cum_mom, 
         Carry_Cumulative = cum_carry)

# Write to Excel file in your working directory
write_xlsx(export_data, "WTI_Strategy_Returns_2006.xlsx")

cat("\nData successfully exported to 'WTI_Strategy_Returns_2006.xlsx'\n")
# ==============================================================================

# ==============================================================================
# 3) PERFORMANCE & PLOTTING ($100 invested, rebased to 100 at first shared date)
# ==============================================================================

strategy_wide <- full_data %>%
  left_join(monthly_rebal, by = c("month_id" = "join_month_id")) %>%
  arrange(date) %>%
  tidyr::fill(pos_momentum, pos_carry, .direction = "down") %>%
  filter(!is.na(pos_momentum), !is.na(pos_carry)) %>%
  mutate(
    # Daily strategy returns
    ret_mom_strat   = pos_momentum * mom_ret,
    ret_carry_strat = pos_carry * carry_ret
  ) %>%
  # Optional: if you still want to force a specific start date, keep this line
  # filter(date >= PLOT_START_DATE) %>%
  mutate(
    # Wealth indices from $100 invested
    wealth_mom_raw   = 100 * exp(cumsum(ret_mom_strat)),
    wealth_carry_raw = 100 * exp(cumsum(ret_carry_strat)),
    
    # Rebase BOTH to 100 at the first row of the (shared) sample
    wealth_mom   = 100 * wealth_mom_raw   / first(wealth_mom_raw),
    wealth_carry = 100 * wealth_carry_raw / first(wealth_carry_raw)
  ) %>%
  select(date,
         ret_mom   = ret_mom_strat,
         ret_carry = ret_carry_strat,
         wealth_mom,
         wealth_carry)

strategy_long <- strategy_wide %>%
  select(date, wealth_mom, wealth_carry) %>%
  pivot_longer(cols = c("wealth_mom", "wealth_carry"),
               names_to = "Strategy",
               values_to = "Wealth")

# --- PLOT (Wealth index in $ terms, starting at 100) ---
cols   <- c("wealth_mom" = "#E74C3C", "wealth_carry" = "#2E5FA1")
labels <- c("wealth_mom" = "Momentum (MA20)", "wealth_carry" = "Carry (F1 - F13)")

final_plot <- ggplot(strategy_long, aes(x = date, y = Wealth, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = cols, labels = labels) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(
    title = "WTI Strategy Comparison",
    y = "Growth of $100 invested",
    x = ""
  ) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

print(final_plot)

export_data <- strategy_wide %>%
  transmute(
    Date = date,
    Momentum_Wealth_100 = wealth_mom,
    Carry_Wealth_100    = wealth_carry
  )

write_xlsx(export_data, "WTI_Strategy_WealthIndex_100.xlsx")


# ==============================================================================
# PERFORMANCE METRICS (Total, CAGR, Sharpe, Ann.Vol, MaxDD)
# ==============================================================================

perf_metrics <- function(dates, log_ret, wealth_index_100) {
  # Convert daily log returns to arithmetic daily returns
  ar_ret <- exp(log_ret) - 1
  
  # Sample length in years (trading days convention)
  n <- sum(!is.na(ar_ret))
  yrs <- n / 252
  
  # Total return from wealth index (rebased to 100)
  total_ret <- last(wealth_index_100) / first(wealth_index_100) - 1
  
  # Annualised return (CAGR) from wealth and time
  ann_ret <- (last(wealth_index_100) / first(wealth_index_100))^(1 / yrs) - 1
  
  # Annualised volatility
  ann_vol <- sd(ar_ret, na.rm = TRUE) * sqrt(252)
  
  # Sharpe (rf = 0)
  ann_mu <- mean(ar_ret, na.rm = TRUE) * 252
  sharpe <- ifelse(ann_vol > 0, ann_mu / ann_vol, NA_real_)
  
  # Max drawdown from wealth index
  wealth <- as.numeric(wealth_index_100)
  running_max <- cummax(wealth)
  drawdown <- wealth / running_max - 1
  max_dd <- min(drawdown, na.rm = TRUE)
  
  tibble(
    total_return      = total_ret,
    annualised_return = ann_ret,
    sharpe            = sharpe,
    annualised_vola   = ann_vol,
    max_drawdown      = max_dd
  )
}

mom_perf <- perf_metrics(strategy_wide$date, strategy_wide$ret_mom, strategy_wide$wealth_mom) %>%
  mutate(strategy = "Momentum (MA20)")
carry_perf <- perf_metrics(strategy_wide$date, strategy_wide$ret_carry, strategy_wide$wealth_carry) %>%
  mutate(strategy = "Carry (F1 - F13)")

perf_table <- bind_rows(mom_perf, carry_perf) %>%
  select(strategy, everything()) %>%
  mutate(
    total_return      = percent(total_return, accuracy = 0.1),
    annualised_return = percent(annualised_return, accuracy = 0.1),
    annualised_vola   = percent(annualised_vola, accuracy = 0.1),
    max_drawdown      = percent(max_drawdown, accuracy = 0.1),
    sharpe            = round(sharpe, 2)
  )

print(perf_table)
