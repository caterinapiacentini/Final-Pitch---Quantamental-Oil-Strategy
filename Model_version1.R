########## Model using WTI data from Bloomberg (Monthly Rebalancing)
library(tidyverse)
library(readxl)
library(lubridate)
library(TTR)
library(scales)
library(gridExtra)
library(ggplot2)
library(zoo)

# ==============================================================================
# 1) Read Futures Data (CL1) from Clean_WTI
# ==============================================================================
# Ensure "Super Puper Oil.xlsx" is in your working directory
# We wrap this in tryCatch to allow for copy-pasting without the file present
tryCatch({
  Super_Puper_Oil <- read_excel("Super Puper Oil.xlsx", sheet = "Foglio1", skip = 1)
  
  futures_data <- Super_Puper_Oil %>%
    select(Date, Close) %>%
    rename(date = Date, fut_price = Close) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date("1993-01-01") & date <= as.Date("2022-12-31"))
  
  # Linear interpolation for NAs
  futures_data <- futures_data %>%
    arrange(date) %>%
    mutate(fut_price = na.approx(fut_price, na.rm = FALSE)) %>%
    na.omit()
  
}, error = function(e) {
  message("Error reading file: Ensure 'Super Puper Oil.xlsx' exists. Using dummy data for demo.")
  # Dummy data generation for code validation if file is missing
  dates <- seq(as.Date("2000-01-01"), as.Date("2022-12-31"), by="day")
  futures_data <- data.frame(date = dates, fut_price = cumprod(c(20, 1 + rnorm(length(dates)-1, 0, 0.02))))
})

# ==============================================================================
# 2) Strategy: Monthly Rebalancing (Signal at Month End -> Trade Next Month)
# ==============================================================================

# A) Calculate Indicators Daily
daily_calc <- futures_data %>%
  mutate(
    ma_20 = SMA(fut_price, n = 20),
    # Define a 'month_id' to group by month later
    month_id = floor_date(date, "month")
  ) %>%
  na.omit()

# B) Extract Signal on the Last Trading Day of Each Month
monthly_signals <- daily_calc %>%
  group_by(month_id) %>%
  # Take the last available row for each month (Last Trading Day)
  slice_tail(n = 1) %>% 
  ungroup() %>%
  mutate(
    # Signal logic: If Price > MA20 on last day, Go Long (1), else Short (-1)
    monthly_raw_signal = ifelse(fut_price - ma_20 >= 0, 1, -1),
    
    # We want this signal to apply to the *Next* Month.
    # So we shift the 'month_id' forward by 1 month to join it back to daily data.
    join_month_id = month_id %m+% months(1)
  ) %>%
  select(join_month_id, monthly_raw_signal)

# C) Merge Monthly Signal back to Daily Data
strategy_df <- daily_calc %>%
  # Join based on the month the daily data belongs to
  left_join(monthly_signals, by = c("month_id" = "join_month_id")) %>%
  mutate(
    # 'position' is now constant for the whole month based on prev month's signal
    # Fill NAs at the start (before first signal) with 0 or exclude them
    position = replace_na(monthly_raw_signal, 0),
    
    # Daily changes
    fut_price_change = fut_price - lag(fut_price),
    dollar_pnl = position * fut_price_change
  ) %>%
  na.omit() %>%
  mutate(cum_pnl = cumsum(dollar_pnl))

# ==============================================================================
# 3) Visualization in Dollar
# ==============================================================================
plot_cum <- ggplot(strategy_df, aes(x = date, y = cum_pnl)) +
  geom_line(linewidth = 0.8) +
  geom_area(alpha = 0.1) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "WTI Futures Momentum (Monthly Rebalance)",
    subtitle = "Signal: Last Day of Month (Price vs MA20) | Held for Next Month",
    y = "Cumulative P&L ($)",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

annual_stats <- strategy_df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(total_pnl = sum(dollar_pnl, na.rm = TRUE), .groups = "drop") %>%
  mutate(fill_color = ifelse(total_pnl >= 0, "Positive", "Negative"))

plot_annual <- ggplot(annual_stats, aes(x = factor(year), y = total_pnl, fill = fill_color)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_y_continuous(labels = dollar_format()) +
  labs(y = "Annual P&L ($)", x = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

grid.arrange(plot_cum, plot_annual, ncol = 1, heights = c(2, 1))

# ==============================================================================
# 4) Quick Stats Output
# ==============================================================================
cat("=== Dollar P&L Stats ===\n")
cat("Total Profit per Barrel: ", dollar(sum(strategy_df$dollar_pnl)), "\n")
if(nrow(annual_stats) > 0) {
  cat("Best Year: ", annual_stats$year[which.max(annual_stats$total_pnl)], 
      "(", dollar(max(annual_stats$total_pnl)), ")\n")
  cat("Worst Year: ", annual_stats$year[which.min(annual_stats$total_pnl)], 
      "(", dollar(min(annual_stats$total_pnl)), ")\n")
}

# End-of-Year table
eoy_table <- strategy_df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    Annual_PnL = sum(dollar_pnl),
    Cumulative_PnL = last(cum_pnl),
    .groups = "drop"
  ) %>%
  mutate(
    Annual_PnL = dollar(Annual_PnL),
    Cumulative_PnL = dollar(Cumulative_PnL)
  )
print(as.data.frame(eoy_table), row.names = FALSE)

# ==============================================================================
# 5) Visualization in Returns (Log Returns)
# ==============================================================================
strategy_df <- strategy_df %>%
  mutate(
    fut_logret = log(fut_price / lag(fut_price)),
    # Position is already lagged/monthly aligned in previous steps
    strat_logret = position * fut_logret,
    cum_strat_logret = cumsum(replace_na(strat_logret, 0)),
    cum_ret_log = exp(cum_strat_logret) - 1
  )

annual_stats_log <- strategy_df %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarise(
    annual_logret = sum(strat_logret, na.rm = TRUE),
    annual_ret_log = exp(annual_logret) - 1,
    .groups = "drop"
  ) %>%
  mutate(fill_color = ifelse(annual_ret_log >= 0, "Positive", "Negative"))

plot_cum_log <- ggplot(strategy_df, aes(x = date, y = cum_ret_log)) +
  geom_line(linewidth = 0.8) +
  geom_area(alpha = 0.1) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "WTI Momentum (Monthly Rebalance) - Log Return View",
    subtitle = "Rebalanced Monthly | cum = exp(cumsum(strategy log returns)) - 1",
    y = "Cumulative Return",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

plot_annual_log <- ggplot(annual_stats_log, aes(x = factor(year), y = annual_ret_log, fill = fill_color)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(y = "Annual Return", x = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

grid.arrange(plot_cum_log, plot_annual_log, ncol = 1, heights = c(2, 1))

# End-of-Year table for returns
eoy_table_log <- strategy_df %>%
  mutate(year = lubridate::year(date)) %>%
  group_by(year) %>%
  summarise(
    Annual_LogRet = sum(strat_logret, na.rm = TRUE),
    Annual_Ret_Log = exp(Annual_LogRet) - 1,
    Cumulative_LogRet = last(cum_strat_logret),
    Cumulative_Ret_Log = exp(Cumulative_LogRet) - 1,
    .groups = "drop"
  ) %>%
  mutate(
    Annual_Ret_Log = percent(Annual_Ret_Log, accuracy = 0.01),
    Cumulative_Ret_Log = percent(Cumulative_Ret_Log, accuracy = 0.01)
  )
print(as.data.frame(eoy_table_log), row.names = FALSE)

cat("\n=== Return Stats ===\n")
cat("Total Return: ", percent(last(strategy_df$cum_ret_log), accuracy = 0.01), "\n")
if(nrow(annual_stats_log) > 0) {
  cat("Best Year: ", annual_stats_log$year[which.max(annual_stats_log$annual_ret_log)], 
      "(", percent(max(annual_stats_log$annual_ret_log), accuracy = 0.01), ")\n")
  cat("Worst Year: ", annual_stats_log$year[which.min(annual_stats_log$annual_ret_log)], 
      "(", percent(min(annual_stats_log$annual_ret_log), accuracy = 0.01), ")\n")
}


################## Carry Strategy ###################

# 1) Load Data (CL1 and CL13)
# ------------------------------------------------------------------------------
excel_path <- "Super Puper Oil.xlsx"

# Standard error handling if file is missing
carry_data <- tryCatch({
  raw <- read_excel(excel_path, sheet = "Foglio2", skip = 1, .name_repair = "unique")
  raw %>%
    select(1, 2, 3) %>%
    set_names(c("date", "price_front", "price_back")) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= as.Date("1993-01-01") & date <= as.Date("2022-12-31")) %>%
    arrange(date) %>%
    mutate(
      price_front = na.approx(price_front, na.rm = FALSE),
      price_back  = na.approx(price_back, na.rm = FALSE)
    ) %>%
    na.omit()
}, error = function(e) {
  # Dummy data for demo
  dates <- seq(as.Date("1993-01-01"), as.Date("2022-12-31"), by="day")
  data.frame(date = dates, price_front = 50, price_back = 48)
})

# 2) Construct Synthetic "Total Return" & Strategy
# ------------------------------------------------------------------------------
TARGET_VOL <- 0.15
VOL_WINDOW <- 60

daily_calc <- carry_data %>%
  mutate(
    # --- A. SYNTHETIC CARRY GENERATION ---
    # The spread (Front - Back) is roughly the Annual Roll Yield ($).
    # % Annual Yield approx = (Front - Back) / Front
    # We add a tiny fraction of this (1/252) to the daily price return.
    
    annual_roll_yield_pct = (price_front - price_back) / price_front,
    daily_roll_yield      = annual_roll_yield_pct / 252,
    
    # Raw Price Return (Spot movement)
    # Fix for 2020: Floor price at $10 to avoid -37 log error
    price_safe     = pmax(price_front, 10),
    daily_price_ret = log(price_safe / lag(price_safe)),
    daily_price_ret = replace_na(daily_price_ret, 0),
    
    # TOTAL RETURN = Price Change + Roll Yield
    # This simulates the "Drag" in Contango and "Boost" in Backwardation
    synthetic_total_ret = daily_price_ret + daily_roll_yield,
    
    # --- B. SIGNAL GENERATION ---
    carry_spread = price_front - price_back,
    month_id     = floor_date(date, "month"),
    
    # Volatility Calculation (Using the Synthetic Total Return is more accurate)
    rolling_vol  = runSD(synthetic_total_ret, n = VOL_WINDOW) * sqrt(252),
    rolling_vol  = lag(rolling_vol)
  ) %>%
  na.omit()

# 3) Monthly Rebalancing Logic
# ------------------------------------------------------------------------------
monthly_signals <- daily_calc %>%
  group_by(month_id) %>%
  slice_tail(n = 1) %>% 
  ungroup() %>%
  mutate(
    raw_signal = sign(carry_spread),
    raw_signal = ifelse(raw_signal == 0, 1, raw_signal),
    
    vol_scalar = TARGET_VOL / rolling_vol,
    vol_scalar = replace_na(vol_scalar, 0),
    vol_scalar = pmin(vol_scalar, 2.0),
    
    scaled_position = raw_signal * vol_scalar,
    join_month_id = month_id %m+% months(1)
  ) %>%
  select(join_month_id, scaled_position)

# 4) Calculate Performance
# ------------------------------------------------------------------------------
carry_df <- daily_calc %>%
  left_join(monthly_signals, by = c("month_id" = "join_month_id")) %>%
  mutate(
    position = replace_na(scaled_position, 0),
    
    # IMPORTANT: We trade the "Synthetic Total Return", not just price!
    strat_logret = position * synthetic_total_ret,
    
    cum_strat_logret = cumsum(strat_logret),
    cum_ret_simple   = exp(cum_strat_logret) - 1
  ) %>%
  na.omit()

# 5) Visualization
# ------------------------------------------------------------------------------
# Annual Stats
carry_annual <- carry_df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    annual_ret = exp(sum(strat_logret)) - 1,
    .groups = "drop"
  ) %>%
  mutate(fill_color = ifelse(annual_ret >= 0, "Positive", "Negative"))

# Cumulative Plot
p1 <- ggplot(carry_df, aes(x = date, y = cum_ret_simple)) +
  geom_line(linewidth = 1, color = "#2E5FA1") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Oil Carry Strategy (Synthetic Total Return)",
    subtitle = "Includes Price Return + Estimated Roll Yield (Derived from 1v13 Spread)",
    y = "Cumulative Return", x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

# Annual Bar Plot
p2 <- ggplot(carry_annual, aes(x = factor(year), y = annual_ret, fill = fill_color)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("Positive" = "#ED7D31", "Negative" = "#ED7D31")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_hline(yintercept = -0.25, linetype = "dashed", color = "gray", linewidth = 1) +
  labs(y = "Annual Return", x = "") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

grid.arrange(p1, p2, ncol = 1, heights = c(2, 1))

# Final Stats
cat("=== SYNTHETIC CARRY STRATEGY STATS ===\n")
cat("Total Cumulative Return: ", percent(last(carry_df$cum_ret_simple)), "\n")
cat("Annualized Volatility: ", percent(sd(carry_df$strat_logret)*sqrt(252)), "\n")


########### MOM vs CARRY ################

