# ==============================================================================
# STRATEGY COMPARISON: WTI MOMENTUM VS CARRY (Paper-style carry accrual)
#
# Data:
# - Foglio1: CL1 (Date, Close)
# - Foglio2: CL13 (Date, Close)
# - Match dates; interpolate missing values
#
# Strategy:
# - Momentum: price log returns only (MA20 signal)
# - Carry:
#   signal = sign(C_t(1,13)) where C_t(1,13) = F1 - F13 (month-end, applied next month)
#   synthetic TR = price log return + (month-end carry_annual_yield)/252 accrued next month
#
# Vol scaling:
# - Common vol scaler based on price returns (consistent across strategies)
# - No leverage: cap scalar at 1.0
# ==============================================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(gridExtra)
library(zoo)
library(TTR)

# -----------------------------
# Inputs
# -----------------------------
excel_path <- "Super Puper Oil.xlsx"   # put file in working directory or use full path

SHEET_CL1  <- "Foglio1"               # change to "Foglio 1" if needed
SHEET_CL13 <- "Foglio2"               # change to "Foglio 2" if needed

START_DATE <- as.Date("1993-01-01")
END_DATE   <- as.Date("2024-12-31")

TARGET_VOL <- 0.15
VOL_WINDOW <- 60
MA_WINDOW  <- 20
LEV_CAP    <- 1.0     # no leverage

# -----------------------------
# Helper: robust Excel date parsing
# -----------------------------
parse_excel_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  suppressWarnings(as.Date(as.character(x)))
}

# ==============================================================================
# 1) Read CL1 (Foglio1) and CL13 (Foglio2)
#    Wrapped in tryCatch so script runs even if the file isn't present.
# ==============================================================================
tryCatch({
  
  # --- CL1 ---
  raw_cl1 <- read_excel(excel_path, sheet = SHEET_CL1, skip = 1, .name_repair = "unique") %>%
    select(Date, Close) %>%
    rename(date_raw = Date, price_front = Close) %>%
    mutate(date = parse_excel_date(date_raw)) %>%
    select(date, price_front) %>%
    filter(!is.na(date)) %>%
    filter(date >= START_DATE & date <= END_DATE)
  
  # --- CL13 ---
  raw_cl13 <- read_excel(excel_path, sheet = SHEET_CL13, skip = 1, .name_repair = "unique") %>%
    select(Date, Close) %>%
    rename(date_raw = Date, price_back = Close) %>%
    mutate(date = parse_excel_date(date_raw)) %>%
    select(date, price_back) %>%
    filter(!is.na(date)) %>%
    filter(date >= START_DATE & date <= END_DATE)
  
  # --- Match dates + interpolate missing ---
  data_raw <- full_join(raw_cl1, raw_cl13, by = "date") %>%
    arrange(date) %>%
    mutate(
      price_front = na.approx(price_front, x = date, na.rm = FALSE),
      price_back  = na.approx(price_back,  x = date, na.rm = FALSE)
    ) %>%
    na.omit()
  
}, error = function(e) {
  message("Error reading file/sheets. Ensure 'Super Puper Oil.xlsx' exists and sheet names match. Using dummy data for demo.")
  set.seed(1)
  dates <- seq(as.Date("2000-01-01"), as.Date("2022-12-31"), by = "day")
  price_front <- cumprod(c(20, 1 + rnorm(length(dates) - 1, 0, 0.02)))
  price_back  <- price_front * (1 + rnorm(length(dates), 0, 0.01))
  data_raw <- data.frame(date = dates, price_front = price_front, price_back = price_back) %>%
    filter(date >= START_DATE & date <= END_DATE)
})

# ==============================================================================
# 2) Build returns + signals + COMMON rolling vol
# ==============================================================================
full_data <- data_raw %>%
  arrange(date) %>%
  mutate(
    # PRICE RETURN (used for momentum PnL and common vol scaling)
    price_safe      = pmax(price_front, 10),
    daily_price_ret = log(price_safe / lag(price_safe)),
    
    # Momentum signal (daily, sampled at month-end)
    ma_20   = SMA(price_front, n = MA_WINDOW),
    mom_raw = ifelse(price_front >= ma_20, 1, -1),
    
    # Carry signal (daily, sampled at month-end): sign(C_t(1,13)) with C = F1 - F13
    carry_spread = price_front - price_back,
    carry_raw    = sign(carry_spread),
    carry_raw    = ifelse(carry_raw == 0, 1, carry_raw),
    
    # COMMON rolling vol based on price returns
    rolling_vol_common = runSD(daily_price_ret, n = VOL_WINDOW) * sqrt(252),
    rolling_vol_common = lag(rolling_vol_common),
    
    month_id = floor_date(date, "month")
  )

# ==============================================================================
# 3) Monthly rebalancing at month-end, applied next month
#    - Carry annual yield locked at month-end, accrued daily next month
#    - Common vol scalar capped at 1.0 (no leverage)
# ==============================================================================
monthly_rebal <- full_data %>%
  group_by(month_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    carry_annual_yield = (price_front - price_back) / price_front,  # ((F1 - F13)/F1)
    
    vol_scalar = pmin(TARGET_VOL / rolling_vol_common, LEV_CAP),
    
    pos_momentum = mom_raw   * vol_scalar,
    pos_carry    = carry_raw * vol_scalar,
    
    join_month_id = month_id %m+% months(1)
  ) %>%
  filter(
    is.finite(pos_momentum),
    is.finite(pos_carry),
    is.finite(carry_annual_yield)
  ) %>%
  select(join_month_id, pos_momentum, pos_carry, carry_annual_yield)

# ==============================================================================
# 4) Merge positions onto daily data; compute strategy returns and cumulative PnL
# ==============================================================================
strategy_wide <- full_data %>%
  left_join(monthly_rebal, by = c("month_id" = "join_month_id")) %>%
  arrange(date) %>%
  tidyr::fill(pos_momentum, pos_carry, carry_annual_yield, .direction = "down") %>%
  filter(!is.na(pos_momentum), !is.na(pos_carry), !is.na(carry_annual_yield), !is.na(daily_price_ret)) %>%
  mutate(
    # carry accrual fixed within month (set at prior month-end)
    daily_roll_yield    = carry_annual_yield / 252,
    synthetic_total_ret = daily_price_ret + daily_roll_yield,
    
    ret_mom   = pos_momentum * daily_price_ret,
    ret_carry = pos_carry    * synthetic_total_ret,
    
    cum_mom   = exp(cumsum(ret_mom)) - 1,
    cum_carry = exp(cumsum(ret_carry)) - 1
  ) %>%
  select(date, ret_mom, ret_carry, cum_mom, cum_carry)

strategy_long <- strategy_wide %>%
  select(date, cum_mom, cum_carry) %>%
  pivot_longer(cols = c("cum_mom", "cum_carry"),
               names_to = "Strategy",
               values_to = "Return")

# ==============================================================================
# 5) Plots
# ==============================================================================
cols   <- c("cum_mom" = "#E74C3C", "cum_carry" = "#2E5FA1")
labels <- c("cum_mom" = "Momentum (MA20) — Price only",
            "cum_carry" = "Carry C(1,13) — Price + Roll (accrued)")

plot_mom <- ggplot(strategy_wide, aes(x = date, y = cum_mom)) +
  geom_line(linewidth = 0.8, color = cols["cum_mom"]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "WTI Momentum Strategy",
    subtitle = "Vol Target 15% (no leverage) | Trades price log returns only",
    y = "Cumulative Return", x = "",
    caption = "Signal: CL1 >= SMA(20). Common vol scaler from price returns, capped at 1x."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "none")

plot_carry <- ggplot(strategy_wide, aes(x = date, y = cum_carry)) +
  geom_line(linewidth = 0.8, color = cols["cum_carry"]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "WTI Carry Strategy (Synthetic Total Return)",
    subtitle = "Vol Target 15% (no leverage) | Price log returns + month-end carry accrual",
    y = "Cumulative Return", x = "",
    caption = "Carry: sign(CL1 - CL13). Roll accrual: ((CL1-CL13)/CL1)/252 locked at month-end, applied next month."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "none")

final_plot <- ggplot(strategy_long, aes(x = date, y = Return, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = cols, labels = labels) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Strategy Comparison: WTI Momentum vs. Carry",
    subtitle = "Vol Target 15% (no leverage) | Momentum: price only; Carry: price + roll accrual",
    y = "Cumulative Return", x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 11))

print(plot_mom)
print(plot_carry)
gridExtra::grid.arrange(plot_mom, plot_carry, ncol = 1)
print(final_plot)

# ==============================================================================
# 6) Stats
# ==============================================================================
ann_stats <- function(x) {
  mu  <- mean(x, na.rm = TRUE) * 252
  vol <- sd(x, na.rm = TRUE) * sqrt(252)
  sr  <- ifelse(vol > 0, mu / vol, NA_real_)
  c(mu = mu, vol = vol, sharpe = sr)
}

mom_stats   <- ann_stats(strategy_wide$ret_mom)
carry_stats <- ann_stats(strategy_wide$ret_carry)

cor_mom_carry <- cor(strategy_wide$ret_mom, strategy_wide$ret_carry, use = "complete.obs")

cat("=== COMPARISON STATS ===\n")
cat("Correlation (Mom vs Carry): ", round(cor_mom_carry, 2), "\n\n")

cat("Momentum (price only):\n")
cat("  Total Return:   ", percent(last(strategy_wide$cum_mom)), "\n")
cat("  Ann. Return:    ", percent(mom_stats["mu"]), "\n")
cat("  Ann. Vol:       ", percent(mom_stats["vol"]), "\n")
cat("  Sharpe:         ", round(mom_stats["sharpe"], 2), "\n\n")

cat("Carry (price + roll accrual):\n")
cat("  Total Return:   ", percent(last(strategy_wide$cum_carry)), "\n")
cat("  Ann. Return:    ", percent(carry_stats["mu"]), "\n")
cat("  Ann. Vol:       ", percent(carry_stats["vol"]), "\n")
cat("  Sharpe:         ", round(carry_stats["sharpe"], 2), "\n")


