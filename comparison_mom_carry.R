# ==============================================================================
# STRATEGY COMPARISON: MOMENTUM VS CARRY (Vol Scaled)
# ==============================================================================
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(gridExtra)
library(zoo)
library(TTR)

# 1) Load Data & Create "Tradable Asset" (Synthetic Total Return)
# ------------------------------------------------------------------------------
excel_path <- "Super Puper Oil.xlsx"

data_raw <- tryCatch({
  read_excel(excel_path, sheet = "Foglio2", skip = 1, .name_repair = "unique") %>%
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
  # Dummy data for demo if file missing
  dts <- seq(as.Date("1993-01-01"), as.Date("2022-12-31"), by="day")
  data.frame(date = dts, price_front = 50 + cumsum(rnorm(length(dts))), price_back = 48 + cumsum(rnorm(length(dts))))
})

# Parameters
TARGET_VOL <- 0.15
VOL_WINDOW <- 60

# Calculate Indicators & Returns
full_data <- data_raw %>%
  mutate(
    # --- A. SYNTHETIC TOTAL RETURN (The Asset) ---
    # Price Return (Floored at $10 for 2020 safety)
    price_safe = pmax(price_front, 10),
    daily_price_ret = log(price_safe / lag(price_safe)),
    daily_price_ret = replace_na(daily_price_ret, 0),
    
    # Roll Yield Estimate (Spread / Price / 252)
    daily_roll_yield = ((price_front - price_back) / price_front) / 252,
    
    # Total Return to be Traded
    daily_total_ret = daily_price_ret + daily_roll_yield,
    
    # --- B. SIGNALS ---
    # 1. Momentum Signal: Price > MA(20)
    ma_20 = SMA(price_front, n = 20),
    mom_raw = ifelse(price_front >= ma_20, 1, -1),
    
    # 2. Carry Signal: Front > Back (Backwardation)
    carry_spread = price_front - price_back,
    carry_raw = sign(carry_spread),
    carry_raw = ifelse(carry_raw == 0, 1, carry_raw),
    
    # --- C. VOLATILITY SCALAR ---
    # Based on the asset's total return
    rolling_vol = runSD(daily_total_ret, n = VOL_WINDOW) * sqrt(252),
    rolling_vol = lag(rolling_vol), # Lag to avoid look-ahead
    
    month_id = floor_date(date, "month")
  ) %>%
  na.omit()

# 2) Monthly Rebalancing Logic (For BOTH Strategies)
# ------------------------------------------------------------------------------
monthly_rebal <- full_data %>%
  group_by(month_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    # Common Vol Scalar
    vol_scalar = TARGET_VOL / rolling_vol,
    vol_scalar = replace_na(vol_scalar, 0),
    vol_scalar = pmin(vol_scalar, 2.0), # Cap leverage at 2x
    
    # Position Sizing
    pos_momentum = mom_raw * vol_scalar,
    pos_carry    = carry_raw * vol_scalar,
    
    join_month_id = month_id %m+% months(1)
  ) %>%
  select(join_month_id, pos_momentum, pos_carry)

# 3) Merge & Calculate Cumulative Returns
# ------------------------------------------------------------------------------
strategy_comparison <- full_data %>%
  left_join(monthly_rebal, by = c("month_id" = "join_month_id")) %>%
  mutate(
    # Fill NAs
    pos_momentum = replace_na(pos_momentum, 0),
    pos_carry    = replace_na(pos_carry, 0),
    
    # Calculate Strategy Log Returns
    # Note: Both trade the SAME "daily_total_ret" (Price + Roll)
    ret_mom   = pos_momentum * daily_total_ret,
    ret_carry = pos_carry * daily_total_ret,
    
    # Cumulative (Simple % for plotting)
    cum_mom   = exp(cumsum(ret_mom)) - 1,
    cum_carry = exp(cumsum(ret_carry)) - 1
  ) %>%
  select(date, cum_mom, cum_carry) %>%
  # Pivot longer for plotting with ggplot colors
  pivot_longer(cols = c("cum_mom", "cum_carry"), names_to = "Strategy", values_to = "Return")

# 4) Visualization
# ------------------------------------------------------------------------------
# Define custom colors
cols <- c("cum_mom" = "#E74C3C", "cum_carry" = "#2E5FA1") # Red for Mom, Blue for Carry
labels <- c("cum_mom" = "Momentum (MA20)", "cum_carry" = "Carry (1m vs 13m)")

final_plot <- ggplot(strategy_comparison, aes(x = date, y = Return, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = cols, labels = labels) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Strategy Comparison: WTI Momentum vs. Carry",
    subtitle = "Vol Target 15% | Synthetic Total Return (Price + Roll Yield)",
    y = "Cumulative Return",
    x = "",
    caption = "Both strategies trade the Synthetic Total Return index."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12)
  )

print(final_plot)

# 5) Correlations & Stats
# ------------------------------------------------------------------------------
# We need the wide format back for correlation
stats_df <- full_data %>%
  left_join(monthly_rebal, by = c("month_id" = "join_month_id")) %>%
  mutate(
    ret_mom   = replace_na(pos_momentum, 0) * daily_total_ret,
    ret_carry = replace_na(pos_carry, 0) * daily_total_ret
  ) %>%
  na.omit()

cor_matrix <- cor(stats_df$ret_mom, stats_df$ret_carry)

cat("=== COMPARISON STATS ===\n")
cat("Correlation (Mom vs Carry): ", round(cor_matrix, 2), "\n")
cat("Total Return Momentum:      ", percent(last(strategy_comparison$Return[strategy_comparison$Strategy == "cum_mom"])), "\n")
cat("Total Return Carry:         ", percent(last(strategy_comparison$Return[strategy_comparison$Strategy == "cum_carry"])), "\n")

