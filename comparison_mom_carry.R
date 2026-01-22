# ==============================================================================
# STRATEGY COMPARISON: WTI MOMENTUM VS CARRY (Fixed for NA Data Gaps)
# ==============================================================================

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(gridExtra)
library(zoo)
library(TTR)

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
  # Remove NAs created by Lag/SMA *before* Vol calc to satisfy TTR strictness
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
    # CRITICAL FIX: Remove any rows with missing prices immediately
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
  # CRITICAL FIX: Drop the first NA from lag() BEFORE running runSD
  # runSD fails if there are NAs anywhere in the vector
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
# 3) PERFORMANCE & PLOTTING
# ==============================================================================
strategy_wide <- full_data %>%
  left_join(monthly_rebal, by = c("month_id" = "join_month_id")) %>%
  arrange(date) %>%
  tidyr::fill(pos_momentum, pos_carry, .direction = "down") %>%
  filter(!is.na(pos_momentum), !is.na(pos_carry)) %>%
  mutate(
    ret_mom_strat   = pos_momentum * mom_ret,
    ret_carry_strat = pos_carry * carry_ret,
    
    cum_mom   = exp(cumsum(ret_mom_strat)) - 1,
    cum_carry = exp(cumsum(ret_carry_strat)) - 1
  ) %>%
  select(date, ret_mom = ret_mom_strat, ret_carry = ret_carry_strat, cum_mom, cum_carry)

strategy_long <- strategy_wide %>%
  select(date, cum_mom, cum_carry) %>%
  pivot_longer(cols = c("cum_mom", "cum_carry"), names_to = "Strategy", values_to = "Return")

# PLOTS
cols   <- c("cum_mom" = "#E74C3C", "cum_carry" = "#2E5FA1")
labels <- c("cum_mom" = "Momentum (MA20)", "cum_carry" = "Carry (F1 - F13)")

plot_mom <- ggplot(strategy_wide, aes(x = date, y = cum_mom)) +
  geom_line(linewidth = 0.8, color = cols["cum_mom"]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Momentum Strategy", y = "Cumulative Return", x = "") + theme_minimal()

plot_carry <- ggplot(strategy_wide, aes(x = date, y = cum_carry)) +
  geom_line(linewidth = 0.8, color = cols["cum_carry"]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Carry Strategy (Price Only)", subtitle = "Short Contango / Long Backwardation", y = "Cumulative Return", x = "") + theme_minimal()

final_plot <- ggplot(strategy_long, aes(x = date, y = Return, color = Strategy)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = cols, labels = labels) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(title = "Strategy Comparison: WTI Momentum vs. Carry", y = "Cumulative Return", x = "") +
  theme_minimal() + theme(legend.position = "top", legend.title = element_blank())

gridExtra::grid.arrange(plot_mom, plot_carry, ncol = 1)
print(final_plot)

# STATS
ann_stats <- function(x) {
  mu  <- mean(x, na.rm = TRUE) * 252
  vol <- sd(x, na.rm = TRUE) * sqrt(252)
  sr  <- ifelse(vol > 0, mu / vol, NA_real_)
  c(mu = mu, vol = vol, sharpe = sr)
}
mom_stats   <- ann_stats(strategy_wide$ret_mom)
carry_stats <- ann_stats(strategy_wide$ret_carry)
cor_mc      <- cor(strategy_wide$ret_mom, strategy_wide$ret_carry, use = "complete.obs")

cat("\n=== COMPARISON STATS ===\n")
cat("Correlation: ", round(cor_mc, 3), "\n\n")
cat("Momentum (Original):\n")
cat("  Total Return:", percent(last(strategy_wide$cum_mom)), "\n")
cat("  Sharpe:      ", round(mom_stats["sharpe"], 2), "\n\n")
cat("Carry (New WTI_TS):\n")
cat("  Total Return:", percent(last(strategy_wide$cum_carry)), "\n")
cat("  Sharpe:      ", round(carry_stats["sharpe"], 2), "\n")