# ==============================================================================
# STRATEGY COMPARISON: MOMENTUM VS CARRY (Vol Scaled)
#   Data columns (Bloomberg export): Date | CL1 | CL13 Comdty
#
# UPDATE REQUEST:
# - Carry should trade a Synthetic Total Return = price log return + roll yield
#   where roll yield = ((CL1 - CL13)/CL1)/252 (as in your carry-only code).
# - Momentum should NOT be changed (i.e., momentum trades price returns only).
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
excel_path <- "~/Desktop/extracurricular/wutis/Final Pitch/Final Pitch/brent and wti data.xlsx"
sheet_name <- "WTI"

START_DATE <- as.Date("1993-01-01")
END_DATE   <- as.Date("2024-12-31")

TARGET_VOL <- 0.15
VOL_WINDOW <- 60
MA_WINDOW  <- 20
LEV_CAP    <- 2.0

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
# 1) Load Data (Date, CL1, CL13 Comdty)
# ==============================================================================
data_raw <- read_excel(excel_path, sheet = sheet_name, .name_repair = "unique") %>%
  select(any_of(c("Date", "CL1", "CL13 Comdty"))) %>%
  rename(date_raw = Date, price_front = CL1, price_back = `CL13 Comdty`) %>%
  mutate(date = parse_excel_date(date_raw)) %>%
  select(date, price_front, price_back) %>%
  filter(!is.na(date)) %>%
  filter(date >= START_DATE & date <= END_DATE) %>%
  arrange(date) %>%
  mutate(
    price_front = na.approx(price_front, na.rm = FALSE),
    price_back  = na.approx(price_back,  na.rm = FALSE)
  ) %>%
  na.omit()

# ==============================================================================
# 2) Build returns + signals + rolling vols
#    Momentum: trades PRICE return only (do not add roll yield)
#    Carry: trades SYNTHETIC total return (price + roll yield)
# ==============================================================================
full_data <- data_raw %>%
  mutate(
    # --- A) PRICE RETURN (used by MOMENTUM) ---
    price_safe       = pmax(price_front, 10),
    daily_price_ret  = log(price_safe / lag(price_safe)),
    daily_price_ret  = replace_na(daily_price_ret, 0),
    
    # --- B) ROLL YIELD (used ONLY by CARRY) ---
    annual_roll_yield_pct = (price_front - price_back) / price_front,
    daily_roll_yield      = annual_roll_yield_pct / 252,
    
    # Synthetic total return for carry (your carry-only code logic)
    synthetic_total_ret   = daily_price_ret + daily_roll_yield,
    
    # --- C) SIGNALS ---
    ma_20   = SMA(price_front, n = MA_WINDOW),
    mom_raw = ifelse(price_front >= ma_20, 1, -1),
    
    carry_spread = price_front - price_back,
    carry_raw    = sign(carry_spread),
    carry_raw    = ifelse(carry_raw == 0, 1, carry_raw),
    
    # --- D) ROLLING VOLS (separate, so we don't alter momentum logic) ---
    rolling_vol_mom   = runSD(daily_price_ret,      n = VOL_WINDOW) * sqrt(252),
    rolling_vol_mom   = lag(rolling_vol_mom),
    
    rolling_vol_carry = runSD(synthetic_total_ret,  n = VOL_WINDOW) * sqrt(252),
    rolling_vol_carry = lag(rolling_vol_carry),
    
    month_id = floor_date(date, "month")
  ) %>%
  na.omit()

# ==============================================================================
# 3) Monthly rebalancing (positions set at month-end, applied next month)
#    Momentum uses its own rolling vol; Carry uses its own rolling vol.
# ==============================================================================
monthly_rebal <- full_data %>%
  group_by(month_id) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    # Vol scalars (separate)
    vol_scalar_mom = TARGET_VOL / rolling_vol_mom,
    vol_scalar_mom = replace_na(vol_scalar_mom, 0),
    vol_scalar_mom = pmin(vol_scalar_mom, LEV_CAP),
    
    vol_scalar_carry = TARGET_VOL / rolling_vol_carry,
    vol_scalar_carry = replace_na(vol_scalar_carry, 0),
    vol_scalar_carry = pmin(vol_scalar_carry, LEV_CAP),
    
    # Positions
    pos_momentum = mom_raw   * vol_scalar_mom,
    pos_carry    = carry_raw * vol_scalar_carry,
    
    join_month_id = month_id %m+% months(1)
  ) %>%
  select(join_month_id, pos_momentum, pos_carry)

# ==============================================================================
# 4) Merge positions onto daily data; compute strategy returns and cumulative PnL
#    Momentum trades DAILY_PRICE_RET only.
#    Carry trades SYNTHETIC_TOTAL_RET (price + roll yield).
# ==============================================================================
strategy_wide <- full_data %>%
  left_join(monthly_rebal, by = c("month_id" = "join_month_id")) %>%
  mutate(
    pos_momentum = replace_na(pos_momentum, 0),
    pos_carry    = replace_na(pos_carry, 0),
    
    ret_mom      = pos_momentum * daily_price_ret,
    ret_carry    = pos_carry    * synthetic_total_ret,
    
    cum_mom      = exp(cumsum(ret_mom)) - 1,
    cum_carry    = exp(cumsum(ret_carry)) - 1
  ) %>%
  select(date, ret_mom, ret_carry, cum_mom, cum_carry)

strategy_long <- strategy_wide %>%
  select(date, cum_mom, cum_carry) %>%
  pivot_longer(cols = c("cum_mom", "cum_carry"),
               names_to = "Strategy",
               values_to = "Return")

# ==============================================================================
# 5) Plots (Momentum alone, Carry alone, then combined)
# ==============================================================================
cols   <- c("cum_mom" = "#E74C3C", "cum_carry" = "#2E5FA1")
labels <- c("cum_mom" = "Momentum (MA20) — Price only",
            "cum_carry" = "Carry (CL1 vs CL13) — Price + Roll")

plot_mom <- ggplot(strategy_wide, aes(x = date, y = cum_mom)) +
  geom_line(linewidth = 0.8, color = cols["cum_mom"]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "WTI Momentum Strategy",
    subtitle = "Vol Target 15% | Trades price log returns (no roll yield)",
    y = "Cumulative Return", x = "",
    caption = "Signal: CL1 >= SMA(20). Position scaled by rolling vol of price returns."
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        legend.position = "none")

plot_carry <- ggplot(strategy_wide, aes(x = date, y = cum_carry)) +
  geom_line(linewidth = 0.8, color = cols["cum_carry"]) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "WTI Carry Strategy (Synthetic Total Return)",
    subtitle = "Vol Target 15% | Trades price log returns + estimated roll yield",
    y = "Cumulative Return", x = "",
    caption = "Roll yield: ((CL1 - CL13)/CL1)/252. Signal: sign(CL1 - CL13)."
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
    subtitle = "Vol Target 15% | Momentum trades price only; Carry trades price + roll yield",
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

cat("Carry (price + roll):\n")
cat("  Total Return:   ", percent(last(strategy_wide$cum_carry)), "\n")
cat("  Ann. Return:    ", percent(carry_stats["mu"]), "\n")
cat("  Ann. Vol:       ", percent(carry_stats["vol"]), "\n")
cat("  Sharpe:         ", round(carry_stats["sharpe"], 2), "\n")
