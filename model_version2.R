# ============================================================
# WTI 20-day Time-Series Momentum (2000–2022)
# Signal from FRED spot WTI, Trading on Yahoo futures (CL=F)
# ============================================================

library(quantmod)
library(xts)
library(zoo)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)
library(fredr)


# ----------------------------
# 2) Download data
# ----------------------------
# Spot WTI (Cushing) from FRED
fredr_set_key(Sys.getenv("FRED_API_KEY")) # Your Key

message("Downloading Spot Prices (Signal)...")
spot_data <- fredr(series_id = "DCOILWTICO", observation_start = as.Date("2000-01-01")) %>%
  select(date, spot_price = value) %>%
  na.omit()


# ============================================================
# Continue from your spot_data (FRED) -> trade Yahoo CL=F
# Replicate 20-day time-series momentum, 2000–2022
# ============================================================

# ----------------------------
# Parameters
# ----------------------------
from <- as.Date("2000-01-01")
to   <- as.Date("2022-12-31")
n    <- 20
loss_threshold <- -0.25   # dashed reference line like your figure (optional)

# Ensure spot is within window and sorted
spot_data <- spot_data %>%
  filter(date >= from, date <= to) %>%
  arrange(date)

# ----------------------------
# Download WTI futures from Yahoo (continuous) and clean NAs
# ----------------------------
message("Downloading Futures Prices (Trading Asset) from Yahoo...")

fut_xts <- getSymbols("CL=F", src = "yahoo",
                      from = from, to = to, auto.assign = FALSE)

fut_data <- tibble(
  date = as.Date(index(fut_xts)),
  fut_price = as.numeric(Cl(fut_xts))
) %>%
  filter(!is.na(fut_price)) %>%   # <-- your requested na.omit() equivalent
  arrange(date)

# ----------------------------
# Align series: keep futures trading days, carry-forward spot
# ----------------------------
dat <- fut_data %>%
  left_join(spot_data, by = "date") %>%
  arrange(date) %>%
  mutate(
    # carry-forward spot over missing days (holidays / gaps)
    spot_price = na.locf(spot_price, na.rm = FALSE)
  ) %>%
  filter(!is.na(spot_price))

# ----------------------------
# Signal: MA(20) on spot, momentum = spot - MA(20), sign signal
# Trade: use yesterday's signal on today's futures log return
# ----------------------------
dat <- dat %>%
  mutate(
    ma20 = rollmean(spot_price, k = n, align = "right", fill = NA),
    mom  = spot_price - ma20,
    signal = if_else(mom >= 0, 1, -1),
    signal_lag = lag(signal, 1),
    fut_logret = log(fut_price / lag(fut_price)),
    strat_logret = signal_lag * fut_logret
  ) %>%
  filter(!is.na(strat_logret))

# ----------------------------
# Annual + cumulative returns (from log returns)
# ----------------------------
annual <- dat %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    annual_log = sum(strat_logret, na.rm = TRUE),
    annual_ret = exp(annual_log) - 1,
    .groups = "drop"
  ) %>%
  arrange(year) %>%
  mutate(
    cum_log = cumsum(annual_log),
    cum_ret = exp(cum_log) - 1
  )

# ----------------------------
# Plot: bars = annual return, line = cumulative (secondary axis)
# ----------------------------
ann_min <- min(c(annual$annual_ret, annual$cum_ret, loss_threshold), na.rm = TRUE)
ann_max <- max(c(annual$annual_ret, annual$cum_ret, 0), na.rm = TRUE)

cum_min <- min(annual$cum_ret, na.rm = TRUE)
cum_max <- max(annual$cum_ret, na.rm = TRUE)

# Protect against flat series (rare, but avoids division by zero)
scale_fac <- ifelse(cum_max == cum_min, 1, (ann_max - ann_min) / (cum_max - cum_min))
shift_fac <- ann_min - cum_min * scale_fac

p <- ggplot(annual, aes(x = year)) +
  geom_col(aes(y = annual_ret), width = 0.7) +
  geom_hline(yintercept = loss_threshold, linetype = "dashed") +
  
  # nicer cumulative line (rounded ends + joins)
  geom_line(
    aes(y = cum_ret * scale_fac + shift_fac),
    linewidth = 1.6,
    lineend = "round",
    linejoin = "round"
  ) +
  # nicer points (subtle; or delete this geom entirely)
  geom_point(
    aes(y = cum_ret * scale_fac + shift_fac),
    size = 1.8
  ) +
  
  scale_x_continuous(breaks = seq(2000, 2022, by = 2)) +
  scale_y_continuous(
    name = "Annual Return",
    labels = percent_format(accuracy = 1),
    sec.axis = sec_axis(
      trans = ~ (. - shift_fac) / scale_fac,
      name = "Cumulative Return",
      labels = percent_format(accuracy = 1)
    )
  ) +
  labs(
    title = "Oil Momentum",
    subtitle = "20-day time-series momentum (signal: FRED spot WTI, trading: Yahoo CL=F), 2000–2022",
    x = NULL
  ) +
  theme_minimal()

print(p)
