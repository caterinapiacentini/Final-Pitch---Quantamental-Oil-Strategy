library(fredr)
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(TTR)
library(scales)
library(gridExtra)

# ==============================================================================
# 1. Download Data Sources
# ==============================================================================
# A. Signal Data: FRED WTI Spot
fredr_set_key("6aed54afc7147969fa90cc56b29e9127") # Your Key
message("Downloading Spot Prices (Signal)...")
spot_data <- fredr(series_id = "DCOILWTICO", observation_start = as.Date("2000-01-01")) %>%
  select(date, spot_price = value) %>%
  na.omit()

# B. Execution Data: Yahoo Futures (CL=F)
message("Downloading Futures Prices (Execution)...")
getSymbols("CL=F", src = "yahoo", auto.assign = TRUE, from = "2000-01-01")
futures_data <- data.frame(date = index(`CL=F`), fut_price = as.numeric(Cl(`CL=F`))) %>%
  na.omit() %>%
  filter(fut_price > 0.01) # Filter negative oil days for stability

# ==============================================================================
# 2. Merge & Align
# ==============================================================================
# Inner Join ensures we only trade days where BOTH prices exist
combined_data <- inner_join(spot_data, futures_data, by = "date") %>%
  arrange(date)

# ==============================================================================
# 3. Strategy Logic: Hybrid 20-Day Momentum
# ==============================================================================
strategy_df <- combined_data %>%
  mutate(
    # --- A. Signal Generation (Using SPOT Price) ---
    # We smooth the physical spot price to find the trend
    spot_ma_20 = SMA(spot_price, n = 20),
    
    # Signal: +1 if Spot > Spot_MA, -1 if Spot < Spot_MA
    # This avoids "Fake Jumps" in futures data triggering false signals.
    raw_signal = ifelse(spot_price >= spot_ma_20, 1, -1),
    
    # --- B. Execution (Trading FUTURES) ---
    # Signal(t-1) determines Position(t)
    position = lag(raw_signal, 1),
    
    # --- C. P&L Calculation (Using FUTURES Price) ---
    # We hold the Future, so our account value changes based on Futures Price.
    # Dollar P&L = Position * (Future_t - Future_t-1)
    fut_price_change = fut_price - lag(fut_price),
    dollar_pnl = position * fut_price_change
  ) %>%
  na.omit() %>%
  filter(date <= "2022-12-31") # Match your previous timeframe

# ==============================================================================
# 4. Visualization
# ==============================================================================
strategy_df <- strategy_df %>%
  mutate(cum_pnl = cumsum(dollar_pnl))

plot_cum <- ggplot(strategy_df, aes(x = date, y = cum_pnl)) +
  geom_line(color = "#2c3e50", linewidth = 0.8) +
  geom_area(fill = "#2c3e50", alpha = 0.1) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    title = "Hybrid WTI Momentum Strategy (2000-2022)",
    subtitle = "Top: Cumulative Dollar Profit | Bottom: Annual Profit per Barrel",
    y = "Cumulative P&L ($)",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

annual_stats <- strategy_df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    total_pnl = sum(dollar_pnl, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    fill_color = ifelse(total_pnl >= 0, "Positive", "Negative")
  )

# Plot B: Annual Returns (Bar)
plot_annual <- ggplot(annual_stats, aes(x = factor(year), y = total_pnl, fill = fill_color)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("Positive" = "#27AE60", "Negative" = "#C0392B")) +
  scale_y_continuous(labels = dollar_format()) +
  labs(
    y = "Annual P&L ($)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate years for readability
  )

# Combine using gridExtra (Stacked Layout)
grid.arrange(plot_cum, plot_annual, ncol = 1, heights = c(2, 1))

# ==============================================================================
# 5. Quick Stats Output
# ==============================================================================
cat("Total Profit per Barrel: ", dollar(sum(strategy_df$dollar_pnl)), "\n")
cat("Best Year: ", annual_stats$year[which.max(annual_stats$total_pnl)], 
    "(", dollar(max(annual_stats$total_pnl)), ")\n")
cat("Worst Year: ", annual_stats$year[which.min(annual_stats$total_pnl)], 
    "(", dollar(min(annual_stats$total_pnl)), ")\n")

# ==============================================================================
# 6. Print Cumulative Performance Table (End-of-Year)
# ==============================================================================

# Calculate End-of-Year Cumulative P&L for display
eoy_table <- strategy_df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    Annual_PnL = sum(dollar_pnl),
    Cumulative_PnL = last(cum_pnl), # The value of the line chart at Dec 31
    .groups = 'drop'
  ) %>%
  mutate(
    Annual_PnL = dollar(Annual_PnL),
    Cumulative_PnL = dollar(Cumulative_PnL)
  )

# Print the table nicely
print(as.data.frame(eoy_table), row.names = FALSE)
