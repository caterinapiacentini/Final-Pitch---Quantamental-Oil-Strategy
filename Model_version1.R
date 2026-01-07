########## Model using WTI data from Bloomberg

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
excel_path <- "Super Puper Oil.xlsx"
Super_Puper_Oil <- read_excel("Super Puper Oil.xlsx", sheet = "Foglio1", skip = 1)
# make the date in date format
futures_data <- Super_Puper_Oil %>%
  select(Date, Close) %>%
  rename(date = Date, fut_price = Close) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date("1993-01-01") & date <= as.Date("2022-12-31"))

# do linear interpolation to avoid NAs using zoo library
futures_data <- futures_data %>%
  arrange(date) %>%
  mutate(fut_price = na.approx(fut_price, na.rm = FALSE)) %>%
  na.omit()


# ==============================================================================
# 2) Strategy: 20-day MA Momentum on FUTURES (signal + execution both on CL1)
#    IMPORTANT: position at t uses signal computed at t-1 (lag) to avoid look-ahead.
# ==============================================================================
strategy_df <- futures_data %>%
  mutate(
    ma_20 = SMA(fut_price, n = 20),
    # Signal based on spread vs MA
    raw_signal = ifelse(fut_price - ma_20 >= 0, 1, -1),
    position = lag(raw_signal, 1),
    
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
    title = "WTI Futures Momentum Strategy (CL1, 2000–2022)",
    subtitle = "Signal: sign(Ft − MA20(Ft)) | P&L uses signal(t−1)",
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
cat("Total Profit per Barrel: ", dollar(sum(strategy_df$dollar_pnl)), "\n")
cat("Best Year: ", annual_stats$year[which.max(annual_stats$total_pnl)],
    "(", dollar(max(annual_stats$total_pnl)), ")\n")
cat("Worst Year: ", annual_stats$year[which.min(annual_stats$total_pnl)],
    "(", dollar(min(annual_stats$total_pnl)), ")\n")

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

################## Visualisation in returns ##########################
strategy_df <- strategy_df %>%
  mutate(
    fut_logret      = log(fut_price / lag(fut_price)),
    strat_logret    = position * fut_logret,
    cum_strat_logret = cumsum(replace_na(strat_logret, 0)),
    cum_ret_log     = exp(cum_strat_logret) - 1
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
    title = "WTI Futures Momentum Strategy (Log-return view, CL1, 2000–2022)",
    subtitle = "Signal uses t−1 | cum = exp(cumsum(strategy log returns)) − 1",
    y = "Cumulative return",
    x = ""
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))

plot_annual_log <- ggplot(annual_stats_log, aes(x = factor(year), y = annual_ret_log, fill = fill_color)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(y = "Annual return", x = "") +
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
cat("Total Return: ", percent(last(strategy_df$cum_ret_log), accuracy = 0.01), "\n")
cat("Best Year: ", annual_stats_log$year[which.max(annual_stats_log$annual_ret_log)],
    "(", percent(max(annual_stats_log$annual_ret_log), accuracy = 0.01), ")\n")
cat("Worst Year: ", annual_stats_log$year[which.min(annual_stats_log$annual_ret_log)],
    "(", percent(min(annual_stats_log$annual_ret_log), accuracy = 0.01), ")\n")
# End of Model_version1.R


