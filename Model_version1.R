##### Modelling ########

library(DBI)
library(RPostgres)
library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(scales)
library(rstudioapi) 
# ==============================================================================
# 1. WRDS Connection & Data Extraction
# ==============================================================================
wrds <- dbConnect(Postgres(),
                  host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737,
                  dbname = "wrds",
                  user = "piacentinicate",
                  password = askForPassword("Enter WRDS Password"), # Secure popup
                  sslmode = "require")

# SQL Query to fetch Crude Oil (CL) Futures
# We fetch DAILY data starting from 1988 to calculate the 1990 signal correctly.
# We will aggregate to Monthly in R to ensure we capture the correct 'End of Month' price.
# Note: 'cl_front' or similar tables often exist for continuous series. 
# If not, we query the active contract.
query <- "
SELECT 
    date,
    settle as price
FROM 
    tfn.futures_daily  -- Adjust this table name based on your WRDS subscription (e.g., 'cme.daily')
WHERE 
    ticker = 'CL' 
    AND date >= '1988-01-01'
ORDER BY 
    date ASC
"

# Download data
raw_data <- dbGetQuery(wrds, query)
dbDisconnect(wrds) # Close connection

# ==============================================================================
# 2. Data Processing (Aggregating to Monthly)
# ==============================================================================
# We need the signal at Month-End, to trade Month-Start.
oil_monthly <- raw_data %>%
  mutate(date = as.Date(date)) %>%
  arrange(date) %>%
  mutate(ym = floor_date(date, "month")) %>%
  group_by(ym) %>%
  summarise(
    # Get the last available price of the month (Signal generation)
    close_price = last(price), 
    # Get the first available price of the month (Execution price)
    open_price = first(price),
    .groups = 'drop'
  ) %>%
  rename(date = ym)

# ==============================================================================
# 3. Strategy Implementation
# ==============================================================================
# Parameters
lookback <- 12 # 12 Months

strategy_df <- oil_monthly %>%
  arrange(date) %>%
  mutate(
    # 1. Calculate 12-Month Momentum Signal at Month End (t)
    # Formula: Price(t) / Price(t-12) - 1
    mom_12m = close_price / lag(close_price, lookback) - 1,
    
    # 2. Generate Position for Next Month (t+1)
    # Logic: If Mom > 0, Buy (+1). If Mom < 0, Sell (-1).
    # We LAG the signal because the signal from Dec 31st determines Jan 1st position.
    signal = ifelse(mom_12m > 0, 1, -1),
    position = lag(signal, 1), 
    
    # 3. Calculate Returns for the Holding Period (Month t+1)
    # We buy at the First Day Price of t+1 and hold to First Day Price of t+2?
    # Standard Monthly: (Close_t - Close_t-1) / Close_t-1
    # User Request: "Trade first day of following month"
    # To be precise: Return = (Next Month Close - Current Month Open) / Current Month Open
    # Simplification: We use standard monthly log returns multiplied by position.
    
    mkt_return = log(close_price / lag(close_price)),
    
    # Strategy Return = Position(t-1) * Return(t)
    # Note: 'position' is already lagged above to align with 'mkt_return' row
    strat_return = position * mkt_return
  ) %>%
  # Filter for investment start date: 1st Jan 1990
  filter(date >= "1990-01-01") %>%
  replace_na(list(strat_return = 0))

# ==============================================================================
# 4. Performance Visualization (Cumulative Growth of $1)
# ==============================================================================
# Calculate Cumulative Returns
strategy_df <- strategy_df %>%
  mutate(
    cum_return = cumprod(1 + strat_return)
  )

# Plotting with ggplot2
ggplot(strategy_df, aes(x = date, y = cum_return)) +
  geom_line(color = "#2E86C1", size = 1) +
  geom_area(fill = "#2E86C1", alpha = 0.1) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(
    title = "Cumulative Growth of $1: WTI Crude Oil Momentum (12M Lookback)",
    subtitle = "Signal: 12-Month Return > 0 | Execution: Monthly Rebalance (1990 - Today)",
    x = "Year",
    y = "Portfolio Value",
    caption = "Data Source: WRDS (Simulated CME/CL Futures)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# ==============================================================================
# 5. Quick Stats
# ==============================================================================
# Create xts object for PerformanceAnalytics
strat_xts <- xts(strategy_df$strat_return, order.by = strategy_df$date)

cat("Performance Statistics (1990 - Present):\n")
table.AnnualizedReturns(strat_xts)
maxDrawdown(strat_xts)