
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(tidyr)


analysis_start <- as.Date("2006-06-20")

# Load COT data (Disaggregated positions)
cot_raw <- read_xls("Database_COT_86_26.xls", sheet = "Cot_DisAgg_06_26")

# Load Price data (Foglio 2 contains daily WTI prices)
# We skip the first row because it contains metadata ("CL1 Comdty")
price_raw <- read_excel("Super Puper Oil.xlsx", sheet = "Foglio2", skip = 1)
colnames(price_raw)[1:2] <- c("Date", "WTI_Close")

#Cot data
cot_processed <- cot_raw %>%
  mutate(
    # Convert Excel numeric date to R Date class (Origin 1899-12-30)
    Date = as.Date(Report_Date_as_MM_DD_YYYY, origin = "1899-12-30"),
    # Define Net Positions
    Net_HF = M_Money_Positions_Long_ALL - M_Money_Positions_Short_ALL,
    Net_SD = Swap_Positions_Long_All - Swap__Positions_Short_All
  ) %>%
  filter(Date >= analysis_start) %>%
  arrange(Date) %>%
  mutate(
    # Weekly Flows (Change in Net Position)
    Flow_HF = Net_HF - lag(Net_HF),
    Flow_SD = Net_SD - lag(Net_SD)
  )
price_processed <- price_raw %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date >= analysis_start) %>%
  arrange(Date)


merged_data <- inner_join(cot_processed, price_processed, by = "Date") %>%
  mutate(Price_Change = WTI_Close - lag(WTI_Close)) %>%
  filter(!is.na(Price_Change), !is.na(Flow_HF))

#Corr
cor_hf <- cor(merged_data$Flow_HF, merged_data$Price_Change)
cor_sd <- cor(merged_data$Flow_SD, merged_data$Price_Change)

cat("\n--- PITCH ANALYTICS ---\n")
cat("Hedge Fund Flow Correlation (+): ", round(cor_hf, 4), "\n")
cat("Swap Dealer Flow Correlation (-):", round(cor_sd, 4), "\n")

#Visualisation 

# Plot 1: Hedge Fund Momentum Scatter
p1 <- ggplot(merged_data, aes(x = Flow_HF, y = Price_Change)) +
  geom_point(alpha = 0.5, color = "midnightblue") +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Hedge Fund Flow vs WTI Price Change",
       subtitle = paste("Positive Correlation (Momentum):", round(cor_hf, 3)),
       x = "Weekly HF Flow (Contracts)", y = "Weekly Price Change ($)")

# Plot 2: Swap Dealer Hedging Scatter
p2 <- ggplot(merged_data, aes(x = Flow_SD, y = Price_Change)) +
  geom_point(alpha = 0.5, color = "darkred") +
  geom_smooth(method = "lm", color = "black") +
  theme_minimal() +
  labs(title = "Swap Dealer Flow vs WTI Price Change",
       subtitle = paste("Negative Correlation (Hedging):", round(cor_sd, 3)),
       x = "Weekly SD Flow (Contracts)", y = "Weekly Price Change ($)")

# Plot 3: Price vs. Positioning Time Series
# First, normalize positions for better overlay visual (scaling by 10,000s)
p3 <- ggplot(merged_data) +
  geom_line(aes(x = Date, y = WTI_Close), color = "black", size = 1) +
  geom_area(aes(x = Date, y = Net_HF / 5000 + 40), fill = "blue", alpha = 0.2) + # Scaled for visibility
  geom_area(aes(x = Date, y = Net_SD / 5000 + 40), fill = "red", alpha = 0.2) +
  theme_minimal() +
  labs(title = "WTI Price Overlay with Institutional Positioning",
       subtitle = "Black: Price | Blue Shade: HF Net Position | Red Shade: SD Net Position",
       x = "Date", y = "WTI Price ($)")

#Graph on net position and price 
print(p1)
print(p2)
print(p3)

z_thresh <- 1.5


trading_system <- merged_data %>%
  mutate(
    # Calculate Z-scores for Momentum (Flows)
    HF_Z = (Flow_HF - mean(Flow_HF, na.rm=T)) / sd(Flow_HF, na.rm=T),
    SD_Z = (Flow_SD - mean(Flow_SD, na.rm=T)) / sd(Flow_SD, na.rm=T),
    
    # HF ANOMALY LOGIC: Extreme activity (Long or Short accumulation) -> GO LONG
    sig_hf = ifelse(abs(HF_Z) > z_thresh, 1, 0),
    
    # SD ANOMALY LOGIC: Fade the Swap Dealer momentum
    sig_sd = case_when(
      SD_Z > z_thresh  ~ -1, # Extreme buying by SD -> Enter SHORT
      SD_Z < -z_thresh ~ 1,  # Extreme selling by SD -> Enter LONG
      TRUE ~ 0
    ),
    
    
    # Signal Logic 
    sig_hf = ifelse(abs(HF_Z) > z_thresh, 1, 0),
    sig_sd = case_when(SD_Z > z_thresh ~ -1, SD_Z < -z_thresh ~ 1, TRUE ~ 0),
    final_signal = pmax(pmin(sig_hf + sig_sd, 1), -1),
    
    # RETURNS CALCULATION
    # Signal at end of Tuesday applies to price move until next Tuesday
    Price_Return = (WTI_Close / lag(WTI_Close)) - 1,
    strat_ret = final_signal * lead(Price_Return),
    cum_strat = cumprod(1 + replace_na(strat_ret, 0)),
    cum_bench = cumprod(1 + replace_na(Price_Return, 0))
  )
#1. Total count of trade entries
total_positions <- sum(df_strat$final_signal != lag(df_strat$final_signal, default = 0) & df_strat$final_signal != 0, na.rm = TRUE)

# 2. Annualized Frequency (360-day basis)
sample_days <- as.numeric(difftime(max(df_strat$Date), min(df_strat$Date), units = "days"))
trading_freq_360 <- (total_positions / sample_days) * 360

cat("\n--- TRADING SYSTEM STATS ---\n")
cat("Total Amount of Positions:    ", total_positions, "\n")
cat("Frequency (Trades/360 days):  ", round(trading_freq_360, 2), "\n")
cat("----------------------------\n")


# --- 3. VISUALIZATION: HF SHORT FLOWS ---
# Re-extracting Short positions from your original 'cot_raw' if needed, 
# or assuming they were kept in merged_data
p_hf_shorts <- ggplot(merged_data, aes(x = Date)) +
  geom_area(aes(y = M_Money_Positions_Short_ALL, fill = "HF Gross Short Position"), alpha = 0.4) +
  geom_line(aes(y = abs(Flow_HF), color = "HF Net Flow Magnitude"), size = 0.5) +
  scale_fill_manual(values = c("HF Gross Short Position" = "red")) +
  scale_color_manual(values = c("HF Net Flow Magnitude" = "black")) +
  theme_minimal() +
  labs(title = "Hedge Fund Short Exposure & Flow Magnitude",
       subtitle = "Visualizing the 'Anomalies' in the speculative short book",
       y = "Contracts", fill = "", color = "")

# --- 4. VISUALIZATION: EQUITY CURVE ---
p_equity <- ggplot(trading_system, aes(x = Date)) +
  geom_line(aes(y = cum_strat, color = "Z-Score Strategy"), size = 1) +
  geom_line(aes(y = cum_bench, color = "WTI Buy & Hold"), linetype = "dashed") +
  scale_color_manual(values = c("Z-Score Strategy" = "blue", "WTI Buy & Hold" = "gray")) +
  theme_minimal() +
  labs(title = "Momentum System Performance: HF/SD Anomalies",
       subtitle = paste("Start Date: 2006-06-20 | Threshold:", z_thresh, "Sigma"),
       y = "Cumulative Growth (Base 1.0)", x = "")

# --- 5. PERFORMANCE OUTPUT ---
sharpe <- (mean(trading_system$strat_ret, na.rm=T) / sd(trading_system$strat_ret, na.rm=T)) * sqrt(52)
cat("\n--- TRADING SYSTEM STATS ---\n")
cat("Strategy Sharpe Ratio:", round(sharpe, 2), "\n")
cat("Total Strategy Return:", round((tail(trading_system$cum_strat, 1) - 1) * 100, 2), "%\n")


# Display Plots
print(p_hf_shorts)
print(p_equity)

