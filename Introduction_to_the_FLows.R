library(tidyverse)
library(readxl)



# Load price data
price_raw <- read_excel("Super Puper Oil.xlsx", sheet = "Foglio1", skip = 1)

# Load COT data
cot_raw <- read_excel("Database_COT_86_26.xls", sheet = "Cot_DisAgg_06_26")

# Show structure
cat("COT DATA STRUCTURE:\n")
cat("Columns:", ncol(cot_raw), "\n")
cat("Column names:\n")
print(names(cot_raw))
cat("\nFirst 5 rows:\n")
print(head(cot_raw, 5))

cat("\n\nPRICE DATA STRUCTURE:\n")
cat("Columns:", ncol(price_raw), "\n")
cat("Column names:\n")
print(names(price_raw))
cat("\nFirst 5 rows:\n")
print(head(price_raw, 5))

# 2. CLEAN PRICE DATA
cat("\n", rep("=", 60), "\n", sep = "")
cat("CLEANING PRICE DATA\n")
cat(rep("=", 60), "\n", sep = "")

price_clean <- price_raw %>%
  rename(date = 1, price = 2) %>%
  mutate(
    date = as.Date(date),
    price = as.numeric(price)
  ) %>%
  filter(!is.na(date), !is.na(price)) %>%
  arrange(date)

cat("Price observations:", nrow(price_clean), "\n")
cat("Date range:", as.character(min(price_clean$date)), "to", 
    as.character(max(price_clean$date)), "\n")

# 3. CLEAN COT DATA
cat("\n", rep("=", 60), "\n", sep = "")
cat("CLEANING COT DATA\n")
cat(rep("=", 60), "\n", sep = "")

# Get column names to identify positions
col_names <- names(cot_raw)
cat("\nIdentifying columns...\n")

# Find date column (usually first)
date_col <- 1

# Find position columns by exact name patterns
oi_col <- grep("Open_Interest_All", col_names, ignore.case = TRUE)[1]
prod_long_col <- grep("Prod_Merc_Positions_Long_ALL", col_names, ignore.case = TRUE)[1]
prod_short_col <- grep("Prod_Merc_Positions_Short_ALL", col_names, ignore.case = TRUE)[1]
swap_long_col <- grep("Swap_Positions_Long_All", col_names, ignore.case = TRUE)[1]
swap_short_col <- grep("Swap__Positions_Short_All", col_names, ignore.case = TRUE)[1]
mm_long_col <- grep("M_Money_Positions_Long_ALL", col_names, ignore.case = TRUE)[1]
mm_short_col <- grep("M_Money_Positions_Short_ALL", col_names, ignore.case = TRUE)[1]
other_long_col <- grep("Other_Rept_Positions_Long_ALL", col_names, ignore.case = TRUE)[1]
other_short_col <- grep("Other_Rept_Positions_Short_ALL", col_names, ignore.case = TRUE)[1]

cat("Found columns:\n")
cat("  Date:", date_col, "-", col_names[date_col], "\n")
cat("  Open Interest:", oi_col, "-", col_names[oi_col], "\n")
cat("  Producer Long:", prod_long_col, "-", col_names[prod_long_col], "\n")
cat("  Producer Short:", prod_short_col, "-", col_names[prod_short_col], "\n")
cat("  Swap Long:", swap_long_col, "-", col_names[swap_long_col], "\n")
cat("  Swap Short:", swap_short_col, "-", col_names[swap_short_col], "\n")
cat("  MM Long:", mm_long_col, "-", col_names[mm_long_col], "\n")
cat("  MM Short:", mm_short_col, "-", col_names[mm_short_col], "\n")
cat("  Other Long:", other_long_col, "-", col_names[other_long_col], "\n")
cat("  Other Short:", other_short_col, "-", col_names[other_short_col], "\n")

# Clean COT data
cot_clean <- cot_raw %>%
  mutate(
    date = as.Date(.[[date_col]]),
    oi = as.numeric(.[[oi_col]]),
    
    # Producers
    prod_long = as.numeric(.[[prod_long_col]]),
    prod_short = as.numeric(.[[prod_short_col]]),
    prod_net = prod_long - prod_short,
    
    # Swap Dealers
    swap_long = as.numeric(.[[swap_long_col]]),
    swap_short = as.numeric(.[[swap_short_col]]),
    swap_net = swap_long - swap_short,
    
    # Managed Money
    mm_long = as.numeric(.[[mm_long_col]]),
    mm_short = as.numeric(.[[mm_short_col]]),
    mm_net = mm_long - mm_short,
    
    # Other Reportables
    other_long = as.numeric(.[[other_long_col]]),
    other_short = as.numeric(.[[other_short_col]]),
    other_net = other_long - other_short
  ) %>%
  filter(!is.na(date), !is.na(oi), oi > 0) %>%
  mutate(
    # Calculate percentages
    prod_pct = prod_net / oi,
    swap_pct = swap_net / oi,
    mm_pct = mm_net / oi,
    other_pct = other_net / oi
  ) %>%
  select(date, prod_pct, swap_pct, mm_pct, other_pct, oi) %>%
  arrange(date)

cat("\nCOT observations:", nrow(cot_clean), "\n")
cat("Date range:", as.character(min(cot_clean$date)), "to", 
    as.character(max(cot_clean$date)), "\n")

# 4. FIND COMMON DATE RANGE
cat("\n", rep("=", 60), "\n", sep = "")
cat("FINDING COMMON DATE RANGE\n")
cat(rep("=", 60), "\n", sep = "")

start_date <- max(min(price_clean$date), min(cot_clean$date))
end_date <- min(max(price_clean$date), max(cot_clean$date))

cat("Common period:", as.character(start_date), "to", as.character(end_date), "\n")

price_clean <- price_clean %>% filter(date >= start_date, date <= end_date)
cot_clean <- cot_clean %>% filter(date >= start_date, date <= end_date)

cat("Price obs after filter:", nrow(price_clean), "\n")
cat("COT obs after filter:", nrow(cot_clean), "\n")

# 5. MERGE DATA
cat("\n", rep("=", 60), "\n", sep = "")
cat("MERGING DATA\n")
cat(rep("=", 60), "\n", sep = "")

merged <- inner_join(price_clean, cot_clean, by = "date") %>%
  arrange(date) %>%
  mutate(
    return = (price / lag(price) - 1) * 100,
    return_next = lead(return, 1),
    prod_lag = lag(prod_pct, 1),
    swap_lag = lag(swap_pct, 1),
    mm_lag = lag(mm_pct, 1),
    other_lag = lag(other_pct, 1)
  ) %>%
  filter(!is.na(return_next), !is.na(prod_lag))

cat("Merged observations:", nrow(merged), "\n")

if (nrow(merged) == 0) {
  stop("No data after merge. Check if dates align between files.")
}

# 6. CORRELATION ANALYSIS
cat("\n", rep("=", 60), "\n", sep = "")
cat("CORRELATION ANALYSIS\n")
cat(rep("=", 60), "\n", sep = "")

cor_results <- data.frame(
  Trader_Type = c("Producers", "Swap Dealers", "Managed Money", "Other Reportables"),
  Correlation = round(c(
    cor(merged$return_next, merged$prod_lag, use = "complete.obs"),
    cor(merged$return_next, merged$swap_lag, use = "complete.obs"),
    cor(merged$return_next, merged$mm_lag, use = "complete.obs"),
    cor(merged$return_next, merged$other_lag, use = "complete.obs")
  ), 4)
)

print(cor_results)

# 7. REGRESSION ANALYSIS
cat("\n", rep("=", 60), "\n", sep = "")
cat("REGRESSION ANALYSIS\n")
cat(rep("=", 60), "\n", sep = "")

model_mm <- lm(return_next ~ mm_lag, data = merged)
model_all <- lm(return_next ~ prod_lag + swap_lag + mm_lag + other_lag, data = merged)

cat("\n1. MANAGED MONEY alone:\n")
cat("   R-squared:", round(summary(model_mm)$r.squared * 100, 2), "%\n")
cat("   Coefficient:", round(coef(model_mm)[2], 4), "\n")

cat("\n2. ALL FOUR TRADER TYPES:\n")
cat("   R-squared:", round(summary(model_all)$r.squared * 100, 2), "%\n")

coef_summary <- summary(model_all)$coefficients
coef_df <- data.frame(
  Trader = c("Producers", "Swap Dealers", "Managed Money", "Other Reportables"),
  Coefficient = round(coef_summary[2:5, 1], 4),
  P_Value = round(coef_summary[2:5, 4], 4),
  Significant = ifelse(coef_summary[2:5, 4] < 0.05, "**", 
                       ifelse(coef_summary[2:5, 4] < 0.1, "*", ""))
)

cat("\n3. REGRESSION COEFFICIENTS:\n")
print(coef_df)

# 8. SUMMARY TABLE
cat("\n", rep("=", 60), "\n", sep = "")
cat("SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")

summary_table <- data.frame(
  Trader_Type = cor_results$Trader_Type,
  Correlation = cor_results$Correlation,
  Coefficient = coef_df$Coefficient,
  P_Value = coef_df$P_Value,
  Significant = coef_df$Significant
)

print(summary_table)



# Plot 1: Correlations
ggplot(cor_results, aes(x = reorder(Trader_Type, Correlation), y = Correlation, fill = Correlation)) +
  geom_col() +
  geom_text(aes(label = Correlation), hjust = -0.2) +
  coord_flip() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
  labs(title = "Correlation with Next Week Oil Return", 
       x = "", y = "Correlation Coefficient") +
  theme_minimal()

# Plot 2: Price over time
ggplot(merged, aes(x = date, y = price)) +
  geom_line(color = "blue", linewidth = 0.8) +
  labs(title = "Oil Price Over Time", 
       subtitle = paste0("Observations: ", nrow(merged)),
       x = "Date", y = "Price ($)") +
  theme_minimal()

cat("\n✓ Analysis complete!\n") debugg me the code now the excel where taking the data is this one that is easier since contain just the column that you need to use it 

