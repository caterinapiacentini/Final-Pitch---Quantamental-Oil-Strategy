library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readxl)
library(zoo)

# =========================
# 1) DKW data (CSV) instead of TIPS.xlsx
# =========================
dkw_file <- "DKW_updates (1).csv"

# Find the header row that starts with "date," (your CSV has metadata lines on top)
header_idx <- which(grepl("^date,", readLines(dkw_file, n = 200)))[1]
if (is.na(header_idx)) stop("Could not find the header row starting with 'date,' in the CSV.")

dkw <- read_csv(
  dkw_file,
  skip = header_idx - 1,
  na = c("NA", "", "NaN")
) %>%
  mutate(
    Date = mdy(date)
  ) %>%
  # 10Y real yield (fitted) = nominal yield (fitted) - inflation compensation (fitted)
  mutate(
    DKW_RY10 = `nominal.yield.fitted.10` - `ic.fitted.10`
  ) %>%
  arrange(Date) %>%
  # Interpolate NAs (internal) + carry ends (if any)
  mutate(
    DKW_RY10 = na.approx(DKW_RY10, x = Date, na.rm = FALSE),
    DKW_RY10 = na.locf(DKW_RY10, na.rm = FALSE),
    DKW_RY10 = na.locf(DKW_RY10, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  select(Date, DKW_RY10) %>%
  filter(!is.na(Date), !is.na(DKW_RY10))

# =========================
# 2) Oil data (same as before), interpolate NAs as well
# =========================
oil_data <- read_excel("Super Puper Oil.xlsx", sheet = "Foglio1", skip = 1) %>%
  mutate(Date = as.Date(Date)) %>%
  rename(CL1 = Close) %>%
  arrange(Date) %>%
  mutate(
    CL1 = na.approx(CL1, x = Date, na.rm = FALSE),
    CL1 = na.locf(CL1, na.rm = FALSE),
    CL1 = na.locf(CL1, fromLast = TRUE, na.rm = FALSE)
  ) %>%
  filter(!is.na(Date), !is.na(CL1))

# =========================
# 3) Join + filter window (same as your code)
# =========================
macro_df <- inner_join(dkw, oil_data, by = "Date") %>%
  filter(Date >= as.Date("2016-05-01"))

# =========================
# 4) Correlation + regression (same structure)
# =========================
correlation_val <- cor(macro_df$DKW_RY10, macro_df$CL1)
reg_model <- lm(CL1 ~ DKW_RY10, data = macro_df)
reg_summary <- summary(reg_model)

cat("--- MACRO RESULTS (DKW, >=2016-05-01) ---\n",
    "Correlation (r): ", round(correlation_val, 3), "\n",
    "R-Squared:       ", round(reg_summary$r.squared, 3), "\n",
    "Regression: CL1 = ", round(coef(reg_model)[1], 2), " + ",
    round(coef(reg_model)[2], 2), " * DKW_RY10\n")

# =========================
# 5) Plot (same plot logic)
# =========================
ggplot(macro_df, aes(x = DKW_RY10, y = CL1)) +
  geom_point(alpha = 0.5, color = "royalblue") +
  geom_smooth(method = "lm", color = "red", size = 1.2, se = TRUE) +
  labs(
    title = "Post-2020 Macro: WTI Oil vs. TIPS Real Yield",
    subtitle = paste0("Correlation: ", round(correlation_val, 2),
                      " | R-Squared: ", round(reg_summary$r.squared, 3)),
    x = "10Y Real Yield (DKW fitted, % points)",
    y = "WTI Crude Price (CL1 $)"
  ) +
  theme_minimal()
