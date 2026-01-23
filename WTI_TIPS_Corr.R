
library(readxl)    
library(dplyr)
library(ggplot2)
library(lubridate)

tips_data <- read_excel("TIPS.xlsx", sheet = "Daily") %>%
  mutate(Date = as.Date(observation_date)) %>%
  select(Date, DFII10) %>%
  filter(!is.na(DFII10))

oil_data <- read_excel("Super Puper Oil.xlsx", sheet = "Foglio1", skip = 1) %>%
  mutate(Date = as.Date(Date)) %>%
  rename(CL1 = Close) %>%
  filter(!is.na(CL1))


macro_df <- inner_join(tips_data, oil_data, by = "Date") %>%
  filter(Date >= as.Date("2016-05-01"))


correlation_val <- cor(macro_df$DFII10, macro_df$CL1)
reg_model <- lm(CL1 ~ DFII10, data = macro_df)
reg_summary <- summary(reg_model)

# Print Summary to Console
cat("--- POST-2020 MACRO RESULTS ---\n",
    "Correlation (r): ", round(correlation_val, 3), "\n",
    "R-Squared:       ", round(reg_summary$r.squared, 3), "\n",
    "Regression: CL1 = ", round(coef(reg_model)[1], 2), " + ", 
    round(coef(reg_model)[2], 2), " * DFII10\n")


ggplot(macro_df, aes(x = DFII10, y = CL1)) +
  geom_point(alpha = 0.5, color = "royalblue") +
  geom_smooth(method = "lm", color = "red", size = 1.2, se = TRUE) +
  labs(
    title = "Post-2020 Macro: WTI Oil vs. TIPS Real Yield",
    subtitle = paste0("Correlation: ", round(correlation_val, 2), " | R-Squared: ", round(reg_summary$r.squared, 3)),
    x = "TIPS 10Y Real Yield (DFII10 %)",
    y = "WTI Crude Price (CL1 $)"
  ) +
  theme_minimal()