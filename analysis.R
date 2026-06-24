# analysis.R
# Nairobi Rental Price Trends Analysis (2021-2026)
# Date: 2026-06-24

library(tidyverse)
library(lubridate)
library(scales)
library(broom)

# 1. Synthetic Data Generation
# Simulating Nairobi rent data for 5 major neighborhoods over 5.5 years
set.seed(20260624)

neighborhoods <- c("Westlands", "Kilimani", "Karen", "Langata", "Kasarani")
dates <- seq(as.Date("2021-01-01"), as.Date("2026-06-01"), by = "month")

# Base prices represent average 2-bedroom apartments in KES
nairobi_rent_data <- expand.grid(Date = dates, Neighborhood = neighborhoods) %>%
  mutate(
    BasePrice = case_when(
      Neighborhood == "Karen" ~ 160000,
      Neighborhood == "Westlands" ~ 110000,
      Neighborhood == "Kilimani" ~ 85000,
      Neighborhood == "Langata" ~ 55000,
      Neighborhood == "Kasarani" ~ 30000
    ),
    # Adding a growth factor and seasonal noise
    MonthCount = as.numeric(interval(min(Date), Date) %/% months(1)),
    GrowthFactor = case_when(
      Neighborhood == "Westlands" ~ 1.006,  # 0.6% monthly growth
      Neighborhood == "Kasarani" ~ 1.004,   # 0.4% monthly growth
      TRUE ~ 1.005                         # 0.5% default monthly growth
    ),
    Seasonality = 1000 * sin(2 * pi * month(Date) / 12),
    RandomError = rnorm(n(), mean = 0, sd = 2000),
    Rent_KES = (BasePrice * (GrowthFactor ^ MonthCount)) + Seasonality + RandomError
  ) %>%
  select(Date, Neighborhood, Rent_KES)

# 2. Descriptive Statistics
cat("--- Neighborhood Rental Summary (2021-2026) ---\n")
rent_summary <- nairobi_rent_data %>%
  group_by(Neighborhood) %>%
  summarise(
    Min_Rent = min(Rent_KES),
    Max_Rent = max(Rent_KES),
    Current_Rent = last(Rent_KES),
    Average_Rent = mean(Rent_KES),
    Volatility = sd(Rent_KES)
  ) %>%
  arrange(desc(Current_Rent))

print(rent_summary)

# 3. Time Series Visualization
trend_plot <- ggplot(nairobi_rent_data, aes(x = Date, y = Rent_KES, color = Neighborhood)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE, size = 0.5) +
  scale_y_continuous(labels = label_comma(prefix = "KES ")) +
  labs(
    title = "Nairobi Rental Price Evolution by Neighborhood",
    subtitle = "Historical trends from Jan 2021 to June 2026",
    x = "Timeline",
    y = "Monthly Rent (2-Bedroom)",
    caption = "Source: Simulated Market Data Analysis"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

# 4. Growth Rate Calculation
# Calculating Compound Annual Growth Rate (CAGR)
cagr_analysis <- nairobi_rent_data %>%
  group_by(Neighborhood) %>%
  summarise(
    Start_Val = first(Rent_KES),
    End_Val = last(Rent_KES),
    Years = as.numeric(max(Date) - min(Date)) / 365.25
  ) %>%
  mutate(CAGR_Percent = ((End_Val / Start_Val)^(1 / Years) - 1) * 100)

cat("\n--- Compound Annual Growth Rate (CAGR) by Area ---\n")
print(cagr_analysis %>% select(Neighborhood, CAGR_Percent))

# 5. Simple Linear Forecasting for 2027
cat("\n--- 2027 Forecast Insights ---\n")
forecast_2027 <- nairobi_rent_data %>%
  group_by(Neighborhood) %>%
  do(model = lm(Rent_KES ~ MonthCount, data = .)) %>%
  mutate(
    Forecast_Dec_2027 = predict(model, newdata = data.frame(MonthCount = 83))
  ) %>%
  select(Neighborhood, Forecast_Dec_2027)

print(forecast_2027)

# Save outputs
# ggsave("nairobi_rent_trends.png", trend_plot, width = 10, height = 6)
# write.csv(nairobi_rent_data, "nairobi_rent_data_cleaned.csv", row.names = FALSE)

cat("\nAnalysis Complete. Visualizations generated.")