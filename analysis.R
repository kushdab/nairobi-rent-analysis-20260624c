library(tidyverse)
library(lubridate)
library(scales)
library(broom)

# Set seed for reproducibility
set.seed(20260624)

# 1. Synthetic Data Generation
# Simulating Nairobi rental data for 5 major neighborhoods over 5 years (60 months)
neighborhoods <- c("Westlands", "Kilimani", "Karen", "Langata", "Roysambu")
start_date <- as.Date("2021-01-01")
months_seq <- seq(start_date, by = "month", length.out = 60)

df_raw <- expand.grid(Date = months_seq, Neighborhood = neighborhoods) %>%
  mutate(
    # Baseline prices (KES)
    base_price = case_when(
      Neighborhood == "Karen" ~ 150000,
      Neighborhood == "Westlands" ~ 110000,
      Neighborhood == "Kilimani" ~ 95000,
      Neighborhood == "Langata" ~ 65000,
      Neighborhood == "Roysambu" ~ 35000,
      TRUE ~ 50000
    ),
    # Add a general market trend (3-5% annual growth) + seasonal noise + random error
    month_index = as.numeric(difftime(Date, start_date, units = "days")) / 30,
    trend = 1 + (month_index * 0.005), # Roughly 6% annual increase
    seasonal = 1 + (0.02 * sin(month_index * pi / 6)),
    noise = rnorm(n(), mean = 1, sd = 0.03),
    Price = base_price * trend * seasonal * noise
  )

# 2. Data Cleaning and Feature Engineering
rent_data <- df_raw %>%
  mutate(
    Year = year(Date),
    Month = month(Date, label = TRUE)
  ) %>%
  select(Date, Year, Month, Neighborhood, Price) %>%
  arrange(Date, Neighborhood)

# 3. Exploratory Data Analysis
summary_stats <- rent_data %>%
  group_by(Neighborhood) %>%
  summarize(
    Mean_Rent = mean(Price),
    Median_Rent = median(Price),
    Min_Rent = min(Price),
    Max_Rent = max(Price),
    Growth_Total_Pct = (last(Price) - first(Price)) / first(Price) * 100
  )

print("--- Nairobi Rental Summary Statistics (2021-2026) ---")
print(summary_stats)

# 4. Visualization: Price Trends Over Time
trend_plot <- ggplot(rent_data, aes(x = Date, y = Price, color = Neighborhood)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_smooth(method = "lm", linetype = "dashed", size = 0.5, se = FALSE) +
  scale_y_continuous(labels = label_comma(prefix = "KES ")) +
  labs(
    title = "Nairobi Rental Price Trends by Neighborhood",
    subtitle = "Historical Analysis: 2021 - 2026",
    x = "Year",
    y = "Monthly Rent (KES)",
    caption = "Source: Simulated Nairobi Real Estate Index"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("nairobi_rent_trends.png", plot = trend_plot, width = 10, height = 6)

# 5. Comparative Analysis: Annual Average Rent
annual_plot <- rent_data %>%
  group_by(Year, Neighborhood) %>%
  summarize(Avg_Price = mean(Price), .groups = 'drop') %>%
  ggplot(aes(x = factor(Year), y = Avg_Price, fill = Neighborhood)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = label_comma(prefix = "KES ")) +
  labs(
    title = "Annual Average Rent Comparison",
    x = "Year",
    y = "Average Monthly Rent",
    fill = "Neighborhood"
  ) +
  theme_light()

ggsave("annual_comparison.png", plot = annual_plot, width = 10, height = 6)

# 6. Statistical Modeling: Growth Rates
models <- rent_data %>%
  group_by(Neighborhood) %>%
  do(model = lm(Price ~ month_index, data = df_raw[df_raw$Neighborhood == .$Neighborhood[1], ])) %>%
  tidy(model)

cat("\n--- Linear Growth Coefficients by Neighborhood ---\n")
print(models %>% filter(term == "month_index") %>% select(Neighborhood, estimate, p.value))

cat("\nAnalysis Complete. Plots saved as nairobi_rent_trends.png and annual_comparison.png.\n")