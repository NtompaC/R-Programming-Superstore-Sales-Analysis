# ================================================
# Project: Superstore Sales Analysis
# File: 02_analysis_and_modeling.R
# Author: Mutono Ntompa
# Purpose: Exploratory analysis, KPIs, and statistical insights
# ================================================

library(tidyverse)
library(lubridate)

# Load cleaned data
cleaned_data <- readRDS("cleaned_superstore_data.rds")

# 1. Overall KPIs
overall_kpis <- cleaned_data %>%
  summarise(
    total_sales = sum(sales),
    total_profit = sum(profit),
    profit_margin_pct = round(mean(profit_margin, na.rm = TRUE), 2),
    total_quantity = sum(quantity),
    avg_discount = round(mean(discount), 3),
    number_of_orders = n_distinct(order_id)
  )

print("=== Overall Business KPIs ===")
print(overall_kpis)
View(overall_kpis)

# 2. Sales & Profit by Category and Sub-Category
category_analysis <- cleaned_data %>%
  group_by(category, sub_category) %>%
  summarise(
    total_sales = sum(sales),
    total_profit = sum(profit),
    profit_margin = round(mean(profit_margin), 2),
    order_count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))

print("=== Top Categories by Sales ===")
print(category_analysis %>% slice_max(total_sales, n = 10))
View(category_analysis)

# 3. Regional Performance
region_analysis <- cleaned_data %>%
  group_by(region) %>%
  summarise(
    total_sales = sum(sales),
    total_profit = sum(profit),
    profit_margin = round(mean(profit_margin), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sales))
View(region_analysis)

# 4. Impact of Discount on Profit (important insight)
discount_impact <- cleaned_data %>%
  group_by(discount) %>%
  summarise(
    avg_profit = mean(profit),
    avg_sales = mean(sales),
    count = n()
  )

# 5. Simple Linear Regression: Does Discount predict Profit negatively?
model <- lm(profit ~ discount + quantity + days_to_ship, data = cleaned_data)
summary(model)

# Save results for later use in visualizations
saveRDS(category_analysis, "category_analysis.rds")
saveRDS(region_analysis, "region_analysis.rds")
