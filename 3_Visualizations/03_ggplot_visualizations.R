# ================================================
# Project: Superstore Sales Analysis
# File: 03_ggplot_visualizations.R
# Author: Mutono Ntompa
# Purpose: Create publication-quality visualizations using ggplot2
# ================================================

library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)

#Creating a reusable Ksh formatter
ksh_format <- scales::label_number(prefix = "KSh ", big.mark = ",")

# Load data
cleaned_data <- readRDS("cleaned_superstore_data.rds")
category_analysis <- readRDS("category_analysis.rds")
region_analysis <- readRDS("region_analysis.rds")

# ========================================
# 1. Overall Sales & Profit Trend by Year
# ========================================
trend_plot <- cleaned_data %>%
  group_by(year) %>%
  summarise(
    total_sales = sum(sales),
    total_profit = sum(profit)
  ) %>%
  ggplot(aes(x = year)) +
  geom_line(aes(y = total_sales, color = "Total Sales"), linewidth = 1.2) +
  geom_line(aes(y = total_profit, color = "Total Profit"), linewidth = 1.2) +
  geom_point(aes(y = total_sales), size = 3) +
  geom_point(aes(y = total_profit), size = 3) +
  labs(title = "Sales & Profit Trend Over Time",
       subtitle = "Superstore Sales Analysis (Kenyan Supermarket Context)",
       x = "Year", y = "Amount (Ksh)") +
  scale_y_continuous(labels = ksh_format) +
  scale_color_manual(values = c("Total Sales" = "#1f77b4", "Total Profit" = "#2ca02c")) +
  theme_minimal() +
  theme(legend.position = "top")

# ========================================
# 2. Profit Margin by Category
# ========================================
category_plot <- category_analysis %>%
  slice_max(total_sales, n = 8) %>%
  ggplot(aes(x = reorder(sub_category, total_sales), y = profit_margin)) +
  geom_col(fill = "#ff7f0e", alpha = 0.85) +
  coord_flip() +
  labs(title = "Profit Margin by Sub-Category",
       x = "Sub-Category", y = "Profit Margin (%)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# ========================================
# 3. Sales by Region
# ========================================
region_plot <- region_analysis %>%
  ggplot(aes(x = reorder(region, total_sales), y = total_sales)) +
  geom_col(fill = "#9467bd", alpha = 0.85) +
  coord_flip() +
  labs(title = "Total Sales by Region",
       x = "Region", y = "Total Sales") +
  scale_y_continuous(labels = ksh_format) +
  theme_minimal()

# ========================================
# 4. Discount vs Profit Scatter Plot
# ========================================
discount_plot <- cleaned_data %>%
  sample_n(2000) %>%   # Sample for clearer visualization
  ggplot(aes(x = discount, y = profit, color = category)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "Impact of Discount on Profit by Category",
       x = "Discount Rate", y = "Profit") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal()

# ========================================
# Combine plots using patchwork
# ========================================
combined_plot <- (trend_plot | region_plot) / 
  (category_plot | discount_plot)

combined_plot <- combined_plot + 
  plot_annotation(
    title = "Superstore Sales & Profitability Dashboard",
    subtitle = "Key Insights for a Kenyan Supermarket Chain",
    caption = "Created by Mutono Ntompa | R Programming Portfolio Project",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

# Display the combined dashboard
print(combined_plot)

# Save high-quality image for reports
ggsave("3_Visualizations/superstore_dashboard.png", 
       combined_plot, width = 14, height = 10, dpi = 300)

