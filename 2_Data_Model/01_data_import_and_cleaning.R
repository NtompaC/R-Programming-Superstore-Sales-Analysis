# ================================================
# Project: Superstore Sales Analysis
# File: 01_data_import_and_cleaning.R
# Author: Mutono Ntompa
# Purpose: Import and clean the raw superstore data
# ================================================

# Load required libraries
library(tidyverse)
library(lubridate)
library(skimr)

# 1. Import the raw data
raw_data <- read_csv("0_Raw_Dataset/raw_superstore_sales.csv")

# 2. Quick overview of the data
print("Dataset Dimensions:")
dim(raw_data)
skim(raw_data)

# 3. Basic cleaning steps
cleaned_data <- raw_data %>%
  # Rename columns to snake_case for easier use in R
  janitor::clean_names() %>%   # You may need to install janitor if not working
  # Convert dates
  mutate(order_date = mdy(order_date),
         ship_date = mdy(ship_date)) %>%
  # Create useful derived columns
  mutate(
    year = year(order_date),
    month = month(order_date, label = TRUE),
    profit_margin = profit / sales * 100,
    days_to_ship = as.numeric(ship_date - order_date)
  )

# 4. Save cleaned data for later use
saveRDS(cleaned_data, "cleaned_superstore_data.rds")

# View first few rows
glimpse(cleaned_data)
# Opens the dataset in a spreadsheet view 
View(cleaned_data)

# ========================
# Quick Data Exploration
# ========================

# 1. Summary statistics
summary(cleaned_data)

# 2. Better summary using skimr
skim(cleaned_data)

# 3. Check for missing values
colSums(is.na(cleaned_data))

# 4. Key business questions - quick insights
cleaned_data %>%
  summarise(
    total_sales = sum(sales),
    total_profit = sum(profit),
    overall_profit_margin = mean(profit_margin, na.rm = TRUE),
    avg_discount = mean(discount),
    total_orders = n_distinct(order_id)
  )

# 5. Top 5 Categories by Sales
cleaned_data %>%
  group_by(category) %>%
  summarise(total_sales = sum(sales),
            total_profit = sum(profit),
            profit_margin = mean(profit_margin)) %>%
  arrange(desc(total_sales))

# View the first 10 rows
head(cleaned_data, 10)
