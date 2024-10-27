---
title: "Milestone 2"
author: "Vi Conrad"
date: "2024-10-27"
output: html_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = TRUE, message = FALSE)

# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(hms)
library(reshape2)
```
## Load and set Data
```{r loading_data}
# Set up data types for each column
sales_data <- read.csv("supermarket_sales.csv", stringsAsFactors = TRUE, colClasses = c(
  "invoice_id" = "character",
  "branch" = "factor",
  "city" = "factor",
  "customer_type" = "factor",
  "gender_customer" = "factor",
  "product_line" = "factor",
  "unit_cost" = "numeric",
  "quantity" = "integer",
  "5pct_markup" = "numeric",
  "revenue" = "numeric",
  "date" = "Date",
  "time" = "character",  # Assuming 'time' is initially character
  "payment_method" = "factor",
  "cogs" = "numeric",
  "gm_pct" = "numeric",
  "gross_income" = "numeric",
  "rating" = "numeric"
))
# Note: A warning may appear stating "not all columns named in colClasses exist."
# This warning is okay because we have confirmed that all necessary columns for our analysis are present in the dataset.

# Convert the 'time' column to POSIXct
sales_data$time <- as.POSIXct(sales_data$time, format = "%H:%M")

# Set new names for the columns
colnames(sales_data) <- c(
  "invoiceID",
  "branch",
  "city",
  "customerType",
  "gender",
  "category",
  "unitCost",
  "quantity",
  "markup",
  "revenue",
  "date",
  "time",
  "paymentMethod",
  "cogs",
  "gmpct",
  "grossIncome",
  "rating"
)

# Display the first few rows of the data to verify that the data has been loaded correctly
# This helps to ensure that the data frame is structured as expected
# head(sales_data)

# Display the structure of the data frame to check the data types of each column
# This is useful for understanding the types of data in your dataset (e.g., numeric, factor)
# str(sales_data)
```
# Customer Focused
## Customer Spending Habits - Customer Perspective
```{r boxplot-customer-spending-habits}
# Boxplot Analysis
ggplot(sales_data, aes(x = customerType, y = cogs)) +
  geom_boxplot(outlier.shape = NA) +  # Remove outliers from the box plot
  geom_jitter(alpha = 0.6, width = 0.2) +  # Jitter to spread points
  coord_flip() +  # Flip the axes
  ggtitle("Member Spent VS. Non-Members Spent") +
  theme_minimal()  
```
```{r barchart-customer-spending-habits}
# Bar Chart Analysis
cType_spent <- sales_data %>%  
  group_by(customerType, gender) %>%
  summarise(Total_cogs = sum(cogs), .groups = "drop")

ggplot(cType_spent, aes(x = customerType, y = Total_cogs, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Member Spent VS. Non-Members Spent") +
  labs(
    x = "Customer Type",          # Set the x-axis label
    y = "Total Cost of Goods Sold" # Set the y-axis label
  ) +
  theme_minimal()

# Convert ggplot to an interactive plotly object
#interactive_plot <- ggplotly(bar_chart, tooltip = c("customerType", "gender", "Total_cogs"))

# Show the interactive plot
#interactive_plot
```
## Date/Time vs Category Spending
```{r}
# Stacked bar Chart

```
```{r}
# Heatmap Analysis
```
## Find the distribution of payment methods used.
```{r}
# Count payment methods
payment_distribution <- sales_data %>%
  group_by(paymentMethod) %>%
  summarise(count = n())
payment_distribution
# bar chart
ggplot(payment_distribution, aes(x = paymentMethod, y = count, fill = paymentMethod)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Payment Methods", x = "Payment Method", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

```
## Payment Method vs Category vs Amount
```{r}
# Barchart
```
```{r}
# Heatmap Analysis
```
## Date/Time vs Amount Spent (Peak Spending Hours)
```{r}
# line graph
# might need to change how hourly_revenue is calculated? 
hourly_revenue <- data.frame(hour = 0:23, total_revenue = runif(24, min = 100, max = 1000))

ggplot(hourly_revenue, aes(x = hour, y = total_revenue)) + geom_line(color = "blue", size = 1) + geom_point(color = "blue", size = 2) +
  labs(title = "Total Revenue by Hour of the Day",
       x = "Hour of the Day",
       y = "Total Revenue") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23)
```
## Date/time vs Payment Method
```{r}
# line graph by day

```
```{r}
# Create a bar plot for payment method usage by hour of the day
sales_data$hour <- hour(sales_data$time)

ggplot(sales_data, aes(x = hour, fill = paymentMethod)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Payment Method Usage by Hour of the Day",
    x = "Hour of the Day",
    y = "Count of Payment Methods",
    fill = "Payment Method"
  ) +
  theme_minimal()
```
## Member/Non-member vs Rating
```{r}
# boxplot
ggplot(sales_data, aes(x=customerType, y=rating)) + geom_boxplot() + geom_jitter(alpha=0.6) + coord_flip() + ggtitle("Member Rating VS. Non-Members Rating")
```
## Markup vs Product Line (pink tax)
```{r}
ggplot(sales_data, aes(x = category, y = markup)) +
  geom_boxplot(color = "orange", fill = NA, outlier.color = "black", outlier.shape = 1) +
  coord_flip() +  labs(title = "Markup Distribution by Product Line", y = "Markup (%)", x = "Product Line") +  theme_minimal()
```
# Calculate the average gross income by gender and product line.
```{r}
# Average gross income by gender and category
gender_product_income <- sales_data %>%
  group_by(gender, category) %>%
  summarise(avg_gross_income = mean(grossIncome))
gender_product_income
```
# Company Focused
Make sure Branch = City
```{r}
# Count payment methods
payment_distribution <- sales_data %>%
  group_by(branch, city) %>%
  summarise(count = n())
payment_distribution
```

## What Sells Best for the Branch
```{r}
# Stacked Bar Chart
```
```{r}
# Heatmap
```
## Branch vs Payment Method
```{r}
ggplot(sales_data, aes(x=branch, fill=paymentMethod)) + geom_bar(position="dodge") + ggtitle("Branch VS. Payment")
```
```{r}
ggplot(sales_data, aes(x=branch, fill=paymentMethod)) + geom_bar(position="fill") + ggtitle("Branch VS. Payment")
```
```{r}

```

# My stuff
```{r}
# Gender vs categories
```
```{r}
# branch vs rating
#Calculate the average rating by product line.

# Average rating by product line
product_rating <- sales_data %>%
  group_by(category) %>%
  summarise(avg_rating = mean(rating))
product_rating
```


```{r}
# Bar Graph of count of purchases by gender and product line
ggplot(gender_category_count, aes(x = category, y = count, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Purchases by Gender and Product Line", x = "Product Line", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```
```{r}
# Average gross income by gender and category
gender_product_income <- sales_data %>%
  group_by(gender, category) %>%
  summarise(avg_gross_income = mean(grossIncome))
gender_product_income

```