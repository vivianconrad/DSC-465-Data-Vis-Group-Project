# Read the supermarket sales data
sales <- read.csv("supermarket_sales.csv")

# View the first few rows of the data
head(sales)

# Summary statistics of the data
summary(sales)

# Create a plot to analyze Gender vs Categories
library(ggplot2)
ggplot(sales, aes(x = Gender, fill = Product_line)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender vs. Product Line",
       x = "Gender",
       y = "Count")

library(dplyr)

# Total revenue by branch
branch_revenue <- data %>%
  group_by(branch) %>%
  summarise(total_revenue = sum(revenue))
branch_revenue
