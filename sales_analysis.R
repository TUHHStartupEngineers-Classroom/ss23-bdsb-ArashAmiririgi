# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bikes <- read_excel('bikes.xlsx')
bikeshops <- read_excel('bikeshops.xlsx')
orderlines <- read_excel('orderlines.xlsx')

# 3.0 Examining Data ----
glimpse(orderlines)

# 4.0 Joining Data ----
joined_data <- left_join(orderlines, bikes,
  by=c('product.id'='bike.id')) %>% left_join(bikeshops,
  by=c('customer.id'='bikeshop.id'))

# 5.0 Wrangling Data ----
joined_data_wragled <- separate(joined_data, col = 'category',
  into = c('category.1', 'category.2', 'category.3')
  ,sep = ' - ')
joined_data_wragled <- joined_data_wragled %>% mutate('total_price' = price * quantity)
joined_data_wragled <- joined_data_wragled %>% select(-c(...1, gender))
joined_data_wragled <- joined_data_wragled %>% select(-ends_with('.id'))
joined_data_wragled <- bind_cols(joined_data_wragled, joined_data %>% select(order.id))
joined_data_wragled <- joined_data_wragled %>% select(order.id, contains('order'),
  contains('model'), contains('category'), price, quantity, total_price, everything())
glimpse(joined_data_wragled)

################################last step is not done

# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate

# Step 2 - Visualize


# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate

# Step 2 - Visualize



# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----