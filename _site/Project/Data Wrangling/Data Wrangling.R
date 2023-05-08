library(readxl)
library(stringr)

# change column names
bikes_tbl <- read_excel('bikes.xlsx')
names(bikes_tbl) <- set_names(names(bikes_tbl) %>% str_replace_all('\\.', '_'))

# turn one column into two
bikes_tbl <- bikes_tbl %>% separate(col = 'category',
      into = c('category_1', 'category_2', 'category_3'), sep = ' - ')


bikes_tbl %>% select(model, category_1, category_2, category_3, price) %>%
  rename( 'Model' = 'model', 'Bike Family' = 'category_1', 'Ride Style' = 'category_2',
          'Bike Category' = 'category_3', 'Price in Euro' = 'price')

bikes_tbl %>% select(model, category_1, category_2, category_3, price) %>% set_names(
  'Model', 'Bike Family', 'Ride Style', 'Bike Category', 'Price in Euro')

