library(readxl)
library(stringr)
library(tidyverse)
library(scales)

## recap
# 1

bikes_tbl <- read_excel('bikes.xlsx')
names(bikes_tbl) <- set_names(names(bikes_tbl) %>% str_replace_all('\\.', '_'))

bikes_tbl <- bikes_tbl %>% separate(col = 'category',
  into = c('category_1', 'category_2', 'category_3'), sep = ' - ')

bikes_tbl %>% select(model, category_1, category_2, category_3, price) %>%
  rename( 'Model' = 'model', 'Bike Family' = 'category_1', 'Ride Style' = 'category_2',
  'Bike Category' = 'category_3', 'Price in Euro' = 'price')

bikes_tbl %>% select(model, category_1, category_2, category_3, price) %>% set_names(
  'Model', 'Bike Family', 'Ride Style', 'Bike Category', 'Price in Euro')

bikes_tbl %>% select(model, category_1, category_2, category_3, price) %>%
  set_names(names(.) %>% str_replace_all('_', ' ') %>% str_to_title())

# 2

bikes_tbl %>% select(model, price) %>% arrange(desc(price))

bikes_tbl %>% select(model, price) %>% filter(price < '1000' | price > '5000') %>% arrange(desc(price))

bikes_tbl %>% select(model, price) %>% filter(price > 5000 & model %>% str_detect('Endurace'))

bikes_tbl %>% filter(!(category_1 %>% str_detect('City') | category_1 %>% str_detect('E-Bikes')))

bikes_tbl %>% filter(category_2 != 'E-Mountain')

bikes_tbl %>% filter(!(category_1 %in% c("Hybrid / City", "E-Bikes")))

bikes_tbl %>% arrange(desc(price)) %>% slice((nrow(bikes_tbl)-4):nrow(bikes_tbl))

bikes_tbl %>% distinct(category_2, category_1, category_1)

# 3

joined_data_wrangled <- read_rds('joined_data_wragled.rds')
joined_data_wrangled %>% mutate(freight_cost = 2 * weight)
joined_data_wrangled %>% mutate(total_price = log(total_price))
joined_data_wrangled %>% mutate(price_log = log(total_price)) %>% mutate(price_sqrt = total_price ^ 0.5)
joined_data_wrangled %>% mutate(is_strive = model %>% str_to_lower() %>% str_detect('strive')) %>% filter(is_strive)
joined_data_wrangled %>% mutate(price_binned = ntile(total_price, 3)) %>%
  select(total_price, price_binned, everything())
joined_data_wrangled %>% mutate(price_binned = ntile(total_price, 3)) %>%
  mutate(price_binned2 = case_when(
  total_price > quantile(total_price, 0.75) ~ 'High',
  total_price > quantile(total_price, 0.25) ~ 'Mediun',
  TRUE ~ 'Low'))
joined_data_wrangled %>% mutate(is_Aeroad = case_when(
  (model %>% str_to_lower() %>% str_detect('aeroad')) ~ 'Aeroad',
  (model %>% str_to_lower() %>% str_detect('ultimate')) ~ 'Ultimate',
  TRUE ~ 'NOT'))

# 4

joined_data_wrangled %>% summarise(revenue = max(total_price))
joined_data_wrangled %>% group_by(category_1) %>% summarise(revenue = sum(total_price))
joined_data_wrangled %>% group_by(category_1, category_2) %>%
  summarise( count = n(),
             avg   = mean(total_price),
             med   = median(total_price),
             sd    = sd(total_price),
             min   = min(total_price),
             max   = max(total_price)
  ) %>% ungroup()

bike_orderlines_missing <- joined_data_wrangled %>%
  mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))

bike_orderlines_missing %>% summarise(across(total_price, ~sum(is.na(.))))
bike_orderlines_missing %>% summarise(across(total_price, ~sum(is.na(.)) / length(.)))
bike_orderlines_missing %>% filter(!is.na(total_price))

# 5

bikeshop_revenue_tbl <- joined_data_wrangled %>% select(bikeshop, category_1, total_price) %>%
  group_by(bikeshop, category_1) %>%
  summarise(sales = sum(total_price)) %>% ungroup() %>% arrange(desc(sales))
bikeshop_revenue_formatted_tbl<- bikeshop_revenue_tbl %>%
  pivot_wider(names_from = category_1, values_from = sales) %>%
  mutate(Mountain = dollar(Mountain, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
         Gravel = scales::dollar(Gravel, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
         Road = scales::dollar(Road, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
         `Hybrid / City` = scales::dollar(`Hybrid / City`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"),
         `E-Bikes` = scales::dollar(`E-Bikes`, big.mark = ".", decimal.mark = ",", prefix = "", suffix = " €"))

bikeshop_revenue_formatted_tbl %>% pivot_longer(cols = c(names(.)[2:6]), names_to = 'category_1', values_to = 'sales') %>%
  mutate(sales = sales %>% str_remove_all('€|\\.'))

# 6

order_dates_tbl <- joined_data_wrangled %>% select(1:3)
order_items_tbl <- joined_data_wrangled %>% select(1:2, 4:8)
left_join(order_items_tbl, order_items_tbl,
          by = c('order_id'='order_id',  "order_line" = "order_line"))
joined_data_wrangled %>% select(-contains('category')) %>%
  bind_cols(joined_data_wrangled %>% select(category_1))
train_tbl <- joined_data_wrangled %>% slice(1:(nrow(.)/2))
test_tbl <-  joined_data_wrangled %>% slice((nrow(.)/2+1):nrow(.))
bind_rows(train_tbl, test_tbl)  

# 7

joined_data_wrangled %>% separate(order_date, into = c('year', 'month', 'day'), sep = '-') %>%
  unite(col = 'order_date', c('year', 'month', 'day'), sep = '-', remove = TRUE) %>%
  mutate(order_date = as.numeric(order_date))

## data.table
# 1

library(data.table)
library(lubridate)

# 2

url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)
#covid_data_dt[i, j, by]
covid_data_dt[year == 2019, sum(cases), by = continentExp]

# 3

covid_data_dt[countriesAndTerritories == 'Germany' & 
  lubridate::month(dateRep, label = T, abbr = T) == 'Jan']
covid_data_dt[1:2]
covid_data_dt[order(year, month, day, -countriesAndTerritories)]

# 4

covid_data_dt[, c('geoId', 'countriesAndTerritories')]
covid_data_dt[, .(geoId, countriesAndTerritories)]
covid_data_dt[,list(geoId)]
covid_data_dt[,.(geoId)]
covid_data_dt[, !c('geoId', 'countriesAndTerritories')]
covid_data_dt[, .(CountryCode = geoId, country = countriesAndTerritories)]
select_cols = c("cases", "deaths")
covid_data_dt[, ..select_cols]
colnames(covid_data_dt)
setnames(covid_data_dt, "dateRep", "date")
setnames(covid_data_dt, "countriesAndTerritories", "country")
setnames(covid_data_dt, "continentExp", "continent")

# 4 Exercise
aq_dt <- data.table(airquality)
aq_dt[!is.na(Ozone), .(Solar.R, Wind, Temp)]

# 5

covid_data_dt[, sum(deaths>1000)]
covid_data_dt[deaths>1000]

# 6

covid_data_dt[, deaths_per_capita := deaths/popData2019]
covid_data_dt[, `:=`(deaths_per_capita = deaths / popData2019,
                    cases_per_capita = cases / popData2019)]
covid_data_dt[, deaths_per_cases := NULL]

# 6 Exercise

mtcars_dt <- data.table(mtcars)
mtcars_dt[, mileage_type := ifelse(mpg > 20, 'high', 'low')]

# 7

covid_data_dt[country == 'Germany' & month(date) == 4,
              .(m_cases = mean(cases),
              m_deaths = mean(deaths))]
covid_data_dt[country == 'United_States_of_America' & month == 5 &
              deaths < 1000, length(day)]
covid_data_dt[country == 'United_States_of_America' & month == 5,
                sum(deaths < 1000)]
covid_data_dt[country == 'United_States_of_America' & month == 5 &
                deaths < 1000, .N]

# 8

covid_data_dt[deaths > 1000]
covid_data_dt[, .I[deaths > 1000 & country == 'United_States_of_America']]

covid_data_dt[continent == 'Europe', .(ave_deaths = mean(deaths),
            ave_cases = mean(cases)), by = .(country, month, year)]

# 8 Exercise

mtcars_dt[, .(.N, mileage = mean(mpg) %>% round(2)), by=gear]

# 9

covid_cases_means <- covid_data_dt[, .(m_deaths = mean(deaths) %>% round(1),
  m_cases = mean(cases) %>% round(2)), by = country]
covid_cases_means <- covid_cases_means[order(-m_deaths)]
covid_cases_means <- covid_data_dt[, .(m_deaths = round(mean(deaths),
  digits = 2), m_cases = round(mean(cases), digits = 2)), by = country][order(-m_deaths)]

covid_data_dt[deaths>1000 & cases<1000, .N, by = country]
covid_data_dt[, .N, by = .(death_gt_1k = deaths > 1000,
                           cases_lt_1k = cases < 1000, month, year)]

# 10

covid_data_dt[, print(.SD), by = year]
covid_data_dt[, lapply(.SD, mean), by = year, .SDcols = c(5:6)]
covid_data_dt[, lapply(.SD, sum), by = country, .SDcols = -(c(1:4, 7:14))]

# 11

setkey(covid_data_dt, date, country)
key(covid_data_dt)
covid_data_EUR_dt <- covid_data_dt[continent == 'Europe', lapply(.SD, function(x){
  round(mean(x), digits = 2)}), by = country, .SDcols = c('cases', 'deaths')]

setkey(covid_data_EUR_dt, country)
key(covid_data_EUR_dt)
cd_dt1 <- covid_data_EUR_dt[, .(country, cases)]
cd_dt2 <- covid_data_EUR_dt[1:20, .(country, deaths)]
cd_dt1[cd_dt2]

setkey(cd_dt1, NULL)
setkey(cd_dt2, NULL)
cd_dt1[cd_dt2, on = 'country']

# 12

m = matrix(1, nrow = 20, ncol = 5)
df = as.data.frame(m)
dt = as.data.table(m)

for (i in 1:20) df[i, 1] = 3
for (i in 1:5) {df[2, i] <- 4}
for (i in 1:20) {set(df, i, 1L, 3)}
for (i in 1:5) {set(df, 2L, i, 4)}
df

## Business case

# 1

library(tidyverse)
library(data.table)
library(vroom)
library(tictoc)

# 2

col_types_acq <- list(
  loan_id                            = col_factor(),
  original_channel                   = col_factor(NULL),
  seller_name                        = col_factor(NULL),
  original_interest_rate             = col_double(),
  original_upb                       = col_integer(),
  original_loan_term                 = col_integer(),
  original_date                      = col_date("%m/%Y"),
  first_pay_date                     = col_date("%m/%Y"),
  original_ltv                       = col_double(),
  original_cltv                      = col_double(),
  number_of_borrowers                = col_double(),
  original_dti                       = col_double(),
  original_borrower_credit_score     = col_double(),
  first_time_home_buyer              = col_factor(NULL),
  loan_purpose                       = col_factor(NULL),
  property_type                      = col_factor(NULL),
  number_of_units                    = col_integer(),
  occupancy_status                   = col_factor(NULL),
  property_state                     = col_factor(NULL),
  zip                                = col_integer(),
  primary_mortgage_insurance_percent = col_double(),
  product_type                       = col_factor(NULL),
  original_coborrower_credit_score   = col_double(),
  mortgage_insurance_type            = col_double(),
  relocation_mortgage_indicator      = col_factor(NULL))

acquisition_data <- vroom(file = 'loan_data/loan_data/Acquisition_2019Q1.txt',
                          delim = '|', col_names = names(col_types_acq),
                          col_types = col_types_acq, na = c('', 'NA', 'NULL'))

col_types_perf = list(
  loan_id                                = col_factor(),
  monthly_reporting_period               = col_date("%m/%d/%Y"),
  servicer_name                          = col_factor(NULL),
  current_interest_rate                  = col_double(),
  current_upb                            = col_double(),
  loan_age                               = col_double(),
  remaining_months_to_legal_maturity     = col_double(),
  adj_remaining_months_to_maturity       = col_double(),
  maturity_date                          = col_date("%m/%Y"),
  msa                                    = col_double(),
  current_loan_delinquency_status        = col_double(),
  modification_flag                      = col_factor(NULL),
  zero_balance_code                      = col_factor(NULL),
  zero_balance_effective_date            = col_date("%m/%Y"),
  last_paid_installment_date             = col_date("%m/%d/%Y"),
  foreclosed_after                       = col_date("%m/%d/%Y"),
  disposition_date                       = col_date("%m/%d/%Y"),
  foreclosure_costs                      = col_double(),
  prop_preservation_and_repair_costs     = col_double(),
  asset_recovery_costs                   = col_double(),
  misc_holding_expenses                  = col_double(),
  holding_taxes                          = col_double(),
  net_sale_proceeds                      = col_double(),
  credit_enhancement_proceeds            = col_double(),
  repurchase_make_whole_proceeds         = col_double(),
  other_foreclosure_proceeds             = col_double(),
  non_interest_bearing_upb               = col_double(),
  principal_forgiveness_upb              = col_double(),
  repurchase_make_whole_proceeds_flag    = col_factor(NULL),
  foreclosure_principal_write_off_amount = col_double(),
  servicing_activity_indicator           = col_factor(NULL))

performance_data <- vroom(
  file       = "loan_data/loan_data/Performance_2019Q1.txt", 
  delim      = "|", 
  col_names  = names(col_types_perf),
  col_types  = col_types_perf,
  na         = c("", "NA", "NULL"))

# 3

setDT(acquisition_data)
setDT(performance_data)

# 4

tic()
combined_data <- merge(x = acquisition_data, y = performance_data,
                       by = "loan_id", all.x = T, all.y = F)
toc()
tic()
performance_data %>%
  left_join(acquisition_data, by = "loan_id")
toc()

setkey(combined_data, 'loan_id')
##### combined_data %>% arrange(loan_id, monthly_reporting_period)
setorderv(combined_data, c('loan_id', 'monthly_reporting_period'), c(1, 1))
##### setorder(combined_data, loan_id, monthly_reporting_period)

combined_data %>% dim()
keep_cols <- c("loan_id",
               "monthly_reporting_period",
               "seller_name",
               "current_interest_rate",
               "current_upb",
               "loan_age",
               "remaining_months_to_legal_maturity",
               "adj_remaining_months_to_maturity",
               "current_loan_delinquency_status",
               "modification_flag",
               "zero_balance_code",
               "foreclosure_costs",
               "prop_preservation_and_repair_costs",
               "asset_recovery_costs",
               "misc_holding_expenses",
               "holding_taxes",
               "net_sale_proceeds",
               "credit_enhancement_proceeds",
               "repurchase_make_whole_proceeds",
               "other_foreclosure_proceeds",
               "non_interest_bearing_upb",
               "principal_forgiveness_upb",
               "repurchase_make_whole_proceeds_flag",
               "foreclosure_principal_write_off_amount",
               "servicing_activity_indicator",
               "original_channel",
               "original_interest_rate",
               "original_upb",
               "original_loan_term",
               "original_ltv",
               "original_cltv",
               "number_of_borrowers",
               "original_dti",
               "original_borrower_credit_score",
               "first_time_home_buyer",
               "loan_purpose",
               "property_type",
               "number_of_units",
               "property_state",
               "occupancy_status",
               "primary_mortgage_insurance_percent",
               "product_type",
               "original_coborrower_credit_score",
               "mortgage_insurance_type",
               "relocation_mortgage_indicator")
combined_data <- combined_data[, ..keep_cols]

combined_data[, current_loan_delinquency_status] %>% unique()
combined_data[, gt_1mo_behind_in_3mo := lead(current_loan_delinquency_status, n = 3) >= 1,
              by = loan_id]
combined_data %>% dim()

# 5

combined_data[!(is.na(monthly_reporting_period)), .N, by = monthly_reporting_period]
combined_data[current_loan_delinquency_status >= 1, .(loan_id,  monthly_reporting_period, current_loan_delinquency_status, seller_name, current_upb)][,
    .(max = max(current_loan_delinquency_status)), by = loan_id][order(-max)]
combined_data %>% filter(current_loan_delinquency_status >= 1) %>% group_by(loan_id) %>%
  summarise(total_delinq = max(current_loan_delinquency_status)) %>%
  ungroup() %>% arrange(desc(total_delinq))


combined_data[current_loan_delinquency_status >= 1, .SD[.N], by = loan_id][!is.na(current_upb)][
  order(-current_upb)][, .(loan_id, monthly_reporting_period, current_loan_delinquency_status, seller_name, current_upb)]
combined_data %>% filter(current_loan_delinquency_status >= 1) %>%
  filter(!is.na(current_upb)) %>%
  group_by(loan_id) %>% slice(n()) %>% ungroup() %>% arrange(desc(current_upb)) %>%
  select(loan_id, monthly_reporting_period, current_loan_delinquency_status, seller_name, current_upb)

upb_by_company_dt <- combined_data[!is.na(current_upb), .SD[.N], by = loan_id][,
  .(sum_current_upb = sum(current_upb, na.rm = T), cnt_current_upb = .N), by = seller_name][
  order(sum_current_upb)]
upb_by_company_tbl <- combined_data %>% filter(!is.na(current_upb)) %>% group_by(loan_id) %>%
  slice(n()) %>% ungroup() %>% group_by(seller_name) %>% summarise(sum_current_upb = sum(current_upb, na.rm = T), cnt_current_upb = n()) %>%
  arrange(desc(sum_current_upb))

# challenge

library(tidyverse)
library(data.table)
library(vroom)
library(lubridate)

col_patent_types <- list(id = col_character(),
                         date = col_date("%Y-%m-%d"),
                         num_claims = col_double())
col_patent_assignee_types <- list(patent_id = col_character(),
                                  assignee_id = col_character())
col_assignee_types <- list(id = col_character(),
                           type = col_double(),
                           organization = col_character())
col_uspc_types <- list(patent_id = col_character(),
                       mainclass_id = col_double(),
                       sequence = col_double())

patent_tbl <- vroom(file = "patent.tsv", delim = "\t", col_types  = col_patent_types, 
                    na = c("", "NA", "NULL"))

patent_assignee_tbl <- vroom(file = "patent_assignee.tsv", delim = "\t", col_types  = col_patent_assignee_types, 
                    na = c("", "NA", "NULL"))
assignee_tbl <- vroom(file = "assignee.tsv", delim = "\t", col_types  = col_assignee_types, 
                             na = c("", "NA", "NULL"))
uspc_tbl <- vroom(file = "uspc.tsv", delim = "\t", col_types  = col_uspc_types, 
                      na = c("", "NA", "NULL"))

## question 1
# dplyr
patent_organization_dplyr <- left_join(patent_assignee_tbl,assignee_tbl, by= c("assignee_id"="id"))
patent_organization_dplyr %>% filter(!(is.na(organization)) & type == 2) %>% group_by(organization) %>%
  summarise(number_of_patents = n()) %>% ungroup() %>% arrange(desc(number_of_patents)) %>% head(10)
# data.table
setDT(patent_assignee_tbl)
setDT(assignee_tbl)
patent_organization_dt <- patent_assignee_tbl[assignee_tbl, on= c("assignee_id"="id")]
patent_organization_dt[!(is.na(organization)) & type == 2, .N, by = organization][order(-N)] %>% head(10)

## question 2
# dplyr
patent_organization_date_dplyr <- left_join(patent_organization_dplyr, patent_tbl, by = c('patent_id' = 'id')) %>%
  select(1:5)
patent_organization_date_dplyr %>% filter(!(is.na(organization)) & month(date) == 8 & type == 2) %>%
  group_by(organization) %>% summarise(number_of_patents = n()) %>% ungroup() %>% arrange(desc(number_of_patents)) %>% head(10)
# data.table
setDT(patent_tbl)
patent_organization_date_dt <- patent_tbl[patent_organization_dt, on = c('id' = 'patent_id')][,!('num_claims')]
patent_organization_date_dt[!(is.na(organization)) & month(date) == 8 & type == 2, .N, by = organization][order(-N)] %>% head(10)

## question 3
# dplyr
patent_organization_class_dplyr <- left_join(patent_organization_dplyr,uspc_tbl, by = "patent_id",relationship = "many-to-many") %>% select(1:5)
patent_organization_class_dplyr <- patent_organization_class_dplyr %>% filter(!(is.na(organization))) %>% group_by(organization) %>%
  mutate(number_of_patents = n()) %>% ungroup()
top_organization_dplyr <- patent_organization_class_dplyr %>% group_by(organization) %>% slice(1) %>% ungroup() %>%
  arrange(desc(number_of_patents)) %>% head(10) %>% pull(organization)
patent_organization_class_dplyr %>% filter(organization %in% top_organization_dplyr & !(is.na(mainclass_id))) %>%
  group_by(mainclass_id) %>% summarise(number_of_incidents = n()) %>% ungroup() %>% arrange(desc(number_of_incidents)) %>% head(5)

# data.table
setDT(uspc_tbl)
patent_organization_class_dt <- uspc_tbl[patent_organization_dt, on = 'patent_id'][,!('sequence')]
patent_organization_class_dt <- patent_organization_class_dt[!(is.na(organization)), number_of_patents := .N, by = organization]
top_organization_dt <- patent_organization_class_dt[, .SD[1], by = organization][order(-number_of_patents)] %>% head(10) %>% pull(organization)
patent_organization_class_dt[!(is.na(mainclass_id)) & organization %in% top_organization_dt, .N, by = mainclass_id][order(-N)] %>% head(5)


