library(tidyverse)
library(rvest)
library(stringr)
library(tibble)
library(httr)
library(jsonlite)
library(purrr)
library(glue)
library(xopen)

# radon-bikes.de

## mountain E-bikes
url <- 'https://www.radon-bikes.de/e-bike/mountainbike/'
html <- read_html(url)
### model
mountain_ebike <- html %>% html_nodes(css = ".row > h2") %>%
  html_text() %>% as_tibble()
### price
mountain_ebike <- mountain_ebike %>% mutate(new_col=(html %>% html_nodes(css = ".currency_eur .m-serienpanel__price--active") %>% 
  html_text() %>% str_remove_all(' €')))
### column names
mountain_ebike <- mountain_ebike %>% set_names(c('model', 'price'))
mountain_ebike <- mountain_ebike %>% mutate(E_bike_type = 'mountain', .before = 1)

## trekking Ebikes
url2 <- 'https://www.radon-bikes.de/e-bike/trekking/'
html2 <- read_html(url2)
### model
trekking_ebike <- html2 %>% html_nodes(css = ".row > h2") %>%
  html_text() %>% as_tibble()
### price
trekking_ebike <- trekking_ebike %>% mutate(new_col=(html2 %>% html_nodes(css = ".currency_eur .m-serienpanel__price--active") %>% 
  html_text() %>% str_remove_all(' €')))
### column names
trekking_ebike <- trekking_ebike %>% set_names(c('model', 'price'))
trekking_ebike <- trekking_ebike %>% mutate(E_bike_type = 'trekking', .before = 1)

#### merging two tables
ebike_radon <- bind_rows(mountain_ebike, trekking_ebike)
print(ebike_radon, n=36)



url3 <- "https://api-football-v1.p.rapidapi.com/v3/timezone"

response <- VERB("GET", url, add_headers('X-RapidAPI-Key' = 'e7af728c11mshdfa2eeea5a618c0p1a01f2jsnaf586a5b5b25', 'X-RapidAPI-Host' = 'api-football-v1.p.rapidapi.com'), content_type("application/octet-stream"))

content(response, "text")





















