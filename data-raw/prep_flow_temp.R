library(readxl)
library(readr)
library(lubridate)
library(tidyr)
library(dplyr)
library(magrittr)


watershed_ordering <- readr::read_csv('data-raw/All inputs.csv') %>%
  dplyr::select(order = Order, watershed = Watershed)

# create column sorting vector
sort <- watershed_ordering %>%
  arrange(watershed) %>%
  dplyr::mutate(sort = 1:31) %>%
  dplyr::arrange(order) %>%
  magrittr::extract2(3)


readxl::excel_sheets('data-raw/DSM_mapped.xlsx')

flow <- readxl::read_excel('data-raw/DSM_mapped.xlsx', sheet = 'Flow Q0')
flow$`DSM date` <- as.Date(flow$`DSM date`, origin = '1899-12-30')
flow$`CL date` <- as.Date(flow$`CL date`, origin = '1899-12-30')


diversion <- readxl::read_excel('data-raw/DSM_mapped.xlsx', sheet = 'Diversions Q0')
diversion$`DSM date` <- as.Date(diversion$`DSM date`, origin = '1899-12-30')
diversion$`CL date` <- as.Date(diversion$`CL date`, origin = '1899-12-30')

# baseline temperature data, calculate monthly mean by watershed in Celsius
temperature <- readr::read_csv('data-raw/tempQ0.csv')
temperature$`DSM date` <- lubridate::mdy_hm(temperature$`DSM date`)
temperature$`5Q date` <- lubridate::mdy_hm(temperature$`5Q date`)

temp <- temperature %>%
  dplyr::mutate(date = lubridate::floor_date(`DSM date`, unit = 'day')) %>%
  dplyr::select(-`DSM date`, -`5Q date`) %>%
  tidyr::gather(watershed, temperature, - date) %>%
  dplyr::mutate(date = lubridate::floor_date(date, unit = 'month')) %>%
  dplyr::group_by(watershed, date) %>%
  dplyr::summarise(avg_temp = mean(temperature)) %>%
  dplyr::mutate(avg_temp = (5/9) * (avg_temp - 32))

# spread data to conform to DSM model input
temp_spread <- temp %>%
  filter(!(watershed %in% c('SC.Delta', 'N.Delta'))) %>%
  spread(watershed, avg_temp) %>%
  select(date, sort + 1)

readr::write_rds(temp_spread, 'data/temperature.rds')
