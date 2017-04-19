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

flows <- flow %>%
  dplyr::rename(date = `DSM date`) %>%
  dplyr::select(-`CL date`, -SC.Delta, -N.Delta) %>%
  tidyr::gather(watershed, flow, -date)

diversion <- readxl::read_excel('data-raw/DSM_mapped.xlsx', sheet = 'Diversions Q0')
diversion$`DSM date` <- as.Date(diversion$`DSM date`, origin = '1899-12-30')
diversion$`CL date` <- as.Date(diversion$`CL date`, origin = '1899-12-30')

total_diversion <- diversion %>%
  dplyr::select(-N.Delta, -SC.Delta, -`CL date`) %>%
  dplyr::rename(date = `DSM date`)

prop_diversion <- total_diversion %>%
  tidyr::gather(watershed, diversion, -date) %>%
  dplyr::left_join(flows) %>%
  dplyr::mutate(prop_diver = diversion/flow) %>%
  dplyr::select(date, watershed, prop_diver) %>%
  dplyr::filter(!is.na(date)) %>%
  tidyr::spread(watershed, prop_diver) %>%
  dplyr::select(date, sort + 1)


# baseline temperature data, calculate monthly mean by watershed in Celsius----
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

# spread data to conform to DSM array input format
temp_spread <- temp %>%
  filter(!(watershed %in% c('SC.Delta', 'N.Delta'))) %>%
  spread(watershed, avg_temp) %>%
  select(date, sort + 1)

readr::write_rds(temp_spread, 'data/temperature.rds')

delta_temp <- temp %>%
  dplyr::filter(watershed %in% c('SC.Delta', 'N.Delta')) %>%
  tidyr::spread(watershed, avg_temp) %>%
  dplyr::select(date, SC.Delta,  N.Delta)

readr::write_rds(delta_temp, 'data/delta_temperature.rds')
