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

# flows------------------------
flow <- readxl::read_excel('data-raw/DSM_mapped.xlsx', sheet = 'Flow Q0')
flow$`DSM date` <- as.Date(flow$`DSM date`, origin = '1899-12-30')
flow$`CL date` <- as.Date(flow$`CL date`, origin = '1899-12-30')

flows <- flow %>%
  dplyr::rename(date = `DSM date`) %>%
  dplyr::select(-`CL date`, -SC.Delta, -N.Delta) %>%
  tidyr::gather(watershed, flow, -date)

#delta inflow
delta_flows <- flow %>%
  dplyr::select(date = `DSM date`, SC.Delta, N.Delta) %>%
  tidyr::gather(watershed, flow, -date)

# diversions-----------------------
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

#delta diversion prop and total

delta_diversions <- diversion %>%
  dplyr::select(date = `DSM date`, N.Delta, SC.Delta)

delta_prop_diversions <- delta_diversions %>%
  tidyr::gather(watershed, diversion, -date) %>%
  dplyr::left_join(delta_flows) %>%
  dplyr::mutate(prop_diver = diversion/flow) %>%
  dplyr::select(date, watershed, prop_diver) %>%
  dplyr::filter(!is.na(date)) %>%
  tidyr::spread(watershed, prop_diver)

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
test <- temp %>%
  dplyr::ungroup() %>%
  dplyr::filter(!(watershed %in% c('SC.Delta', 'N.Delta')),
                year(date) >= '1970' & year(date) <= '1989') %>%
  tidyr::spread(date, avg_temp) %>%
  dplyr::left_join(watershed_ordering) %>%
  dplyr::arrange(order) %>%
  dplyr::select(-watershed, -order)


t1 <- create_DSM_array(test)

readr::write_rds(temp_spread, 'data/temperature.rds')

delta_temp <- temp %>%
  dplyr::filter(watershed %in% c('SC.Delta', 'N.Delta')) %>%
  tidyr::spread(watershed, avg_temp) %>%
  dplyr::select(date, SC.Delta,  N.Delta)

readr::write_rds(delta_temp, 'data/delta_temperature.rds')

#retQ
#october&november average flow
#?

#upsacQ
#montly average flow at upper sacramento
#?
flows %>%
  dplyr::filter(watershed == 'Upper Sacramento River') %>% View()

#degday
#sum daily mean tmep over oct and nov
#sum * 14/60 upper sac river
#sum * 7/60 everything else
temperature %>%
  dplyr::select(-`5Q date`, -`N.Delta`, -`SC.Delta`) %>%
  tidyr::gather(watershed, temperature, -`DSM date`) %>%
  dplyr::mutate(date = as.Date(`DSM date`)) %>%
  dplyr::filter(lubridate::month(date) %in% c(10, 11)) %>%
  dplyr::group_by(date, watershed) %>%
  dplyr::summarise(avg_temp = mean(temperature)) %>%
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::group_by(year, watershed) %>%
  dplyr::summarise(sum = sum(avg_temp)) %>%
  dplyr::mutate(degday = ifelse(watershed == 'Upper Sacramento River', sum * 14/60, sum * 7/60)) %>%
  dplyr::left_join(watershed_ordering) %>%
  dplyr::arrange(year, order) %>% View()
#zeros 16,17, 21, 22, 24, 31

#prop yolo
#yolo bypass flow/ lower sac flow
prop_yolo <- flow %>%
  dplyr::select(date = `DSM date`, `Yolo Bypass`, `Lower Sacramento River`) %>%
  dplyr::mutate(prop_Q = `Yolo Bypass` / `Lower Sacramento River`) %>%
  dplyr::select(date, prop_Q)

# sutter bypass not in model, use prop sutter mike urkov's spreadsheet
prop_sutter <- readr::read_csv('data-raw/bypass_flows.csv') %>%
  select(date, prop_Q = `Sutter Bypass as a percent of Sacramento`) %>%
  mutate(date = lubridate::mdy(date))

