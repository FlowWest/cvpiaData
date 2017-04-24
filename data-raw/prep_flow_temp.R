# library(readxl)
# library(readr)
# library(lubridate)
# library(tidyr)
# library(dplyr)
# library(magrittr)


watershed_ordering <- readr::read_csv('data-raw/All inputs.csv') %>%
  dplyr::select(order = Order, watershed = Watershed) %>%
  arrange(watershed) %>%
  dplyr::mutate(sort = 1:31) %>%
  dplyr::arrange(order)

devtools::use_data(watershed_ordering)

# create column sorting vector
sort <- watershed_ordering$sort

readxl::excel_sheets('data-raw/DSM_mapped.xlsx')

# flows------------------------
flow <- readxl::read_excel('data-raw/DSM_mapped.xlsx', sheet = 'Flow Q0')
flow$`DSM date` <- as.Date(flow$`DSM date`, origin = '1899-12-30')
flow$`CL date` <- as.Date(flow$`CL date`, origin = '1899-12-30')

flows <- flow %>%
  dplyr::rename(date = `DSM date`) %>%
  dplyr::select(-`CL date`, -SC.Delta, -N.Delta) %>%
  tidyr::gather(watershed, flow, -date)

devtools::use_data(flows, overwrite = TRUE)

#delta inflow
delta_flows <- flow %>%
  dplyr::select(date = `DSM date`, SC.Delta, N.Delta) %>%
  tidyr::gather(watershed, flow, -date)

devtools::use_data(delta_flows)

# diversions-----------------------
diversion <- readxl::read_excel('data-raw/DSM_mapped.xlsx', sheet = 'Diversions Q0')
diversion$`DSM date` <- as.Date(diversion$`DSM date`, origin = '1899-12-30')
diversion$`CL date` <- as.Date(diversion$`CL date`, origin = '1899-12-30')

total_diversion <- diversion %>%
  dplyr::select(-N.Delta, -SC.Delta, -`CL date`) %>%
  dplyr::rename(date = `DSM date`)

devtools::use_data(total_diversion)

prop_diversion <- total_diversion %>%
  tidyr::gather(watershed, diversion, -date) %>%
  dplyr::left_join(flows) %>%
  dplyr::mutate(prop_diver = diversion/flow) %>%
  dplyr::select(date, watershed, prop_diver) %>%
  dplyr::filter(!is.na(date)) %>%
  tidyr::spread(watershed, prop_diver) %>%
  dplyr::select(date, sort + 1)

devtools::use_data(prop_diversion)

#delta diversion prop and total
delta_diversions <- diversion %>%
  dplyr::select(date = `DSM date`, N.Delta, SC.Delta)

devtools::use_data(delta_diversions)

delta_prop_diversions <- delta_diversions %>%
  tidyr::gather(watershed, diversion, -date) %>%
  dplyr::left_join(delta_flows) %>%
  dplyr::mutate(prop_diver = diversion/flow) %>%
  dplyr::select(date, watershed, prop_diver) %>%
  dplyr::filter(!is.na(date)) %>%
  tidyr::spread(watershed, prop_diver)

devtools::use_data(delta_prop_diversions)

# prop flow bypass----------------------------------
#prop yolo
#yolo bypass flow/ lower sac flow
prop_Q_yolo <- flow %>%
  dplyr::select(date = `DSM date`, `Yolo Bypass`, `Lower Sacramento River`) %>%
  dplyr::mutate(prop_Q = `Yolo Bypass` / `Lower Sacramento River`) %>%
  dplyr::select(date, prop_Q)

devtools::use_data(prop_Q_yolo)

# sutter bypass not in model, use prop sutter mike urkov's spreadsheet
prop_Q_sutter <- readr::read_csv('data-raw/bypass_flows.csv') %>%
  select(date, prop_Q = `Sutter Bypass as a percent of Sacramento`) %>%
  mutate(date = lubridate::mdy(date))

devtools::use_data(prop_Q_sutter)

# other flows------------------------------
#retQ
#october&november average flow
#?

#upsacQ
#montly average flow at upper sacramento
#?
flows %>%
  dplyr::filter(watershed == 'Upper Sacramento River') %>% View()

# baseline temperature data, calculate monthly mean by watershed in Celsius----
temperature <- readr::read_csv('data-raw/tempQ0.csv')
temperature$`DSM date` <- lubridate::mdy_hm(temperature$`DSM date`)
temperature$`5Q date` <- lubridate::mdy_hm(temperature$`5Q date`)

temp <- temperature %>%
  dplyr::mutate(month = lubridate::month(`DSM date`), year = lubridate::year(`DSM date`)) %>%
  dplyr::select(-`5Q date`, -`DSM date`) %>%
  tidyr::gather(watershed, temp, -year, -month) %>%
  dplyr::group_by(watershed, year, month) %>%
  dplyr::summarise(avg_temp = mean(temp)) %>%
  dplyr::mutate(avg_temp = (5/9) * (avg_temp - 32)) %>%
  dplyr::ungroup()

temperatures <- temp %>%
  dplyr::filter(!(watershed %in% c('SC.Delta', 'N.Delta')))

devtools::use_data(temperatures, overwrite = TRUE)

delta_temperatures <- temp %>%
  dplyr::filter(watershed %in% c('SC.Delta', 'N.Delta'))

devtools::use_data(delta_temperatures, overwrite = TRUE)

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
  dplyr::arrange(year, order) %>%   dplyr::ungroup() %>% View()
#zeros 16,17, 21, 22, 24, 31


