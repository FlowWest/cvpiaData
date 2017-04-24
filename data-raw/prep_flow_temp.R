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
  dplyr::select(-`CL date`) %>%
  tidyr::gather(watershed, flow, -date) %>%
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
  dplyr::select(watershed, year, month, flow)

devtools::use_data(flows, overwrite = TRUE)

# diversions-----------------------
diversion <- readxl::read_excel('data-raw/DSM_mapped.xlsx', sheet = 'Diversions Q0')
diversion$`DSM date` <- as.Date(diversion$`DSM date`, origin = '1899-12-30')
diversion$`CL date` <- as.Date(diversion$`CL date`, origin = '1899-12-30')

diversions <- diversion %>%
  dplyr::select(-`CL date`) %>%
  dplyr::rename(date = `DSM date`) %>%
  tidyr::gather(watershed, diversion, -date) %>%
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) %>%
  dplyr::left_join(flows) %>%
  dplyr::mutate(prop_diversion = diversion / flow) %>%
  dplyr::filter(!is.na(date)) %>%
  dplyr::select(watershed, year, month, diversion, prop_diversion)

devtools::use_data(diversions)

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

temperatures <- temperature %>%
  dplyr::mutate(month = lubridate::month(`DSM date`), year = lubridate::year(`DSM date`)) %>%
  dplyr::select(-`5Q date`, -`DSM date`) %>%
  tidyr::gather(watershed, temp, -year, -month) %>%
  dplyr::group_by(watershed, year, month) %>%
  dplyr::summarise(avg_temp = mean(temp)) %>%
  dplyr::mutate(avg_temp = (5/9) * (avg_temp - 32)) %>%
  dplyr::ungroup()

devtools::use_data(temperatures, overwrite = TRUE)

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


