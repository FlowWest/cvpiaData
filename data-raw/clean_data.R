
watershed_ordering <- readr::read_csv('data-raw/All inputs.csv') %>%
  dplyr::select(order = Order, watershed = Watershed)

devtools::use_data(watershed_ordering)

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

# add diversions-----------------------
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
  dplyr::select(watershed, year, month, flow, diversion, prop_diversion)

# baseline temperature data, calculate monthly mean by watershed in Celsius----
temperature <- readr::read_csv('data-raw/tempQ0.csv')
temperature$`DSM date` <- lubridate::mdy_hm(temperature$`DSM date`)
temperature$`5Q date` <- lubridate::mdy_hm(temperature$`5Q date`)

temperatures <- temperature %>%
  dplyr::mutate(month = lubridate::month(`DSM date`), year = lubridate::year(`DSM date`)) %>%
  dplyr::select(-`5Q date`, -`DSM date`) %>%
  tidyr::gather(watershed, temp, -year, -month) %>%
  dplyr::group_by(watershed, year, month) %>%
  dplyr::summarise(avg_temp = mean(temp, na.rm = TRUE)) %>%
  dplyr::mutate(avg_temp = (5/9) * (avg_temp - 32)) %>%
  dplyr::ungroup()

#bring in sutter flows from Calsims model
sutter_bypass <- readr::read_csv('data-raw/bypass_flows.csv')
sutter_flow <- sutter_bypass %>%
  dplyr::select(date, flow = `Sutter Bypass`) %>%
  magrittr::extract2(2)

monthly_reach_data <- diversions %>%
  dplyr::left_join(temperatures) %>%
  dplyr::mutate(flow = replace(flow, watershed == 'Sutter Bypass', sutter_flow))

devtools::use_data(monthly_reach_data)

# prop flow bypass----------------------------------
#prop yolo
#yolo bypass flow/ lower sac flow
prop_Q_yolo <- flow %>%
  dplyr::select(date = `DSM date`, `Yolo Bypass`, `Lower Sacramento River`) %>%
  dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date),
                prop_Q_yolo = `Yolo Bypass` / `Lower Sacramento River`) %>%
  dplyr::select(year, month, prop_Q_yolo)

# sutter bypass not in model, use prop sutter mike urkov's spreadsheet
prop_Q_sutter <- readr::read_csv('data-raw/bypass_flows.csv') %>%
  dplyr::select(date, prop_Q_sutter = `Sutter Bypass as a percent of Sacramento`) %>%
  dplyr::mutate(date = lubridate::mdy(date), year = lubridate::year(date), month = lubridate::month(date)) %>%
  dplyr::select(year, month, prop_Q_sutter)

prop_Q_bypass <- dplyr::left_join(prop_Q_yolo, prop_Q_sutter)

devtools::use_data(prop_Q_bypass)


# other flows------------------------------
#retQ - proportion flows at tributary junction coming from natal watershed
#october average flow
sheds <- watershed_ordering[[2]]

#create lookup vector for retQ denominators based on Jim's previous input
denominators <- c(rep(sheds[16], 16), NA, sheds[19], sheds[21], sheds[19],
                  sheds[21], NA, rep(sheds[24],2), sheds[25:27], rep(sheds[31],4))

names(denominators) <- sheds

dens <- flows %>%
  dplyr::filter(month == 10, watershed %in% unique(denominators)) %>%
  dplyr::rename(denominator = watershed, den_flow = flow)

return_flow <- flows %>%
  dplyr::filter(month == 10, watershed != 'N.Delta', watershed != 'SC.Delta') %>%
  dplyr::mutate(denominator = denominators[watershed]) %>%
  dplyr::left_join(dens) %>%
  dplyr::mutate(retQ = ifelse(flow / den_flow > 1, 1, flow / den_flow),
                retQ = replace(retQ, is.na(retQ), 0)) %>%
  dplyr::select(watershed, year, retQ)

devtools::use_data(return_flow, overwrite = TRUE)

#upsacQ
#montly average flow at upper sacramento


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
  dplyr::summarise(avg_temp = mean(temperature, na.rm = TRUE)) %>%
  dplyr::mutate(year = lubridate::year(date)) %>%
  dplyr::group_by(year, watershed) %>%
  dplyr::summarise(sum = sum(avg_temp)) %>%
  dplyr::mutate(degday = ifelse(watershed == 'Upper Sacramento River', sum * 14/60, sum * 7/60)) %>%
  dplyr::left_join(watershed_ordering) %>%
  dplyr::arrange(year, order) %>%   dplyr::ungroup() %>% View()
#zeros 16,17, 21, 22, 24, 31

# yolo and sutter(includes tisdale) overtopping
over <- read_csv('data-raw/sutter_yolo_weir_overtopping.csv',
                 col_types = cols(
                   col_character(),
                   col_double(),
                   col_double()
                 ) )
# flow in bypass oct-nov for adults is 1
#issue with bypass coalescing inappropriate
bypass_over_top <- over %>%
  tidyr::separate(month_year, c('month', 'year'), sep = ' ') %>%
  dplyr::filter(month %in% c('October', 'November')) %>%
  dplyr::mutate(sutter = ifelse(sutter > 0, 1, 0),
                yolo = ifelse(yolo > 0, 1, 0)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sutter = max(sutter), yolo = max(yolo))

devtools::use_data(bypass_over_top)

