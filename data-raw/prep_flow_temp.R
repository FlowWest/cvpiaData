library(readxl)
library(readr)
library(lubridate)

readxl::excel_sheets('data-raw/DSM_mapped.xlsx')

flow <- readxl::read_excel('data-raw/DSM_mapped.xlsx', sheet = 'Flow Q0')
flow$`DSM date` <- as.Date(flow$`DSM date`, origin = '1899-12-30')
flow$`CL date` <- as.Date(flow$`CL date`, origin = '1899-12-30')


diversion <- readxl::read_excel('data-raw/DSM_mapped.xlsx', sheet = 'Diversions Q0')

temperature <- readr::read_csv('data-raw/tempQ0.csv')
temperature$`DSM date` <- lubridate::mdy_hm(temperature$`DSM date`)
temperature$`5Q date` <- lubridate::mdy_hm(temperature$`5Q date`)
