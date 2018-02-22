library(tidyverse)
library(lubridate)
library(devtools)

source('R/utils.R')

prop_diversion <- cvpiaFlow::proportion_diverted %>%
  filter(year(date) >= 1980, year(date) < 2000) %>% 
  gather(watershed, prop_diver, -date) %>% 
  spread(date, prop_diver) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

use_data(prop_diversion)

total_diversion <- cvpiaFlow::total_diverted %>%   
  filter(year(date) >= 1980, year(date) < 2000) %>% 
  gather(watershed, tot_diver, -date) %>% 
  spread(date, tot_diver) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

use_data(total_diversion)

prop_Q_sutter <- cvpiaFlow::propQbypass %>%
  mutate(year = year(date), month = month(date)) %>% 
  select(year, month, propQsutter) %>%
  filter(year >= 1980, year < 2000) %>% 
  spread(year, propQsutter) %>% 
  select(-month)

use_data(prop_Q_sutter)


prop_Q_yolo <- cvpiaFlow::propQbypass %>%
  mutate(year = year(date), month = month(date)) %>% 
  select(year, month, propQyolo) %>%
  filter(year >= 1980, year < 2000) %>% 
  spread(year, propQyolo) %>% 
  select(-month)

use_data(prop_Q_yolo)

returnQ <- cvpiaFlow::return_flow %>%
  mutate(year = year(date)) %>% 
  filter(year >= 1979, year <= 1998) %>% 
  select(watershed, year, retQ) %>% 
  spread(year, retQ) %>% 
  left_join(cvpiaData::watershed_ordering) %>%
  arrange(order) %>% 
  select(-order)

use_data(returnQ, overwrite = TRUE)

upsac_flow <- cvpiaFlow::upsacQ %>% 
  mutate(year = year(date), month = month(date)) %>% 
  filter(year >= 1980, year < 2000) %>% 
  select(-date, -upsacQcfs) %>% 
  spread(year, upsacQcms) %>% 
  select(-month) 

use_data(upsac_flow)

d <- 1:12
names(d) <- month.name

# yolo and sutter(includes tisdale) overtopping
# flow in bypass for adults is 1
bypass_over_top <- read_csv('data-raw/sutter_yolo_weir_overtopping.csv') %>%
  separate(month_year, c('month', 'year'), sep = ' ') %>%
  mutate(sutter = ifelse(sutter > 0, 1, 0),
                yolo = ifelse(yolo > 0, 1, 0),
         month = d[month]) %>%
  filter(year >= 1979 & year <= 1999) %>% 
  arrange(year, month)

use_data(bypass_over_top, overwrite = TRUE)

# delta-----------
# delta prop diverted
dl_prop_div <- cvpiaFlow::delta_flows %>% 
  filter(year(date) >= 1980, year(date) <= 1999) %>% 
  select(date, n_dlt_prop_div, s_dlt_prop_div) %>% 
  gather(delta, prop_div, -date) %>% 
  spread(date, prop_div)

dlt_divers <- array(NA, dim = c(12, 20, 2))
dlt_divers[ , , 1] <- as.matrix(dl_prop_div[1, -1])
dlt_divers[ , , 2] <- as.matrix(dl_prop_div[2, -1])

devtools::use_data(dlt_divers)

# delta total diversions
dl_tot_div <- cvpiaFlow::delta_flows %>% 
  filter(year(date) >= 1980, year(date) <= 1999) %>% 
  select(date, n_dlt_div_cms, s_dlt_div_cms) %>% 
  gather(delta, tot_div, -date) %>% 
  spread(date, tot_div)

dlt_divers_tot <- array(NA, dim = c(12, 20, 2))
dlt_divers_tot[ , , 1] <- as.matrix(dl_tot_div[1, -1])
dlt_divers_tot[ , , 2] <- as.matrix(dl_tot_div[2, -1])

devtools::use_data(dlt_divers_tot)

# delta inflows
dl_inflow <- cvpiaFlow::delta_flows %>% 
  filter(year(date) >= 1980, year(date) <= 1999) %>% 
  select(date, n_dlt_inflow_cms, s_dlt_inflow_cms) %>% 
  gather(delta, inflow, -date) %>% 
  spread(date, inflow)

dlt_inflow <- array(NA, dim = c(12, 20, 2))
dlt_inflow[ , , 1] <- as.matrix(dl_inflow[1, -1])
dlt_inflow[ , , 2] <- as.matrix(dl_inflow[2, -1])
  
devtools::use_data(dlt_inflow)

# flow at freeport 
freeportQcms <- cvpiaFlow::freeportQ %>% 
  mutate(year = year(date), month = month(date)) %>% 
  filter(year >= 1980, year <= 1999) %>% 
  select(-date, -freeportQcfs) %>%
  spread(year, freeportQcms) %>% 
  select(-month) 

devtools::use_data(freeportQcms)

cross_channel_gates <- cvpiaFlow::delta_cross_channel_closed
use_data(cross_channel_gates)

degday <- cvpiaTemperature::deg_days %>%
  spread(date, degdays) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

use_data(degday)

ptemp20mc <- cvpiaTemperature::prop_temp_over_20_migr_cor %>% 
  spread(month, median_p20)

use_data(ptemp20mc)
