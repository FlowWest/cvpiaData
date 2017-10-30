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

prop_Q_dcc <- cvpiaFlow::propQdcc %>% 
  mutate(year = year(date), month = month(date)) %>% 
  select(-date) %>%
  filter(year >= 1980, year < 2000) %>% 
  spread(year, propQdcc) %>% 
  select(-month)

use_data(prop_Q_dcc)

returnQ <- cvpiaFlow::return_flow %>% 
  select(watershed, starts_with('198'), starts_with('199'))

use_data(returnQ, overwrite = TRUE)

upsac_flow <- cvpiaFlow::upsacQ %>% 
  mutate(year = year(date), month = month(date)) %>% 
  filter(year >= 1980, year < 2000) %>% 
  select(-date, -upsacQcfs) %>% 
  spread(year, upsacQcms) %>% 
  select(-month)

use_data(upsac_flow)


# yolo and sutter(includes tisdale) overtopping
# flow in bypass oct-nov for adults is 1
bypass_over_top <- read_csv('data-raw/sutter_yolo_weir_overtopping.csv') %>% 
  separate(month_year, c('month', 'year'), sep = ' ') %>%
  filter(month %in% c('October', 'November')) %>%
  mutate(sutter = ifelse(sutter > 0, 1, 0),
                yolo = ifelse(yolo > 0, 1, 0)) %>%
  group_by(year) %>%
  summarise(tils.ove = max(sutter), yolo.ovr = max(yolo)) %>% 
  filter(year >= 1980 & year <= 1989)

use_data(bypass_over_top, overwrite = TRUE)
