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

# bypass flows for rearing habitat
bp_pf <- cvpiaFlow::propQbypass %>% 
  select(-propQyolo, -propQsutter) %>% 
  filter(between(year(date), 1980, 1999)) %>% 
  gather(bypass, flow, -date) %>% 
  spread(date, flow)

bypass_prop_Q <- array(NA, dim = c(12, 20, 6))
for (i in 1:6) {
  bypass_prop_Q[ , , i] <- as.matrix(bp_pf[i, -1])
}

use_data(bypass_prop_Q)

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
# bypass_over_top <- 
bpo <- cvpiaFlow::bypass_overtopped %>% 
  gather(bypass, overtopped, -date) %>% 
  spread(date, overtopped) 

bypass_over <- array(NA, dim = c(12, 21, 2))
bypass_over[ , , 1] <- as.matrix(bpo[1, -1])
bypass_over[ , , 2] <- as.matrix(bpo[2, -1])

use_data(bypass_over, overwrite = TRUE)

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

dt_tmps <- cvpiaTemperature::delta_temps %>% 
  filter(between(year(date), 1980, 1999)) %>% 
  spread(date, monthly_mean_temp_c)

dlt_temps <- array(NA, dim = c(12, 20, 2))
dlt_temps[ , , 1] <- as.matrix(dt_tmps[1, -1])
dlt_temps[ , , 2] <- as.matrix(dt_tmps[2, -1])

devtools::use_data(dlt_temps)

rearing_temps <- cvpiaTemperature::juv_temp %>% 
  spread(date, monthly_mean_temp_c) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

devtools::use_data(rearing_temps)

egg_temp_effect <- read_csv('data-raw/egg2fry_temp.csv') %>% 
  mutate(mean_temp_effect = (Dry + Wet)/2) %>% 
  select(watershed = Watershed.full, mean_temp_effect)

devtools::use_data(egg_temp_effect)


dt_hab <- cvpiaHabitat::delta_habitat %>% 
  filter(between(year(date), 1980, 1999)) %>% 
  gather(delta, hab_area, -date) %>% 
  spread(date, hab_area)

dlt_hab <- array(NA, dim = c(12, 20, 2))
dlt_hab[ , , 1] <- as.matrix(dt_hab[1, -1])
dlt_hab[ , , 2] <- as.matrix(dt_hab[2, -1])

use_data(dlt_hab)

# inchannel habitat ----------------
inchannel_fry_fall <- cvpiaData::inchannel_habitat %>% 
  filter(species == 'fr', life_stage == 'fry') %>% 
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>% 
  select(date, watershed, habitat) %>% 
  spread(date, habitat) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

use_data(inchannel_fry_fall)

inchannel_juv_fall <- cvpiaData::inchannel_habitat %>% 
  filter(species == 'fr', life_stage == 'juv') %>% 
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>% 
  select(date, watershed, habitat) %>% 
  spread(date, habitat) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

use_data(inchannel_juv_fall)

spawn_fall <- cvpiaData::inchannel_spawning_habitat %>% 
  filter(species == 'fr') %>% 
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>% 
  select(date, watershed, habitat) %>% 
  spread(date, habitat) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

use_data(spawn_fall)

floodplain_fall <- cvpiaData::floodplain_habitat %>% 
  filter(species == 'fr') %>% 
  mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>%
  select(date, watershed, habitat) %>% 
  spread(date, habitat) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

devtools::use_data(floodplain_fall)

misc_delta <- data.frame(
  delta = c('North Delta', 'South Delta'),
  High.pred = c(1, 1),
  contct.pts = c(718, 1437)
)

devtools::use_data(misc_delta)

byp <- as.data.frame(matrix(as.numeric(NA), nrow = 2, ncol = 13))
names(byp) <- c('watershed', as.character(1:12))
byp$watershed <- c('Yolo Bypass', 'Sutter Bypass')

prop_pulse <- cvpiaFlow::flows_cfs %>%
  filter(between(year(date), 1980, 1999)) %>% 
  mutate(`Lower-mid Sacramento River` = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 *`Lower-mid Sacramento River2`) %>% 
  select(-`Lower-mid Sacramento River1`, -`Lower-mid Sacramento River2`) %>% 
  gather(watershed, flow, -date) %>% 
  group_by(month = month(date), watershed) %>% 
  summarise(prop_pulse = sd(flow)/median(flow)) %>% 
  mutate(prop_pulse = replace(prop_pulse, is.infinite(prop_pulse), 0)) %>% 
  select(month, watershed, prop_pulse) %>% 
  spread(month, prop_pulse) %>% 
  bind_rows(byp) %>%
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-order) 

# prop_pulse <- array(0, dim = c(31, 12, 20))
devtools::use_data(prop_pulse, overwrite = TRUE)

# median flow
med_flow <- cvpiaFlow::flows_cfs %>%
  filter(between(year(date), 1980, 1999)) %>% 
  mutate(`Lower-mid Sacramento River` = 35.6/58 * `Lower-mid Sacramento River1` + 22.4/58 *`Lower-mid Sacramento River2`) %>% 
  select(-`Lower-mid Sacramento River1`, -`Lower-mid Sacramento River2`) %>% 
  gather(watershed, flow, -date) %>% 
  group_by(month = month(date), watershed) %>% 
  summarise(median_flow = median(flow)) %>% 
  # mutate(prop_pulse = replace(prop_pulse, is.infinite(prop_pulse), 0)) %>% 
  select(month, watershed, median_flow) %>% 
  spread(month, median_flow) %>% 
  bind_rows(byp) %>%
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-order) 

devtools::use_data(med_flow)

# 
# upmidsac_flow <- dataRetrieval::readNWISdv(siteNumbers = '11390500', parameterCd = '00060', 
#                                            startDate = '1988-01-01', endDate = '1999-12-31')
# 
# meas <- upmidsac_flow %>% 
#   select(Date, flow = X_00060_00003) %>% 
#   mutate(year = year(Date), month = month(Date)) %>% 
#   group_by(month, year) %>% 
#   summarise(prop_pulse = sd(flow)/median(flow)) %>% 
#   ungroup()
# 
# month_mod <- cvpiaFlow::flows_cfs %>% 
#   filter(between(year(date), 1988, 1999)) %>% 
#   gather(watershed, flow, -date) %>% 
#   filter(watershed == 'Upper-mid Sacramento River') %>% 
#   group_by(month = month(date), watershed) %>% 
#   summarise(prop_pulse_month = sd(flow)/median(flow)) %>% 
#   select(-watershed)
# 
# year_mod <- cvpiaFlow::flows_cfs %>% 
#   filter(between(year(date), 1988, 1999)) %>% 
#   gather(watershed, flow, -date) %>% 
#   filter(watershed == 'Upper-mid Sacramento River') %>% 
#   group_by(year = year(date)) %>% 
#   mutate(sd = sd(flow)) %>% 
#   group_by(month = month(date)) %>% 
#   mutate(med = median(flow)) %>% 
#   ungroup() %>% 
#   mutate(prop_pulse_year = sd/med) %>% 
#   select(year, month, prop_pulse_year) 
# 
# meas %>% 
#   left_join(year_mod) %>% 
#   left_join(month_mod) %>% 
#   # ggplot(aes(x = prop_pulse, y = prop_pulse_year)) +
#   ggplot(aes(x = prop_pulse, y = 1/ (1 + exp(prop_pulse_month)))) +
#   geom_point()
# 
# t1 <- meas %>% 
#   left_join(year_mod) %>% 
#   left_join(month_mod) 
# 
# cor(t1$prop_pulse, t1$prop_pulse_year)
# cor(t1$prop_pulse, t1$prop_pulse_month)
