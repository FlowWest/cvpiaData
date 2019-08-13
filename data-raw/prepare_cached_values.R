library(tidyverse)
library(lubridate)
library(usethis)
library(cvpiaTemperature)
source('R/utils.R')


# using bypass node that is activated the most for meanQ
bypass <- cvpiaFlow::bypass_flows %>%
  select(date, `Sutter Bypass` = sutter4, `Yolo Bypass` = yolo2) 

meanQ <- cvpiaFlow::flows_cfs %>%
  left_join(bypass) %>% 
  filter(between(year(date), 1980, 1999)) %>% 
  gather(watershed, flow_cfs, -date) %>% 
  filter(watershed != 'Lower-mid Sacramento River1') %>% 
  mutate(flow_cms = cvpiaFlow::cfs_to_cms(flow_cfs),
         watershed = ifelse(watershed == 'Lower-mid Sacramento River2', 'Lower-mid Sacramento River', watershed)) %>% 
  select(-flow_cfs) %>% 
  spread(date, flow_cms) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>%  
  create_SIT_array()

use_data(meanQ)
  
prop_diversion <- cvpiaFlow::proportion_diverted %>%
  filter(year(date) >= 1980, year(date) < 2000) %>% 
  gather(watershed, prop_diver, -date) %>% 
  mutate(prop_diver = ifelse(is.na(prop_diver), 0, prop_diver)) %>% 
  spread(date, prop_diver) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

use_data(prop_diversion, overwrite = TRUE)

total_diversion <- cvpiaFlow::total_diverted %>%   
  filter(year(date) >= 1980, year(date) < 2000) %>% 
  gather(watershed, tot_diver, -date) %>% 
  mutate(tot_diver = ifelse(is.na(tot_diver), 0, tot_diver)) %>% 
  spread(date, tot_diver) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

use_data(total_diversion, overwrite = TRUE)

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
  mutate(retQ = ifelse(is.na(retQ), 0, retQ)) %>% 
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

usethis::use_data(dlt_divers)

# delta total diversions
dl_tot_div <- cvpiaFlow::delta_flows %>% 
  filter(year(date) >= 1980, year(date) <= 1999) %>% 
  select(date, n_dlt_div_cms, s_dlt_div_cms) %>% 
  gather(delta, tot_div, -date) %>% 
  spread(date, tot_div)

dlt_divers_tot <- array(NA, dim = c(12, 20, 2))
dlt_divers_tot[ , , 1] <- as.matrix(dl_tot_div[1, -1])
dlt_divers_tot[ , , 2] <- as.matrix(dl_tot_div[2, -1])

usethis::use_data(dlt_divers_tot)

# delta inflows
dl_inflow <- cvpiaFlow::delta_flows %>% 
  filter(year(date) >= 1980, year(date) <= 1999) %>% 
  select(date, n_dlt_inflow_cms, s_dlt_inflow_cms) %>% 
  gather(delta, inflow, -date) %>% 
  spread(date, inflow)

dlt_inflow <- array(NA, dim = c(12, 20, 2))
dlt_inflow[ , , 1] <- as.matrix(dl_inflow[1, -1])
dlt_inflow[ , , 2] <- as.matrix(dl_inflow[2, -1])
  
usethis::use_data(dlt_inflow)

# flow at freeport 
freeportQcms <- cvpiaFlow::freeportQ %>% 
  mutate(year = year(date), month = month(date)) %>% 
  filter(year >= 1980, year <= 1999) %>% 
  select(-date, -freeportQcfs) %>%
  spread(year, freeportQcms) %>% 
  select(-month) 

usethis::use_data(freeportQcms)

cross_channel_gates <- cvpiaFlow::delta_cross_channel_closed
use_data(cross_channel_gates)

degday <- cvpiaTemperature::deg_days %>%
  mutate(degdays = ifelse(is.na(degdays), 0, degdays)) %>% 
  spread(date, degdays) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

use_data(degday, overwrite = TRUE)

ptemp20mc <- cvpiaTemperature::prop_temp_over_20_migr_cor %>%
  mutate(median_p20 = ifelse(is.na(median_p20), 0, median_p20)) %>% 
  spread(month, median_p20)

use_data(ptemp20mc, overwrite = TRUE)

dt_tmps <- cvpiaTemperature::delta_temps %>% 
  filter(between(year(date), 1980, 1999)) %>% 
  spread(date, monthly_mean_temp_c)

dlt_temps <- array(NA, dim = c(12, 20, 2))
dlt_temps[ , , 1] <- as.matrix(dt_tmps[1, -1])
dlt_temps[ , , 2] <- as.matrix(dt_tmps[2, -1])

usethis::use_data(dlt_temps, overwrite = TRUE)

rearing_temps <- cvpiaTemperature::juv_temp %>% 
  spread(date, monthly_mean_temp_c) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

usethis::use_data(rearing_temps, overwrite = TRUE)

egg_temp_effect <- read_csv('data-raw/egg2fry_temp.csv') %>% 
  mutate(mean_temp_effect = (Dry + Wet)/2) %>% 
  select(watershed = Watershed.full, mean_temp_effect)

usethis::use_data(egg_temp_effect)


dt_hab <- cvpiaHabitat::delta_habitat %>% 
  filter(between(year(date), 1980, 1999)) %>% 
  gather(delta, hab_area, -date) %>% 
  spread(date, hab_area)

dlt_hab <- array(NA, dim = c(12, 20, 2))
dlt_hab[ , , 1] <- as.matrix(dt_hab[1, -1])
dlt_hab[ , , 2] <- as.matrix(dt_hab[2, -1])

use_data(dlt_hab)

misc_delta <- data.frame(
  delta = c('North Delta', 'South Delta'),
  High.pred = c(1, 1),
  contct.pts = c(718, 1437)
)

usethis::use_data(misc_delta)

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

prop_pulse[is.na(prop_pulse)] <- 0

# prop_pulse <- array(0, dim = c(31, 12, 20))
usethis::use_data(prop_pulse, overwrite = TRUE)

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

usethis::use_data(med_flow)

# pools-------------
pools <- cvpiaHabitat::pools

use_data(pools)

has_spring_run <- data.frame(
  watershed = cvpiaHabitat::modeling_exist$Watershed,
  has_spring_run = !is.na(cvpiaHabitat::modeling_exist$SR_juv))[-32, ]

usethis::use_data(has_spring_run)


# New temperature inputs 

# proportion of month that temps > 20 based on average monthly temp in streams

inv.logit<-function(eta){1/(1+exp(-eta))}

aveT20 <- juv_temp %>% 
  transmute(
    date, 
    watershed,
    aveT20 = inv.logit(-8.9836+ 0.4818*monthly_mean_temp_c)
  ) %>% 
  spread(date, aveT20) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

usethis::use_data(aveT20, overwrite = TRUE)

aveT20D <- delta_temps %>% 
  transmute(
    date, 
    watershed, 
    aveT20D = inv.logit(-18.11910 + 0.94687*monthly_mean_temp_c)  
  ) %>% 
  spread(date, aveT20D) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array() 
  

usethis::use_data(aveT20D, overwrite = TRUE)

maxT24 <- juv_temp %>% 
  transmute(
    date, 
    watershed,
    maxT24 = inv.logit(-22.3888+ 1.4385* monthly_mean_temp_c)
  ) %>% 
  spread(date, maxT24) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

usethis::use_data(maxT24, overwrite = TRUE)

maxT29 <- juv_temp %>% 
  transmute(
    date, 
    watershed, 
    maxT29 = inv.logit(-18.9101+ 1.0058*monthly_mean_temp_c)
  ) %>% 
  spread(date, maxT29) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

usethis::use_data(maxT29, overwrite = TRUE)
