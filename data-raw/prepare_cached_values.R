library(tidyverse)
library(lubridate)
library(usethis)
library(cvpiaTemperature)
source('R/utils.R')

dt_hab <- cvpiaHabitat::delta_habitat %>% 
  filter(between(year(date), 1980, 2000)) %>% 
  gather(delta, hab_area, -date) %>% 
  spread(date, hab_area)

dlt_hab <- array(NA, dim = c(12, 21, 2))
dlt_hab[ , , 1] <- as.matrix(dt_hab[1, -1])
dlt_hab[ , , 2] <- as.matrix(dt_hab[2, -1])

use_data(dlt_hab, overwrite = TRUE)

misc_delta <- data.frame(
  delta = c('North Delta', 'South Delta'),
  High.pred = c(1, 1),
  contct.pts = c(718, 1437)
)

usethis::use_data(misc_delta)

# pools-------------
pools <- cvpiaHabitat::pools

use_data(pools)

has_spring_run <- data.frame(
  watershed = cvpiaHabitat::modeling_exist$Watershed,
  has_spring_run = !is.na(cvpiaHabitat::modeling_exist$SR_juv))[-32, ]

usethis::use_data(has_spring_run)


# temperature proportions 

# proportion of month that temps > 20 based on average monthly temp in streams

inv.logit<-function(eta){1/(1+exp(-eta))}

aveT20 <- juv_temp %>% 
  transmute(
    date, 
    watershed,
    aveT20 = inv.logit(-8.9836 + 0.4818 * monthly_mean_temp_c)
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
    aveT20D = inv.logit(-18.11910 + 0.94687 * monthly_mean_temp_c)  
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
    maxT24 = inv.logit(-22.3888 + 1.4385 * monthly_mean_temp_c)
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
    maxT29 = inv.logit(-18.9101 + 1.0058 * monthly_mean_temp_c)
  ) %>% 
  spread(date, maxT29) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  arrange(order) %>% 
  select(-watershed, -order) %>% 
  create_SIT_array()

usethis::use_data(maxT29, overwrite = TRUE)
