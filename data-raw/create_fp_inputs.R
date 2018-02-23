library(tidyverse)
library(purrr)
library(lubridate)

# flow filtered to time frame of interest-----------
flz <- cvpiaFlow::flows_cfs %>% 
  filter(between(year(date), 1980, 1999)) %>% 
  gather(watershed, flow, -date)

# helper function to get weeks inundated at all flows during period for watershed------
all_wi <- function(ws) {
  tibble(
    date = seq(as.Date('1980-01-01'), as.Date('1999-12-01'), by = 'month'),
    duration = map_dbl(filter(flz, watershed == ws)$flow, function(flows) {
      cvpiaHabitat::weeks_flooded(ws, flows)  
    }),
    watershed = ws
  )
}

#
all_wi('Sutter Bypass')

durations <- tibble()

# exclude bypasses and 'Lower-mid Sacramento River'
ws <- cvpiaData::watershed_ordering$watershed[c(-17, -21, -22)]

for (watershed in ws) {
  print(paste('start', watershed))
  durations <- bind_rows(durations, all_wi(watershed))
  print(paste('end', watershed))
}

cvpiaHabitat::weeks_inundated %>% 
  filter(watershed == 'Deer Creek')

inundation_durations <- durations %>% 
  rbind(
    tibble(
      date = seq(as.Date('1980-01-01'), as.Date('1999-12-01'), by = 'month'),
      `Sutter Bypass` = 2,
      `Yolo Bypass` = 2,
      `Lower-mid Sacramento River` = 2
    ) %>% 
      gather(watershed, duration, -date)
  ) %>% 
  spread(date, duration) %>% 
  left_join(cvpiaData::watershed_ordering) %>%
  arrange(order) %>% 
  select(-order, -watershed) %>% 
  create_SIT_array()
  

devtools::use_data(inundation_durations, overwrite = TRUE)

