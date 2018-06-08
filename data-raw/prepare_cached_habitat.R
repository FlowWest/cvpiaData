library(tidyr)
library(dplyr)
library(purrr)
library(lubridate)
library(cvpiaHabitat)

source('R/utils.R')

# functions ---------
get_flow <- function(watershed, years=c(1980, 1999)) {
  
    # get the flow values at the dates
    dplyr::pull(dplyr::filter(dplyr::select(cvpiaFlow::flows_cfs, date, watershed), 
                                             lubridate::year(date) >= years[1], 
                                             lubridate::year(date) <= years[2]), 2)
}

get_rear_hab_all <- function(watersheds, species, life_stage) {
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed)
    habitat <- cvpiaHabitat::set_instream_habitat(watershed, 
                                                  species = species,
                                                  life_stage = life_stage, 
                                                  flow = flows)
    tibble(
      year = rep(1980:1999, each = 12),
      month = rep(1:12, 20),
      watershed = watershed, 
      hab_sq_m = habitat)
  })
  
  # deal with sacramento special cases
  # lower-mid sac
  low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1')
  low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2')
  
  low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
    cvpiaHabitat::set_instream_habitat('Lower-mid Sacramento River', 
                                       species = species, 
                                       life_stage = life_stage, 
                                       flow = flow, flow2 = flow2)
  })
  
  low_mid_sac <- tibble(
    year = rep(1980:1999, each = 12),
    month = rep(1:12, 20),
    watershed = 'Lower-mid Sacramento River', 
    hab_sq_m = low_mid_sac_hab)
  
  hab <- bind_rows(most, low_mid_sac) %>% 
    spread(watershed, hab_sq_m) %>% 
    bind_cols(tibble(`Sutter Bypass` = rep(NA, 240),
                     `Yolo Bypass` = rep(NA, 240))) %>% 
    gather(watershed, habitat, -year, -month) %>% 
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>% 
    select(date, watershed, habitat) %>% 
    spread(date, habitat) %>% 
    left_join(cvpiaData::watershed_ordering) %>% 
    arrange(order) %>% 
    select(-watershed, -order) %>% 
    create_SIT_array()
  
  return(hab)
}

get_spawn_hab_all <- function(watersheds, species) {
  
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed, years=c(1979, 1999))
    habitat <- cvpiaHabitat::set_spawning_habitat(watershed, 
                                                  species = species,
                                                  flow = flows)
    tibble(
      year = rep(1979:1999, each = 12),
      month = rep(1:12, 21),
      watershed = watershed, 
      hab_sq_m = habitat)
  })
  
  # deal with sacramento special cases
  # upper sac
  up_sac_flows <- get_flow('Upper Sacramento River', years=c(1979, 1999))
  months <- rep(1:12, 21)
  up_sac_hab <- map2_dbl(months, up_sac_flows, function(month, flow) {
    cvpiaHabitat::set_spawning_habitat('Upper Sacramento River', 
                                       species = species, 
                                       flow = flow, month = month)
  })
  
  up_sac <- tibble(
    year = rep(1979:1999, each = 12),
    month = rep(1:12, 21),
    watershed = 'Upper Sacramento River', 
    hab_sq_m = up_sac_hab)
  
  hab <-   bind_rows(most, up_sac) %>% 
    spread(watershed, hab_sq_m) %>% 
    bind_cols(tibble(`Sutter Bypass` = rep(NA, 252),
                     `Yolo Bypass` = rep(NA, 252),
                     `Upper-mid Sacramento River` = rep(NA, 252),
                     `Lower-mid Sacramento River` = rep(NA, 252),
                     `Lower Sacramento River` = rep(NA, 252),
                     `San Joaquin River` = rep(NA, 252))) %>% 
    gather(watershed, habitat, -year, -month) %>% 
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>% 
    select(date, watershed, habitat) %>% 
    spread(date, habitat) %>% 
    left_join(cvpiaData::watershed_ordering) %>% 
    arrange(order) %>% 
    select(-watershed, -order) %>% 
    create_SIT_array()
  
  return(hab)
}

get_floodplain_hab_all <- function(watersheds, species) {
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed)
    habitat <- cvpiaHabitat::acres_to_square_meters(
      cvpiaHabitat::set_floodplain_habitat(watershed, species, flows))
    
    tibble(
      year = rep(1980:1999, each = 12),
      month = rep(1:12, 20),
      watershed = watershed, 
      hab_sq_m = habitat)
  })
  
  # deal with sac, already in square meters
  # upper sac
  up_sac_flow <- get_flow('Upper Sacramento River')
  up_mid_sac_flow <- get_flow('Upper-mid Sacramento River')
  low_sac_flow <- get_flow('Lower Sacramento River')
  
  up_sac_fp <- cvpiaHabitat::set_floodplain_habitat('Upper Sacramento River', species, up_sac_flow)
  up_mid_sac_fp <- cvpiaHabitat::set_floodplain_habitat('Upper-mid Sacramento River', species, up_mid_sac_flow)
  low_sac_fp <- cvpiaHabitat::set_floodplain_habitat('Lower Sacramento River', species, low_sac_flow)
  
  # lower-mid sacramento 
  low_mid_sac_flows1 <- get_flow("Lower-mid Sacramento River1") 
  low_mid_sac_flows2 <- get_flow("Lower-mid Sacramento River2") 
  low_mid_sac_fp <- cvpiaHabitat::set_floodplain_habitat('Lower-mid Sacramento River', species,
                                                         low_mid_sac_flows1, flow2 = low_mid_sac_flows2)
  sac <- tibble(
    year = rep(rep(1980:1999, each = 12), times = 4),
    month = rep(1:12, 80),
    watershed = rep(c('Upper Sacramento River', 'Upper-mid Sacramento River', 
                      'Lower-mid Sacramento River', 'Lower Sacramento River'), each = 240), 
    hab_sq_m = c(up_sac_fp, up_mid_sac_fp, low_mid_sac_fp, low_sac_fp))
  
  hab <- bind_rows(most, sac) %>% 
    spread(watershed, hab_sq_m) %>% 
    bind_cols(tibble(`Sutter Bypass` = rep(NA, 240),
                     `Yolo Bypass` = rep(NA, 240))) %>% 
    gather(watershed, habitat, -year, -month) %>% 
    mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>% 
    select(date, watershed, habitat) %>% 
    spread(date, habitat) %>% 
    left_join(cvpiaData::watershed_ordering) %>% 
    arrange(order) %>% 
    select(-watershed, -order) %>% 
    create_SIT_array()
  
  return(hab)
  
}

# spawning----------------------
spawning_watersheds <- cvpiaHabitat::modeling_exist %>% 
  filter(!is.na(FR_spawn), Watershed != 'Upper Sacramento River', Watershed != 'Upper Mid Sac Region') %>% 
  pull(Watershed)

fr_spawn <- get_spawn_hab_all(spawning_watersheds, 'fr')
sr_spawn <- get_spawn_hab_all(spawning_watersheds, 'sr')
st_spawn <- get_spawn_hab_all(spawning_watersheds, 'st')

devtools::use_data(fr_spawn, overwrite = TRUE)
devtools::use_data(sr_spawn, overwrite = TRUE)
devtools::use_data(st_spawn, overwrite = TRUE)

# rearing--------------------
watersheds_in_order <- cvpiaData::watershed_ordering %>% 
  filter(!(watershed  %in% c('Sutter Bypass',
                             'Lower-mid Sacramento River', 'Yolo Bypass'))) %>%
  pull(watershed)

  #fry------
fr_fry <- get_rear_hab_all(watersheds_in_order, 'fr', 'fry')
sr_fry <- get_rear_hab_all(watersheds_in_order, 'sr', 'fry')
st_fry <- get_rear_hab_all(watersheds_in_order, 'st', 'fry')

devtools::use_data(fr_fry, overwrite = TRUE)
devtools::use_data(sr_fry, overwrite = TRUE)
devtools::use_data(st_fry, overwrite = TRUE)

  #juvenile------
fr_juv <- get_rear_hab_all(watersheds_in_order, 'fr', 'juv')
sr_juv <- get_rear_hab_all(watersheds_in_order, 'sr', 'juv')
st_juv <- get_rear_hab_all(watersheds_in_order, 'st', 'juv')

devtools::use_data(fr_juv, overwrite = TRUE)
devtools::use_data(sr_juv, overwrite = TRUE)
devtools::use_data(st_juv, overwrite = TRUE)

# floodplain------------------------
watersheds_fp <- cvpiaData::watershed_ordering %>% 
  filter(!(watershed  %in% c('Sutter Bypass','Yolo Bypass', 
                             'Lower-mid Sacramento River', 'Upper Sacramento River',
                             'Upper-mid Sacramento River', 'Lower Sacramento River'))) %>%
  pull(watershed)

fr_fp <- get_floodplain_hab_all(watersheds_fp, 'fr')
sr_fp <- get_floodplain_hab_all(watersheds_fp, 'sr')
st_fp <- get_floodplain_hab_all(watersheds_fp, 'st')

devtools::use_data(fr_fp, overwrite = TRUE)
devtools::use_data(sr_fp, overwrite = TRUE)
devtools::use_data(st_fp, overwrite = TRUE)

# bypass in stream ----------------

bpf <- cvpiaFlow::bypass_flows %>% 
  filter(between(year(date), 1980, 1999))

bypass_instream <- bind_cols(
  'date' = pull(bpf, date),
  'yolo1' = cvpiaHabitat::set_bypass_instream_habitat(bypass = 'yolo1', flow = pull(bpf, yolo1)),
  'yolo2' = cvpiaHabitat::set_bypass_instream_habitat(bypass = 'yolo2', flow = pull(bpf, yolo2)),
  'sutter1' = cvpiaHabitat::set_bypass_instream_habitat(bypass = 'sutter1', flow = pull(bpf, sutter1)),
  'sutter2' = cvpiaHabitat::set_bypass_instream_habitat(bypass = 'sutter2', flow = pull(bpf, sutter2)),
  'sutter3' = cvpiaHabitat::set_bypass_instream_habitat(bypass = 'sutter3', flow = pull(bpf, sutter3)),
  'sutter4' = cvpiaHabitat::set_bypass_instream_habitat(bypass = 'sutter4', flow = pull(bpf, sutter4))
)

bypass_floodplain <- bind_cols(
  'date' = pull(bpf, date),
  'yolo1' = cvpiaHabitat::set_bypass_floodplain_habitat(bypass = 'yolo1', flow = pull(bpf, yolo1)),
  'yolo2' = rep(0, 240),
  'sutter1' = cvpiaHabitat::set_bypass_floodplain_habitat(bypass = 'sutter1', flow = pull(bpf, sutter1)),
  'sutter2' = cvpiaHabitat::set_bypass_floodplain_habitat(bypass = 'sutter2', flow = pull(bpf, sutter2)),
  'sutter3' = cvpiaHabitat::set_bypass_floodplain_habitat(bypass = 'sutter3', flow = pull(bpf, sutter3)),
  'sutter4' = cvpiaHabitat::set_bypass_floodplain_habitat(bypass = 'sutter4', flow = pull(bpf, sutter4))
)

inchannel_bypass <- bypass_instream %>% 
  gather(bypass, sq_meters, -date) %>% 
  spread(date, sq_meters) %>% 
  select(-bypass) %>% 
  create_SIT_array()

floodplain_bypass <- bypass_floodplain %>% 
  gather(bypass, sq_meters, -date) %>% 
  spread(date, sq_meters) %>% 
  select(-bypass) %>% 
  create_SIT_array()

devtools::use_data(inchannel_bypass, overwrite = TRUE)
devtools::use_data(floodplain_bypass, overwrite = TRUE)
