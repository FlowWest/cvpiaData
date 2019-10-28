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
    replace(., is.na(.), 0) %>% 
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
    replace(., is.na(.), 0) %>%
    create_SIT_array()
  
  
  return(hab)
}

get_floodplain_hab_all <- function(watersheds, species) {
  most <- map_df(watersheds, function(watershed) {
    flows <- get_flow(watershed)
    habitat <- cvpiaHabitat::set_floodplain_habitat(watershed, species, flows)
    
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
    replace(., is.na(.), 0) %>%
    create_SIT_array()
  
  
  return(hab)
  
}

# spawning----------------------
spawning_watersheds <- cvpiaHabitat::modeling_exist %>% 
  filter(!is.na(FR_spawn), Watershed != 'Upper Sacramento River', Watershed != 'Upper Mid Sac Region') %>% 
  pull(Watershed)


fr_spawn <- get_spawn_hab_all(spawning_watersheds, 'fr')
st_spawn <- get_spawn_hab_all(spawning_watersheds, 'st')

sr_spawn <- get_spawn_hab_all(spawning_watersheds, 'sr')
# several watershed that do not have spring run populations but SIT wants to enable colonization
sr_spawn[15, , ] <- st_spawn[15, , ] # Thomes Creek
sr_spawn[25, , ] <- fr_spawn[25, , ] # Calaveras River
sr_spawn[26, , ] <- fr_spawn[26, , ] # Cosumnes River
sr_spawn[28, , ] <- fr_spawn[28, , ] # Merced River
sr_spawn[is.na(sr_spawn)] <- 0 


usethis::use_data(fr_spawn, overwrite = TRUE)
usethis::use_data(sr_spawn, overwrite = TRUE)
usethis::use_data(st_spawn, overwrite = TRUE)

# rearing--------------------
watersheds_in_order <- cvpiaData::watershed_ordering %>% 
  filter(!(watershed  %in% c('Sutter Bypass',
                             'Lower-mid Sacramento River', 'Yolo Bypass'))) %>%
  pull(watershed)

#fry------
fr_fry <- get_rear_hab_all(watersheds_in_order, 'fr', 'fry')
st_fry <- get_rear_hab_all(watersheds_in_order, 'st', 'fry')

sr_fry <- get_rear_hab_all(watersheds_in_order, 'sr', 'fry')
# several watershed that do not have spring run populations but SIT wants to enable colonization
sr_fry[15, , ] <- st_fry[15, , ] # Thomes Creek
sr_fry[25, , ] <- fr_fry[25, , ] # Calaveras River
sr_fry[26, , ] <- fr_fry[26, , ] # Cosumnes River
sr_fry[28, , ] <- fr_fry[28, , ] # Merced River

usethis::use_data(fr_fry, overwrite = TRUE)
usethis::use_data(sr_fry, overwrite = TRUE)
usethis::use_data(st_fry, overwrite = TRUE)

#juvenile------
fr_juv <- get_rear_hab_all(watersheds_in_order, 'fr', 'juv')
st_juv <- get_rear_hab_all(watersheds_in_order, 'st', 'juv')

sr_juv <- get_rear_hab_all(watersheds_in_order, 'sr', 'juv')
# several watershed that do not have spring run populations but SIT wants to enable colonization
sr_juv[15, , ] <- st_juv[15, , ] # Thomes Creek
sr_juv[25, , ] <- fr_juv[25, , ] # Calaveras River
sr_juv[26, , ] <- fr_juv[26, , ] # Cosumnes River
sr_juv[28, , ] <- fr_juv[28, , ] # Merced River

usethis::use_data(fr_juv, overwrite = TRUE)
usethis::use_data(sr_juv, overwrite = TRUE)
usethis::use_data(st_juv, overwrite = TRUE)

# floodplain------------------------
watersheds_fp <- cvpiaData::watershed_ordering %>% 
  filter(!(watershed  %in% c('Sutter Bypass','Yolo Bypass', 
                             'Lower-mid Sacramento River', 'Upper Sacramento River',
                             'Upper-mid Sacramento River', 'Lower Sacramento River'))) %>%
  pull(watershed)

fr_fp <- get_floodplain_hab_all(watersheds_fp, 'fr')
st_fp <- get_floodplain_hab_all(watersheds_fp, 'st')

sr_fp <- get_floodplain_hab_all(watersheds_fp, 'sr')
# several watershed that do not have spring run populations but SIT wants to enable colonization
sr_fp[15, , ] <- st_fp[15, , ] # Thomes Creek
sr_fp[25, , ] <- fr_fp[25, , ] # Calaveras River
sr_fp[26, , ] <- fr_fp[26, , ] # Cosumnes River
sr_fp[28, , ] <- fr_fp[28, , ] # Merced River

usethis::use_data(fr_fp, overwrite = TRUE)
usethis::use_data(sr_fp, overwrite = TRUE)
usethis::use_data(st_fp, overwrite = TRUE)

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

usethis::use_data(inchannel_bypass, overwrite = TRUE)
usethis::use_data(floodplain_bypass, overwrite = TRUE)

# Winter Run ------------
# only in sacramento
View(cvpiaData::watershed_ordering)
cvpiaData::watershed_ordering$watershed[c(1, 16, 21, 24)]

 # spawn just in Upper Sac
wr_spawn <- array(NA, c(31, 12, 22)) # 22 is years from 1979 to 2000
up_sac_flows <- get_flow('Upper Sacramento River', years=c(1979, 2000))
months <- rep(1:12, 22)
up_sac_hab <- map2_dbl(months, up_sac_flows, function(month, flow) {
  cvpiaHabitat::set_spawning_habitat('Upper Sacramento River', 
                                     species = 'wr', 
                                     flow = flow, month = month)
})

wr_spawn[1,,] <- up_sac_hab
wr_spawn[is.na(wr_spawn)] <- 0
# confirm data has the additional year dimensions
dim(wr_spawn)

usethis::use_data(wr_spawn, overwrite = TRUE)

# fry and juv
wr_fry <- array(NA, c(31, 12, 21)) # 1980 - 2000
wr_fry[1,,] <- cvpiaHabitat::set_instream_habitat('Upper Sacramento River', 
                                                  species = 'wr',
                                                  life_stage = 'fry', 
                                                  flow = get_flow('Upper Sacramento River', 
                                                                  years = c(1980, 2000)))
wr_fry[16,,] <- cvpiaHabitat::set_instream_habitat('Upper-mid Sacramento River', 
                                                  species = 'wr',
                                                  life_stage = 'fry', 
                                                  flow = get_flow('Upper-mid Sacramento River', 
                                                                  years = c(1980, 2000)))
# deal with sacramento special cases
# lower-mid sac
low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1', years = c(1980, 2000))
low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2', years = c(1980, 2000))

low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
  cvpiaHabitat::set_instream_habitat('Lower-mid Sacramento River', 
                                     species = 'wr', 
                                     life_stage = 'fry', 
                                     flow = flow, flow2 = flow2)
})


wr_fry[21,,] <- low_mid_sac_hab

wr_fry[24,,] <- cvpiaHabitat::set_instream_habitat('Lower Sacramento River', 
                                                   species = 'wr',
                                                   life_stage = 'fry', 
                                                   flow = get_flow('Lower Sacramento River', 
                                                                   years = c(1980, 2000)))


wr_fry[is.na(wr_fry)] <- 0

# Confirm new dimensions include the addtional year (2000)
dim(wr_fry)

usethis::use_data(wr_fry, overwrite = TRUE)


wr_juv <- array(NA, c(31, 12, 21)) # 1980-2000 
wr_juv[1,,] <- cvpiaHabitat::set_instream_habitat('Upper Sacramento River', 
                                                  species = 'wr',
                                                  life_stage = 'juv', 
                                                  flow = get_flow('Upper Sacramento River', 
                                                                  years = c(1980, 2000)))
wr_juv[16,,] <- cvpiaHabitat::set_instream_habitat('Upper-mid Sacramento River', 
                                                   species = 'wr',
                                                   life_stage = 'juv', 
                                                   flow = get_flow('Upper-mid Sacramento River', 
                                                                   years = c(1980, 2000)))
# deal with sacramento special cases
# lower-mid sac
low_mid_sac_flow1 <- get_flow('Lower-mid Sacramento River1', years = c(1980, 2000))
low_mid_sac_flow2 <- get_flow('Lower-mid Sacramento River2', years = c(1980, 2000))

low_mid_sac_hab <- map2_dbl(low_mid_sac_flow1, low_mid_sac_flow2, function(flow, flow2) {
  cvpiaHabitat::set_instream_habitat('Lower-mid Sacramento River', 
                                     species = 'wr', 
                                     life_stage = 'juv', 
                                     flow = flow, flow2 = flow2)
})


wr_juv[21,,] <- low_mid_sac_hab

wr_juv[24,,] <- cvpiaHabitat::set_instream_habitat('Lower Sacramento River', 
                                                   species = 'wr',
                                                   life_stage = 'juv', 
                                                   flow = get_flow('Lower Sacramento River', 
                                                                   years = c(1980, 2000)))


wr_juv[is.na(wr_juv)] <- 0
# confirm new dimensions include additional year
dim(wr_juv)

usethis::use_data(wr_juv, overwrite = TRUE)


# floodplain
wr_fp <- array(NA, c(31, 12, 21)) 
wr_fp[1,,] <- cvpiaHabitat::set_floodplain_habitat('Upper Sacramento River', 'wr', 
                                                   get_flow('Upper Sacramento River',
                                                            years = c(1980, 2000)))
wr_fp[16,,] <- cvpiaHabitat::set_floodplain_habitat('Upper-mid Sacramento River', 'wr', 
                                                    get_flow('Upper-mid Sacramento River',
                                                             years = c(1980, 2000)))
wr_fp[24,,] <- cvpiaHabitat::set_floodplain_habitat('Lower Sacramento River', 'wr', 
                                                    get_flow('Lower Sacramento River',
                                                             years = c(1980, 2000)))

# lower-mid sacramento 
low_mid_sac_flows1 <- get_flow("Lower-mid Sacramento River1", years = c(1980, 2000)) 
low_mid_sac_flows2 <- get_flow("Lower-mid Sacramento River2", years = c(1980, 2000)) 
low_mid_sac_fp <- cvpiaHabitat::set_floodplain_habitat('Lower-mid Sacramento River', 'wr',
                                                       low_mid_sac_flows1, flow2 = low_mid_sac_flows2)

wr_fp[21,,] <- low_mid_sac_fp

wr_fp[is.na(wr_fp)] <- 0
# confirm new dimensions 
dim(wr_fp)

usethis::use_data(wr_fp, overwrite = TRUE)
