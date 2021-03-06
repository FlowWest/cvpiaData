## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- warning = FALSE, message = FALSE-----------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(cvpiaHabitat)
library(cvpiaFlow)


## ------------------------------------------------------------------------
# returns flow for each month of a watershed during simulation window
get_flow <- function(watershed, years=c(1980, 1999)) {
  
    # get the flow values at the dates
    dplyr::pull(dplyr::filter(dplyr::select(cvpiaFlow::flows_cfs, date, watershed), 
                                             lubridate::year(date) >= years[1], 
                                             lubridate::year(date) <= years[2]), 2)
}

# transforms to array data structure for SIT model input, [watersheds, months, years]
create_SIT_array <- function(input) {

  output <- array(NA, dim = c(nrow(input), 12, ncol(input) / 12))
  index <-  1
  for (i in seq(1, ncol(input), 12)) {
    output[ , , index] <- as.matrix(input[ , i:(i + 11)])
    index <- index + 1
  }
  return(output)

}

## ------------------------------------------------------------------------
get_spawn_hab_all <- function(species) {
  
  watersheds <- cvpiaHabitat::modeling_exist %>% 
    dplyr::filter(!is.na(FR_spawn), Watershed != 'Upper Sacramento River', Watershed != 'Upper Mid Sac Region') %>% 
    dplyr::pull(Watershed)
  
  most <- purrr::map_df(watersheds, function(watershed) {
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
  up_sac_hab <- purrr::map2_dbl(months, up_sac_flows, function(month, flow) {
    cvpiaHabitat::set_spawning_habitat('Upper Sacramento River', 
                                       species = species, 
                                       flow = flow, month = month)
  })
  
  up_sac <- tibble(
    year = rep(1979:1999, each = 12),
    month = rep(1:12, 21),
    watershed = 'Upper Sacramento River', 
    hab_sq_m = up_sac_hab)
  
  hab <-   dplyr::bind_rows(most, up_sac) %>% 
    tidyr::spread(watershed, hab_sq_m) %>% 
    dplyr::bind_cols(tibble(`Sutter Bypass` = rep(NA, 252),
                     `Yolo Bypass` = rep(NA, 252),
                     `Upper-mid Sacramento River` = rep(NA, 252),
                     `Lower-mid Sacramento River` = rep(NA, 252),
                     `Lower Sacramento River` = rep(NA, 252),
                     `San Joaquin River` = rep(NA, 252))) %>% 
    tidyr::gather(watershed, habitat, -year, -month) %>% 
    dplyr::mutate(date = lubridate::ymd(paste(year, month, 1, '-'))) %>% 
    dplyr::select(date, watershed, habitat) %>% 
    tidyr::spread(date, habitat) %>% 
    dplyr::left_join(cvpiaData::watershed_ordering) %>% 
    dplyr::arrange(order) %>% 
    dplyr::select(-watershed, -order) %>% 
    create_SIT_array()
  
  return(hab)
}

## ---- eval = FALSE-------------------------------------------------------
#  fr_spawn <- get_spawn_hab_all('fr') #fall run
#  sr_spawn <- get_spawn_hab_all('sr') #spring run
#  st_spawn <- get_spawn_hab_all('st') #steelhead

## ---- eval = FALSE-------------------------------------------------------
#  all_model_inputs <- cvpiaData::load_baseline_data('fall')
#  # user must define get_spawn_hab_all_modified to their specifications
#  all_model_inputs$IChab.spawn <- get_spawn_hab_all_modified('fr')

## ---- eval = FALSE-------------------------------------------------------
#  fr_modified <- cvpiaData::fr_spawn #copy baseline fall run spawning habitat
#  new_upper_sac_vals <- 1:252
#  # upper sacramento is the first watershed, see cvpiaData::watershed_ordering
#  fr_modified[1,,] <- new_upper_sac_vals
#  all_model_inputs$IChab.spawn <- fr_modified[1,,]

## ------------------------------------------------------------------------
get_rear_hab_all <- function(species, life_stage) {

  watersheds <- cvpiaData::watershed_ordering %>% 
    dplyr::filter(!(watershed  %in% c('Sutter Bypass',
                             'Lower-mid Sacramento River', 'Yolo Bypass'))) %>%
    dplyr::pull(watershed)
  
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


## ---- eval = FALSE-------------------------------------------------------
#  fr_fry <- get_rear_hab_all('fr', 'fry') #fall run
#  sr_fry <- get_rear_hab_all('sr', 'fry') #spring run
#  st_fry <- get_rear_hab_all('st', 'fry') #steelhead
#  
#  fr_juv <- get_rear_hab_all('fr', 'juv') #fall run
#  sr_juv <- get_rear_hab_all('sr', 'juv') #spring run
#  st_juv <- get_rear_hab_all('st', 'juv') #steelhead
#  

## ---- eval = FALSE-------------------------------------------------------
#  all_model_inputs <- cvpiaData::load_baseline_data('fall')
#  # user must define get_rear_hab_all_modified to their specifications
#  all_model_inputs$IChab.fry <- get_rear_hab_all_modified('fr', 'fry')
#  all_model_inputs$IChab.juv <- get_rear_hab_all_modified('fr', 'juv')

## ---- eval = FALSE-------------------------------------------------------
#  fr_modified <- cvpiaData::fr_fry #copy baseline fall run fry rearing habitat
#  new_upper_sac_vals <- 1:240
#  # upper sacramento is the first watershed, see cvpiaData::watershed_ordering
#  fr_modified[1,,] <- new_upper_sac_vals
#  all_model_inputs$IChab.fry <- fr_modified[1,,]

## ------------------------------------------------------------------------
get_floodplain_hab_all <- function(watersheds, species) {

  watersheds_fp <- cvpiaData::watershed_ordering %>% 
    dplyr::filter(!(watershed  %in% c('Sutter Bypass','Yolo Bypass', 
    'Lower-mid Sacramento River', 'Upper Sacramento River',
    'Upper-mid Sacramento River', 'Lower Sacramento River'))) %>%
    dplyr::pull(watershed)
  
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


## ---- eval = FALSE-------------------------------------------------------
#  fr_fp <- get_floodplain_hab_all('fr') #fall run
#  sr_fp <- get_floodplain_hab_all('sr') #spring run
#  st_fp <- get_floodplain_hab_all('st') #steelhead
#  

## ---- eval = FALSE-------------------------------------------------------
#  all_model_inputs <- cvpiaData::load_baseline_data('fall')
#  # user must define get_floodplain_hab_all_modified to their specifications
#  all_model_inputs$floodP <- get_floodplain_hab_all_modified('fr')

## ---- eval = FALSE-------------------------------------------------------
#  fr_modified <- cvpiaData::fr_fp #copy baseline fall run floodplain rearing habitat
#  new_upper_sac_vals <- 1:240
#  # upper sacramento is the first watershed, see cvpiaData::watershed_ordering
#  fr_modified[1,,] <- new_upper_sac_vals
#  all_model_inputs$floodP <- fr_modified[1,,]

