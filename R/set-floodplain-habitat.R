#' set floodplain area based on a watershed, species and a flow
#' @description based on watershed, species and flow return a WUA
#'
#' @param watershed a watershed defined for the SIT model
#' @param species one of 'fr' (Fall Run), 'sr' (Spring Run), or 'st' (Steelhead)
#' @param flow value of flow to return WUA for
#' @export
set_floodplain_habitat <- function(watershed, species, flow) {
  f <- watershed_to_floodplain_methods[watershed][[1]](species)

  f(flow)
}

# INTERNALS

# x is the wua
# l in the length of watershed
wua_to_area <- function(watershed, wua, life_stage) {
  # implement this to converty the wua into an area
}

species_not_found_error <- function(species, w)
  stop(paste0("species: '",species,"' not found for floodplain habitat in this watershed"),
       call. = FALSE)


# Note - this looks redundant, but it works... eventually as the package matures
#        we can consider refactoring a lot of the repetitive code below
# Note - rule = 2 in approxfun calls below make such that out of range values are assigned
#        to either the min/max(flow_cfs) depending on the side they are on
american_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::american_river_floodplain

  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$ST_floodplain_acres, rule = 2),
         species_not_found_error(species))
}

bear_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::bear_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$ST_floodplain_acres, rule = 2),
         species_not_found_error(species))

}

big_chico_creek_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::big_chico_creek_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         species_not_found_error(species))

}

butte_creek_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::butte_creek_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "sr" = approxfun(d$flow_cfs, d$SR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$ST_floodplain_acres, rule = 2),
         species_not_found_error(species))
}

calaveras_river_floodplain_approx <- function() {
  d <- cvpiaHabitat::calaveras_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$ST_floodplain_acres, rule = 2),
         species_not_found_error(species))
}

cosumnes_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::cosumnes_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$ST_floodplain_acres, rule = 2),
         species_not_found_error(species))
}

cottonwood_creek_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::cottonwood_creek_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         species_not_found_error(species))
}

deer_creek_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::deer_creek_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         species_not_found_error(species))
}

elder_creek_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::elder_creek_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         species_not_found_error(species))
}

feather_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::feather_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "sr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         species_not_found_error(species))
}

lower_mid_sacramento_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::lower_mid_sacramento_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$floodplain_acres, rule = 2),
         species_not_found_error(species))
}

lower_sacramento_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::lower_sacramento_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$floodplain_acres, rule = 2),
         species_not_found_error(species))
}

mokelumne_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::mokelumne_river_floodplain

}

north_delta_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::north_delta_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "sr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2))
}

san_joaquin_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::san_joaquin_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "sr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2))

}

stanislaus_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::stanislaus_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "sr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2))
}

tuolumne_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::tuolumne_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "sr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2))
}

upper_mid_sacramento_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::upper_mid_sacramento_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$floodplain_acres, rule = 2),
         species_not_found_error(species))
}

upper_sacramento_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::upper_sacramento_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$floodplain_acres, rule = 2),
         species_not_found_error(species))
}

yolo_bypass_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::yolo_bypass_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "sr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         species_not_found_error(species))
}

yuba_river_floodplain_approx <- function(species) {
  d <- cvpiaHabitat::yuba_river_floodplain


  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "sr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         "st" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
         species_not_found_error(species))
}

# # map the watershed to correct method
# watershed_to_method <- list(
#   "american_river_floodplain" = american_river_floodplain_approx,
#   "bear_river_floodplain" = bear_river_floodplain_approx,
#   "big_chico_creek" = big_chico_creek_floodplain_approx
# )
