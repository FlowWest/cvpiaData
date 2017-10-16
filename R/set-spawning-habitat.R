#' Set spawning habitat area based on watershed, species, life stage and flow
#'
#' @param watershed one of the watersheds defined for the SIT model
#' @param species one of 'fr', 'sr', or 'st'
#' @param flow value used to determine habitat area
#' @export
set_spawning_area <- function(watershed, species, flow) {
  f <- watershed_to_spawning_methods[watershed][[1]](species)

  f(flow)
}

# INTERNALS

spawning_species_error <- function(species) {
  stop(paste0("'", "' was not found for spawning habitat in this watershed"))
}

american_river_spawning_approx <- function(species) {
  d <- cvpiaHabitat::american_river_instream

  switch(species,
         "fr" = approxfun(d$flow_cfs, d$FR_spawning, rule = 2),
         "st" = approxfun(d$flow_cfs, d$ST_spawning, rule = 2),
         spawning_species_error(species))
}

battle_creek_spawning_approx <- function(species) {
  d <- cvpiaHabitat::battle_creek_instream

  switch(species,
         "fr" = approxfun(d$flow_cfs, d$spawn_WUA, rule = 2),
         spawning_species_error(species))
}


bear_river_spawning_approx <- function(species) {
  d <- cvpiaHabitat::bear_river_instream

  switch(species,
         "fr" = approxfun(d$flow_cfs, d$spawn_WUA, rule = 2),
         spawning_species_error(species))
}

butte_creek_spawning_approx <- function(species) {
  d <- cvpiaHabitat::butte_creek_instream

  switch(species,
         "fr" = approxfun(d$flow_cfs, d$spawn_WUA, rule = 2),
         spawning_species_error(species))
}

calaveras_spawning_approx <- function(species) {
  d <- cvpiaHabitat::calaveras_river_instream

  switch(species,
         "fr" = approxfun(d$flow_cfs, d$spawn_WUA, rule = 2),
         spawning_species_error(species))
}

clear_creek_spawning_approx <- function(species) {
  d <- cvpiaHabitat::clear_creek_instream

  switch(spcies,
         "fr" = approxfun(d$flow_cfs, d$FR_spawning, rule = 2),
         "sr" = approxfun(d$flow_cfs, d$SR_spawning, rule = 2),
         "st" = approxfun(d$flow_cfs, d$ST_spawning, rule = 2),
         spawning_species_error(species))
}

cottonwood_creek_spawning_approx <- function(species) {

}

cow_creek_spawning_approx <- function(species) {

}

feather_river_spawning_approx <- function(species) {

}

lower_sacramneto_spawning_approx <- function(species) {

}

merced_river_spawning_approx <- function(species) {

}

mokelumne_river_spawning_approx <- function(speices) {

}

north_delta_spawning_approx <- function(species) {

}

stanislaus_river_spawning_approx <- function(species) {

}

upper_mid_sacramento_spawning_approx <- function(species) {

}

yuba_river_spawning_approx <- function(species) {

}
