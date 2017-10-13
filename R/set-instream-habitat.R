#' Set instream habitat area based on watershed, species, life stage and flow
#'
#' @param watershed one of the watersheds defined for the SIT model
#' @param species one of 'fr', 'sr', or 'st'
#' @param life_stage life stage of fish, one of 'juv' or 'fry'
#' @param flow value used to determine habitat area
#' @export
set_instream_area <- function(watershed, species, life_stage, flow) {
  f <- watershed_to_instream_methods[watershed][[1]](species, life_stage)

  f(flow)
}

# INTERNALS

instream_species_not_found_error <- function(species, ...)
  stop(paste0("species: '",species,"' not found for instream habitat in this watershed", ...),
       call. = FALSE)

battle_creek_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::battle_creek_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule = 2)
           else instream_species_not_found_error(species, " with life stage fry")},
         instream_species_not_found_error(species))
}

bear_river_instream_approx <- function(species) {
  d <- cvpiaHabitat::bear_river_instream

  # ok this is gonna look ugly
  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule = 2)
           else instream_species_not_found_error(species)},
         instream_species_not_found_error(species))

}

butte_creek_instream_approx <- function(species) {
  d <- cvpiaHabitat::butte_creek_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule =2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$fry_WUA, rule = 2)
         },
         instream_species_not_found_error(species))
}

# calaveras_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
#
# clear_creek_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
#
# cottonwood_creek_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
#
# cow_creek_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
#
# feather_river_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
#
# lower_sacramento_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
#
# merced_river_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
#
# mokelumne_river_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
#
# north_delta_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
#
# stanislaus_river_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
#
# upper_mid_sacramento_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
#
# yuba_river_instream_approx <- function(species) {
#   d <- cvpiaHabitat::_
#
#   switch(species, {})
# }
