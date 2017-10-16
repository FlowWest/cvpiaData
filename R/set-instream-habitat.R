#' Set instream habitat area based on watershed, species, life stage and flow
#'
#' @param watershed one of the watersheds defined for the SIT model
#' @param species one of 'fr', 'sr', or 'st'
#' @param life_stage life stage of fish, one of 'juv', 'adult' or 'fry'
#' @param flow value used to determine habitat area
#' @export
set_instream_area <- function(watershed, species, life_stage, flow) {
  f <- watershed_to_instream_methods[watershed][[1]](species, life_stage)

  wua_value <- f(flow)
  area_value <- wua_to_area(wua = wua_value,
                            ws = watershed,
                            sp = "Fall Run Chinook",
                            ls = "rearing") # ask sadie if rearing is ok here
}

# INTERNALS

instream_species_not_found_error <- function(species, ...)
  stop(paste0("species '",species,"' not found for instream habitat in this watershed", ...),
       call. = FALSE)

battle_creek_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::battle_creek_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule = 2)
           else instream_species_not_found_error(species, " with life stage fry")},
         instream_species_not_found_error(species))
}

bear_river_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::bear_river_instream

  # ok this is gonna look ugly
  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule = 2)
           else instream_species_not_found_error(species)
           },
         instream_species_not_found_error(species))

}

butte_creek_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::butte_creek_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule =2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$fry_WUA, rule = 2)
           else instream_species_not_found_error(species, " with specified life stage")
         },
         instream_species_not_found_error(species))
}

calaveras_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::calaveras_river_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule = 2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$fry_WUA, rule = 2)
           else instream_species_not_found_error(species, "with supplied life stage")
         },
         instream_species_not_found_error(species))
}

clear_creek_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::clear_creek_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$FR_juv, rule = 2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$FR_fry, rule = 2)
         },
         "sr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$SR_juvenile, rule = 2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$SR_fry)
         },
         "st" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$ST_juvenile, rule = 2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$ST_fry, rule = 2)
         },
         instream_species_not_found_error(species))
}

cottonwood_creek_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::cottonwood_creek_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$fry_WUA)
         },
         instream_species_not_found_error(species))
}

cow_creek_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::cow_creek_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule = 2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$fry_WUA, rule = 2)
         },
         instream_species_not_found_error(species))
}

feather_river_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::feather_river_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule = 2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$fry_WUA, rule =2)
         },
         instream_species_not_found_error(species))
}

lower_sacramento_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::lower_sacramento_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule = 2)
           else instream_species_not_found_error(species, " with supplied life stage")
         },
         instream_species_not_found_error(species))
}

merced_river_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::merced_river_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule = 2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$fry_WUA, rule = 2)
         },
         "st" = {
           if (life_stage == "adult") approxfun(d$flow_cfs, d$adult_steelhead_WUA)
           else instream_species_not_found_error(species, " with supplied life stage")
         })
}

mokelumne_river_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::mokelumne_river_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule = 2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$fry_WUA, rule = 2)
         },
         instream_species_not_found_error(species))
}

# Note - this looks a little funny
north_delta_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::north_delta_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$area_acres, rule = 2)
           else instream_species_not_found_error(species, " with supplied life stage")
         })
}

stanislaus_river_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::stanislaus_river_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$FR_juv_WUA, rule = 2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$FR_fry_WUA, rule = 2)
         },
         "st" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$ST_juv_WUA, rule = 2)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$ST_fry_WUA, rule = 2)
         },
         instream_species_not_found_error(species))
}

upper_mid_sacramento_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::upper_mid_sacramento_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$juv_WUA, rule = 2)
           else instream_species_not_found_error(species, " with supplied life stage")
         },
         instream_species_not_found_error(species))
}

yuba_river_instream_approx <- function(species, life_stage) {
  d <- cvpiaHabitat::yuba_river_instream

  switch(species,
         "fr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$FR_SR_juv)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$FR_SR_fry)
         },
         "sr" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$FR_SR_juv)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$FR_SR_fry)
         },
         "st" = {
           if (life_stage == "juv") approxfun(d$flow_cfs, d$ST_juv)
           else if (life_stage == "fry") approxfun(d$flow_cfs, d$ST_fry)
         },
         instream_species_not_found_error(species))
}
