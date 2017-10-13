#' set floodplain area based on a watershed, species and a flow
#' @description based on watershed, species and flow return a WUA
#'
#' @param watershed a watershed defined for the SIT model
#' @param species one of 'fr' (Fall Run), 'sr' (Spring Run), or 'st' (Steelhead)
#' @param flow value of flow to return WUA for
#' @param ... further arguments to be passed into an \code{\link[stats]{approxfun}}
#' @export
set_floodplain_area <- function(watershed, species, flow, ...) {
  f <- watershed_to_floodplain_methods[watershed][[1]](species, ...) # <- TODO set rule=2 here

  f(flow)
}

# INTERNALS

species_not_found_error <- function(species)
  stop(paste0("species: '",species,"' not found"))

# set up approx functions for all floodplains
# note most of these use same data for both FR, SR and ST,
# and so a switch statement may appear a bit redundant
american_river_floodplain_approx <- function(species, method = "interpolate") {
  d <- cvpiaHabitat::american_river_floodplain

  if (method == "interpolate") {
    switch(species,
           "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
           "st" = approxfun(d$flow_cfs, d$ST_floodplain_acres, rule = 2),
           species_not_found_error(species))
  } else if (method == "other"){
    print("nothing here yet")
  }
}

bear_river_floodplain_approx <- function(species, method = "interpolate") {
  d <- cvpiaHabitat::bear_river_floodplain

  if (method == "interpolate") {
    switch(species,
           "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
           "st" = approxfun(d$flow_cfs, d$ST_floodplain_acres, rule = 2),
           species_not_found_error(species))
  } else if (method == "") {
    # do the other stuff
  }
}

big_chico_creek_floodplain_approx <- function(species, method = "interpolate") {
  d <- cvpiaHabitat::big_chico_creek_floodplain

  if (method == "interpolate") {
    switch(species,
           "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
           species_not_found_error(species))
  } else if (method == "") {
    # do the other stuff
  }
}

butte_creek_floodplain_approx <- function(species, method = "interpolate") {
  d <- cvpiaHabitat::butte_creek_floodplain

  if (method == "interpolate") {
    switch(species,
           "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
           "sr" = approxfun(d$flow_cfs, d$SR_floodplain_acres, rule = 2),
           "st" = approxfun(d$flow_cfs, d$ST_floodplain_acres, rule = 2),
           species_not_found_error(species))
  } else if (method == "") {
    # do other stuff
  }

}

calaveras_river_floodplain_approx <- function() {
  d <- cvpiaHabitat::calaveras_river_floodplain

  if (method == "interpolate") {
    switch(species,
           "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
           "st" = approxfun(d$flow_cfs, d$ST_floodplain_acres, rule = 2),
           species_not_found_error(species))
  } else if (method == "") {
    # do other stuff
  }
}

cosumnes_river_floodplain_approx <- function(species, method = "interpolate") {
  d <- cvpiaHabitat::cosumnes_river_floodplain

  if (method == "interpolate") {
    switch(species,
           "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres, rule = 2),
           "st" = approxfun(d$flow_cfs, d$ST_floodplain_acres, rule = 2),
           species_not_found_error(species))
  } else if (method == "suitability") {
    # do other stuff
  }
}

cottonwood_creek_floodplain_approx <- function(species, method = "interpolate") {
  d <- cvpiaHabitat::cottonwood_creek_floodplain

  if (method == "interpolate") {
    switch(species,
           "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres),
           species_not_found_error(species))
  } else if (method == "suitability") {
    # other stuff
  }
}

deer_creek_floodplain_approx <- function(species, method = "interpolate") {
  d <- cvpiaHabitat::deer_creek_floodplain

  if (method == "interpolate") {
    switch(species,
           "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres),
           species_not_found_error(species))
  } else if (method == "suitability") {
    # do other stuff
  }
}

elder_creek_floodplain_approx <- function(species, method = "interpolate") {
  d <- cvpiaHabitat::elder_creek_floodplain

  if (method == "interpolate") {
    switch(species,
           "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres),
           species_not_found_error(species))
  } else if (method == "suitability") {
    # do stuff
  }
}

feather_river_floodplain_approx <- function(species, method = "interpolate") {
  d <- cvpiaHabitat::feather_river_floodplain

  if (method == "interpolate") {
    switch(species,
           "fr" = approxfun(d$flow_cfs, d$FR_floodplain_acres),
           "sr" = approxfun(d$flow_cfs, d$FR_floodplain_acres),
           "st" = approxfun(d$flow_cfs, d$FR_floodplain_acres),
           species_not_found_error(species))
  } else if (method == "suitability") {
    # do stuff
  }
}

lower_mid_sacramento_river_floodplain_approx <- function(species, method = "interpolate") {
  d <- cvpiaHabitat::lower_mid_sacramento_river_floodplain

  if (method == "interpolate") {
    switch(species,
           "fr" = approxfun(d$flow_cfs, d$floodplain_acres),
           species_not_found_error(species))
  } else if (method == "suitability") {
    # do stuff
  }
}

lower_sacramento_river_floodplain_approx <- function(species, method = "interpolate") {}

mokelumne_river_floodplain_approx <- function(species, method = "interpolate") {}

north_delta_floodplain_approx <- function(species, method = "interpolate") {}

san_joaquin_river_floodplain_approx <- function(species, method = "interpolate") {}

stanislaus_river_floodplain_approx <- function(species, method = "interpolate") {}

tuolumne_river_floodplain_approx <- function(species, method = "interpolate") {}

upper_mid_sacramento_river_floodplain_approx <- function(species, method = "interpolate") {}

upper_sacramento_river_floodplain_approx <- function(species, method = "interpolate") {}

yolo_bypass_floodplain_approx <- function(species, method = "interpolate") {}

yuba_river_floodplain_approx <- function(species, method = "interpolate") {}

# # map the watershed to correct method
# watershed_to_method <- list(
#   "american_river_floodplain" = american_river_floodplain_approx,
#   "bear_river_floodplain" = bear_river_floodplain_approx,
#   "big_chico_creek" = big_chico_creek_floodplain_approx
# )
