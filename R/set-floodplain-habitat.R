# Steps in development
# 1. get watershed from cvpiaHabitat data package for the watershed
# 2. get the species as defined in input (this will be mostly Fall Run)
# 3. get wua based on the flow input
# 4. do the interpolation to to return the appropriate are in square meters


set_floodplain_area <- function(watershed) {
  # do stuff
}

# INTERNALS

# set up approx functions for all floodplains

american_river_floodplain <- function() {
  d <- cvpiaHabitat::american_river_floodplain
  f <- approxfun(d$flow_cfs, d$FR_floodplain_acres)

  f
}

