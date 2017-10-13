list_floodplains <- function() {
  names(watershed_to_method)
}


# map the watershed to correct method
watershed_to_floodplain_methods <- list(
  "american_river" = american_river_floodplain_approx,
  "bear_river" = bear_river_floodplain_approx,
  "big_chico_creek" = big_chico_creek_floodplain_approx,
  "butte_creek" = butte_creek_floodplain_approx,
  "calaveras_river" = calaveras_river_floodplain_approx,
  "cottonwood_creek" = cottonwood_creek_floodplain_approx,
  "deer_creek" = deer_creek_floodplain_approx,
  "elder_creeK" = elder_creek_floodplain_approx,
  "feather_river" = feather_river_floodplain_approx,
  "lower_mid_sacramento" = lower_mid_sacramento_river_floodplain_approx
)
