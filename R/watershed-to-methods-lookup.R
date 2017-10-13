#' list all watersheds with floodplain habitats
#' @export
list_floodplains <- function() {
  names(watershed_to_floodplain_methods)
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
  "lower_mid_sacramento" = lower_mid_sacramento_river_floodplain_approx,
  "lower_sacramento" = lower_sacramento_river_floodplain_approx,
  "mokelumne_river" = mokelumne_river_floodplain_approx,
  "north_delta" = north_delta_floodplain_approx,
  "san_joaquin_river" = san_joaquin_river_floodplain_approx,
  "stanislaus_river" = stanislaus_river_floodplain_approx,
  "tuolumne_river" = tuolumne_river_floodplain_approx,
  "upper_mid_sacramento" = upper_mid_sacramento_river_floodplain_approx,
  "upper_sacramento" = upper_sacramento_river_floodplain_approx,
  "yolo_bypass" = yolo_bypass_floodplain_approx,
  "yuba_river" = yuba_river_floodplain_approx
)
