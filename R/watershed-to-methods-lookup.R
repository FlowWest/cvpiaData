#' list all watersheds with floodplain habitats
#' @export
list_floodplains <- function() {
  names(watershed_to_floodplain_methods)
}


# map the watershed to correct method
watershed_to_floodplain_methods <- list(
  "American River" = american_river_floodplain_approx,
  "Bear River" = bear_river_floodplain_approx,
  "Big Chico Creek" = big_chico_creek_floodplain_approx,
  "Butte Creek" = butte_creek_floodplain_approx,
  "Calaveras River" = calaveras_river_floodplain_approx,
  "Cottonwood Creek" = cottonwood_creek_floodplain_approx,
  "Deer Creek" = deer_creek_floodplain_approx,
  "Elder Creek" = elder_creek_floodplain_approx,
  "Feather River" = feather_river_floodplain_approx,
  "Lower-mid Sacramento River" = lower_mid_sacramento_river_floodplain_approx,
  "Lower Sacramento River" = lower_sacramento_river_floodplain_approx,
  "Mokelumne River" = mokelumne_river_floodplain_approx,
  "North Delta" = north_delta_floodplain_approx,
  "San Joaquin River" = san_joaquin_river_floodplain_approx,
  "Stanislaus River" = stanislaus_river_floodplain_approx,
  "Tuolumne River" = tuolumne_river_floodplain_approx,
  "Upper-mid Sacramento River" = upper_mid_sacramento_river_floodplain_approx,
  "Upper Sacramento River" = upper_sacramento_river_floodplain_approx,
  "Yolo Bypass" = yolo_bypass_floodplain_approx,
  "Yuba River" = yuba_river_floodplain_approx
)

watershed_to_instream_methods <- list(
  "Battle Creek" = battle_creek_instream_approx,
  "Butte Creek" = butte_creek_instream_approx,
  "Calaveras River" = calaveras_instream_approx,
  "Clear Creek" = clear_creek_instream_approx,
  "Cottonwood Creek" = cottonwood_creek_instream_approx,
  "Cow Creek" = cow_creek_instream_approx,
  "Lower Sacramento River" = lower_sacramento_instream_approx,
  "Feather River" = feather_river_instream_approx,
  "Merced River" = merced_river_instream_approx,
  "Mokelumne River" = mokelumne_river_instream_approx,
  "North Delta" = north_delta_instream_approx,
  "Stanislaus River" = stanislaus_river_instream_approx,
  "Upper-mid Sacramento River" = upper_mid_sacramento_instream_approx,
  "Yuba River" = yuba_river_instream_approx
)

watershed_to_spawning_methods <- list(
  "American River" = american_river_spawning_approx,
  "Battle Creek" = battle_creek_spawning_approx,
  "Bear River" = bear_river_spawning_approx,
  "Butte Creek" = butte_creek_spawning_approx,
  "Calaveras River" = calaveras_river_spawning_approx,
  "Clear Creek" = clear_creek_spawning_approx,
  "Cottonwood Creek" = cottonwood_creek_spawning_approx,
  "Cow Creek" = cow_creek_spawning_approx,
  "Feather River" = feather_river_spawning_approx,
  "Merced River" = merced_river_spawning_approx,
  "Mokleumne River" = mokelumne_river_spawning_approx,
  "Stanislaus River" = stanislaus_river_spawning_approx,
  "Upper-mid Sacramento River" = upper_mid_sacramento_spawning_approx,
  "Yuba River" = yuba_river_spawning_approx
)
