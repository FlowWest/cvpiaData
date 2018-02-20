library(dplyr)
library(purrr)


year_month_df <- tibble::as_tibble(expand.grid(year = 1980:1999, month = 1:12)) %>% 
  arrange(year, month)

watersheds_in_order <- cvpiaData::watershed_ordering %>% 
  filter(!(watershed  %in% c('Upper Sacramento River', 'Sutter Bypass', 
                             'Lower-mid Sacramento River', 'Yolo Bypass'))) %>% 
  pull(watershed)

get_flow <- function(watershed, years=c(1980, 1999)) {
  
    # get the flow values at the dates
    dplyr::pull(dplyr::filter(dplyr::select(cvpiaFlow::flows_cfs, date, watershed), 
                                             lubridate::year(date) >= years[1], 
                                             lubridate::year(date) <= years[2]), 2)
    
  
}

## IN CHANNEL------------------------------------------
# FALL RUN - FRY AND JUV (1980 - 1999)
# FALL RUN - SPAWNING (1979 - 1999)

# IN CHANNEL FALL RUN JUV AND FRY ------------------------------------------------

# Antelope Creek ------------
antelope_creek_flows <- get_flow("Antelope Creek")

# fall run fry
antelope_creek_fr_fry <- purrr::map_dbl(antelope_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Antelope Creek", 
                                     species = "fr", 
                                     life_stage = "fry", 
                                     flow = f)
})

# fall run juv
antelope_creek_fr_juv <- purrr::map_dbl(antelope_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Antelope Creek", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)
})

# Battle Creek --------------------------------

battle_creek_flows <- get_flow("Battle Creek")

# fall run fry 
battle_creek_fr_fry <- cvpiaHabitat::set_instream_habitat("Battle Creek", 
                                                          species = "fr", 
                                                          life_stage = "fry", 
                                                          flow = battle_creek_flows)
# fall run juv 
battle_creek_fr_juv <- cvpiaHabitat::set_instream_habitat("Battle Creek", 
                                                          species = "fr", 
                                                          life_stage = "juv", 
                                                          flow = battle_creek_flows)

# Bear Creek ----------------------------------

bear_creek_flows <- get_flow("Bear Creek")

#NOTE -- this one required purrr
# fall run fry 
bear_creek_fr_fry <- purrr::map_dbl(bear_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Bear Creek", 
                                     species = "fr", 
                                     life_stage = "fry", 
                                     flow = f)
})

# fall run juv
bear_creek_fr_juv <- purrr::map_dbl(bear_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Bear Creek", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)
})

# Big Chico Creek -------------------

big_chico_flows <- get_flow("Big Chico Creek")

# fall run fry
big_chico_fr_fry <- purrr::map_dbl(big_chico_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Big Chico Creek", 
                                     species = "fr", 
                                     life_stage = "fry", 
                                     flow = f)
})

# fall run juv
big_chico_fr_juv <- purrr::map_dbl(big_chico_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Big Chico Creek", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)
})



# Butte Creek ------------------------------------------

butte_creek_flows <- get_flow("Butte Creek")

# fall run fry
butte_creek_fr_fry <- cvpiaHabitat::set_instream_habitat("Butte Creek", 
                                   species = "fr", 
                                   life_stage = "fry", 
                                   flow = butte_creek_flows)
# fall run juv
butte_creek_fr_juv <- cvpiaHabitat::set_instream_habitat("Butte Creek", 
                                                         species = "fr", 
                                                         life_stage = "juv", 
                                                         flow = butte_creek_flows)


# Clear Crekk ----------------------------------

clear_creek_flows <- get_flow("Clear Creek")

# fall run fry
clear_creek_fr_fry <- cvpiaHabitat::set_instream_habitat("Clear Creek", 
                                                         species = "fr", 
                                                         life_stage = "fry", 
                                                         flow = clear_creek_flows)

# fall run juv
clear_creek_fr_juv <- cvpiaHabitat::set_instream_habitat("Clear Creek", 
                                                         species = "fr", 
                                                         life_stage = "juv", 
                                                         flow = clear_creek_flows)

# Cottonwood Creek ---------------------------------

cottonwood_creek_flows <- get_flow("Cottonwood Creek")

# fall run fry
cottonwood_creek_fr_fry <- cvpiaHabitat::set_instream_habitat("Cottonwood Creek", 
                                                         species = "fr", 
                                                         life_stage = "fry", 
                                                         flow = cottonwood_creek_flows)

# fall run juv
cottonwood_creek_fr_juv <- cvpiaHabitat::set_instream_habitat("Cottonwood Creek", 
                                                              species = "fr", 
                                                              life_stage = "juv", 
                                                              flow = cottonwood_creek_flows)


# Cow Creek 

cow_creek_flows <- get_flow("Cow Creek")

#fry
cow_creek_fr_fry <- cvpiaHabitat::set_instream_habitat("Cow Creek", 
                                                       species = "fr", 
                                                       life_stage = "fry", 
                                                       flow = cow_creek_flows)
#juv
cow_creek_fr_juv <- cvpiaHabitat::set_instream_habitat("Cow Creek", 
                                                       species = "fr", 
                                                       life_stage = "juv", 
                                                       flow = cow_creek_flows)


# Deer Creek --------------------------------

deer_creek_flows <- get_flow("Deer Creek")

# fry
deer_creek_fr_fry <- purrr::map_dbl(deer_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Deer Creek", 
                                     species = "fr", 
                                     life_stage = "fry", 
                                     flow = f)
})

# juv
deer_creek_fr_juv <- purrr::map_dbl(deer_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Deer Creek", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)
})

# Elder Creek -------------------------------------------------

elder_creek_flows <- get_flow("Elder Creek")

#fry
elder_creek_fr_fry <- purrr::map_dbl(elder_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Elder Creek", 
                                     species = "fr", 
                                     life_stage = "fry", 
                                     flow = f)
})
#juv
elder_creek_fr_juv <- purrr::map_dbl(elder_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Elder Creek", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)
})

# Mill Creek

mill_creek_flows <- get_flow("Mill Creek")

#fry
mill_creek_fr_fry <- purrr::map_dbl(mill_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Mill Creek", 
                                     species = "fr", 
                                     life_stage = "fry", 
                                     flow = f)
})

#juv
mill_creek_fr_juv <- purrr::map_dbl(mill_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Mill Creek", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)
})

# Paynes Creek ----------------------------------------------

paynes_creek_flows <- get_flow("Paynes Creek")

#fry
paynes_creek_fr_fry <- purrr::map_dbl(paynes_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Paynes Creek", 
                                     species = "fr", 
                                     life_stage = "fry", 
                                     flow = f)
})
#juv
paynes_creek_fr_juv <- purrr::map_dbl(paynes_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Paynes Creek", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)
})

# Stony Creek ---------------------------------------------

stony_creek_flows <- get_flow("Stony Creek")
#fry
stony_creek_fr_fry <- purrr::map_dbl(stony_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Stony Creek", 
                                     species = "fr", 
                                     life_stage = "fry", 
                                     flow = f)
})

#juv
stony_creek_fr_juv <- purrr::map_dbl(stony_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Stony Creek", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)
})

# Thomes ----------------------------------------------------

thomes_creek_flows <- get_flow("Thomes Creek")

#fry
thomes_creek_fr_fry <- purrr::map_dbl(thomes_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Thomes Creek", 
                                     species = "fr", 
                                     life_stage = "fry", 
                                     flow = f)})

#juv
thomes_creek_fr_juv <- purrr::map_dbl(thomes_creek_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Thomes Creek", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)})




#fry 
thomes_creek_fr_fry <- cvpiaHabitat::set_instream_habitat("Thomes Creek", 
                                                          species = "fr", 
                                                          life_stage = "fry", 
                                                          flow = thomes_creek_flows)


# Upper mid Sacr SKIP

# Sutter Bypass SKIP


# Bear River

bear_river_flows <- get_flow("Bear River")
#fry
bear_river_fr_fry <- cvpiaHabitat::set_instream_habitat("Bear River", 
                                                        species = "fr", 
                                                        life_stage = "fry", 
                                                        flow = bear_river_flows)
#juv
bear_river_fr_juv <- cvpiaHabitat::set_instream_habitat("Bear River", 
                                                        species = "fr", 
                                                        life_stage = "juv", 
                                                        flow = bear_river_flows)

# Feather River

feather_river_flows <- get_flow("Feather River")

feather_river_fr_fry <- cvpiaHabitat::set_instream_habitat("Feather River", 
                                                        species = "fr", 
                                                        life_stage = "fry", 
                                                        flow = feather_river_flows)
#juv
feather_river_fr_juv <- cvpiaHabitat::set_instream_habitat("Feather River", 
                                                        species = "fr", 
                                                        life_stage = "juv", 
                                                        flow = feather_river_flows)


# Yuba River ----------------------------------------

yuba_river_flows <- get_flow("Yuba River")

yuba_river_fr_fry <- cvpiaHabitat::set_instream_habitat("Yuba River", 
                                                           species = "fr", 
                                                           life_stage = "fry", 
                                                           flow = yuba_river_flows)
#juv
yuba_river_fr_juv <- cvpiaHabitat::set_instream_habitat("Yuba River", 
                                                           species = "fr", 
                                                           life_stage = "juv", 
                                                           flow = yuba_river_flows)

# Lower-mid Sac SKIP

# Yolo Bypass SKIP

# American River ------------------------------------------
##### ERROR in American River: Unknown or uninitialised column: 'FR_fry_wua'. #########

american_river_flows <- get_flow("American River")

american_river_fr_fry <- cvpiaHabitat::set_instream_habitat("American River", 
                                                        species = "fr", 
                                                        life_stage = "fry", 
                                                        flow = american_river_flows)





# Calaveras River --------------------------------------------

calaveras_river_flows <- get_flow("Calaveras River")

#fry
calaveras_river_fr_fry <- cvpiaHabitat::set_instream_habitat("Calaveras River", 
                                                             species = "fr", 
                                                             life_stage = "fry", 
                                                             flow = calaveras_river_flows)
#juv
calaveras_river_fr_juv <- cvpiaHabitat::set_instream_habitat("Calaveras River", 
                                                             species = "fr", 
                                                             life_stage = "juv", 
                                                             flow = calaveras_river_flows)


# Cosumnes River

cosumnes_river_flows <- get_flow("Cosumnes River")


# fry
cosumnes_river_fr_fry <- purrr::map_dbl(cosumnes_river_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Cosumnes River", 
                                     species = "fr", 
                                     life_stage = "fry", 
                                     flow = f)})


# juv
cosumnes_river_fr_juv <- purrr::map_dbl(cosumnes_river_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("Cosumnes River", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)})


# Mokelumne River ----------------------------------------

mokelumne_river_flows <- get_flow("Mokelumne River")

#fry
mokelumne_river_fr_fry <- cvpiaHabitat::set_instream_habitat("Mokelumne River", 
                                                             species = "fr", 
                                                             life_stage = "fry", 
                                                             flow = mokelumne_river_flows)

#juv
mokelumne_river_fr_juv <- cvpiaHabitat::set_instream_habitat("Mokelumne River", 
                                                             species = "fr", 
                                                             life_stage = "juv", 
                                                             flow = mokelumne_river_flows)


# Merced River

merced_river_flows <- get_flow("Merced River")

# fry
merced_river_fr_fry <- cvpiaHabitat::set_instream_habitat("Merced River", 
                                                          species = "fr", 
                                                          life_stage = "fry", 
                                                          flow = merced_river_flows)

# juv
merced_river_fr_juv <- cvpiaHabitat::set_instream_habitat("Merced River", 
                                                          species = "fr", 
                                                          life_stage = "juv", 
                                                          flow = merced_river_flows)


# Stanislaus River 

stan_river_flows <- get_flow("Stanislaus River")


stanislaus_river_fr_fry <- cvpiaHabitat::set_instream_habitat("Stanislaus River", 
                                                          species = "fr", 
                                                          life_stage = "fry", 
                                                          flow = stan_river_flows)

# juv
stanislaus_river_fr_juv <- cvpiaHabitat::set_instream_habitat("Stanislaus River", 
                                                          species = "fr", 
                                                          life_stage = "juv", 
                                                          flow = stan_river_flows)


# Tuolomne River 

tuolumne_river_flows <- get_flow("Tuolumne River")

tuolumne_river_fr_fry <- cvpiaHabitat::set_instream_habitat("Tuolumne River", 
                                                            species = "fr", 
                                                            life_stage = "fry", 
                                                            flow = tuolumne_river_flows)


tuolumne_river_fr_juv <- cvpiaHabitat::set_instream_habitat("Tuolumne River", 
                                                            species = "fr", 
                                                            life_stage = "juv", 
                                                            flow = tuolumne_river_flows)


# San Joaquin River

san_joaquin_river_flows <- get_flow("San Joaquin River")

san_joaquin_river_fr_fry <- purrr::map_dbl(san_joaquin_river_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("San Joaquin River", 
                                     species = "fr", 
                                     life_stage = "fry", 
                                     flow = f)})

san_joaquin_river_fr_juv <- purrr::map_dbl(san_joaquin_river_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("San Joaquin River", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)})


# Bind to Dataframe -------------------------------------------

# fry
inchannel_fry_habitat <- bind_cols(
  year_month_df, 
  antelope_creek=antelope_creek_fr_fry, 
  battle_creek=battle_creek_fr_fry, 
  bear_creek=bear_creek_fr_fry, 
  big_chico_creek=big_chico_fr_fry, 
  butte_creek=butte_creek_fr_fry, 
  clear_creek=clear_creek_fr_fry, 
  cottonwood_creek=cottonwood_creek_fr_fry, 
  cow_creek=cow_creek_fr_fry,
  deer_creek=deer_creek_fr_fry, 
  elder_creek=elder_creek_fr_fry, 
  mill_creek=mill_creek_fr_fry,
  paynes_creek=paynes_creek_fr_fry,
  stony_creek=stony_creek_fr_fry,
  thomes_creek=thomes_creek_fr_fry,
  bear_river=bear_river_fr_fry,
  feather_river=feather_river_fr_fry,
  yuba_river=yuba_river_fr_fry,
  calaveras_river=calaveras_river_fr_fry,
  cosumnes_river=cosumnes_river_fr_fry,
  mokelumne_river=mokelumne_river_fr_fry,
  merced_river=merced_river_fr_fry,
  stanislaus_river=stanislaus_river_fr_fry,
  tuolumne_river=tuolumne_river_fr_fry,
  san_joaquin_river=san_joaquin_river_fr_fry
) %>% tidyr::gather(watershed, habitat, -c(year, month)) %>% 
  mutate(life_stage="fry")


#juv

inchannel_juv_habitat <- bind_cols(
  year_month_df, 
  antelope_creek=antelope_creek_fr_juv, 
  battle_creek=battle_creek_fr_juv, 
  bear_creek=bear_creek_fr_juv, 
  big_chico_creek=big_chico_fr_juv, 
  butte_creek=butte_creek_fr_juv, 
  clear_creek=clear_creek_fr_juv, 
  cottonwood_creek=cottonwood_creek_fr_juv, 
  cow_creek=cow_creek_fr_juv,
  deer_creek=deer_creek_fr_juv, 
  elder_creek=elder_creek_fr_juv, 
  mill_creek=mill_creek_fr_juv,
  paynes_creek=paynes_creek_fr_juv,
  stony_creek=stony_creek_fr_juv,
  thomes_creek=thomes_creek_fr_juv,
  bear_river=bear_river_fr_juv,
  feather_river=feather_river_fr_juv,
  yuba_river=yuba_river_fr_juv,
  calaveras_river=calaveras_river_fr_juv,
  cosumnes_river=cosumnes_river_fr_juv,
  mokelumne_river=mokelumne_river_fr_juv,
  merced_river=merced_river_fr_juv,
  stanislaus_river=stanislaus_river_fr_juv,
  tuolumne_river=tuolumne_river_fr_juv,
  san_joaquin_river=san_joaquin_river_fr_juv
) %>% tidyr::gather(watershed, habitat, -c(year, month)) %>% 
mutate(life_stage="juv")


inchannel_habitat <- bind_rows(
  inchannel_fry_habitat,
  inchannel_juv_habitat
)













































