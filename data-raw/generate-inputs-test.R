library(dplyr)
library(purrr)
library(cvpiaHabitat)
library(lubridate)

# create the columns that have the year and months for each flow
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


# Cow Creek ------------------------------

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

# Mill Creek ---------------------------------

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

# Upper mid Sacr SKIP ----------------

# Sutter Bypass SKIP -------------


# Bear River ----------------------------------------

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

# Feather River -------------------------------

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

american_river_fr_fry <-   purrr::map_dbl(american_river_flows, function(f) {
    cvpiaHabitat::set_instream_habitat("American River", 
                                       species = "fr", 
                                       life_stage = "fry", 
                                       flow = f)})


american_river_fr_juv <-   purrr::map_dbl(american_river_flows, function(f) {
  cvpiaHabitat::set_instream_habitat("American River", 
                                     species = "fr", 
                                     life_stage = "juv", 
                                     flow = f)})


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


# Cosumnes River ----------------------------------

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


# Merced River -----------------------

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


# Stanislaus River --------------------------------

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


# Tuolomne River --------------------------------------

tuolumne_river_flows <- get_flow("Tuolumne River")

tuolumne_river_fr_fry <- cvpiaHabitat::set_instream_habitat("Tuolumne River", 
                                                            species = "fr", 
                                                            life_stage = "fry", 
                                                            flow = tuolumne_river_flows)


tuolumne_river_fr_juv <- cvpiaHabitat::set_instream_habitat("Tuolumne River", 
                                                            species = "fr", 
                                                            life_stage = "juv", 
                                                            flow = tuolumne_river_flows)


# San Joaquin River ----------------------------------

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


# Upper sacramento river ------------------------------------------------------------
# this is composed of two different curves, the first is the board in and second is boards out
# board IN months 4-10
# board OUT months 1-3, 11-12

# These watersheds do not work with the current iteration on the habitat package
# will do the approx functions manually here
upper_sacramento_flows <- get_flow("Upper Sacramento River")

# for fall run fry modeling does not exist so we use fall run juv
upper_sac_IN_fr_fry_approx <- approxfun(cvpiaHabitat::upper_sac_ACID_boards_in$flow_cfs, 
                                        cvpiaHabitat::upper_sac_ACID_boards_in$FR_fry_wua, rule=2)

upper_sac_IN_fr_juv_approx <- approxfun(cvpiaHabitat::upper_sac_ACID_boards_in$flow_cfs, 
                                        cvpiaHabitat::upper_sac_ACID_boards_in$FR_juv_wua, rule=2)

upper_sac_OUT_fr_fry_approx <- approxfun(cvpiaHabitat::upper_sac_ACID_boards_out$flow_cfs, 
                                         cvpiaHabitat::upper_sac_ACID_boards_out$FR_fry_wua, rule=2)

upper_sac_OUT_fr_juv_approx <- approxfun(cvpiaHabitat::upper_sac_ACID_boards_out$flow_cfs, 
                                         cvpiaHabitat::upper_sac_ACID_boards_out$FR_juv_wua, rule=2)

# board IN months 4-10
# board OUT months 1-3, 11-12
upper_sac_input_df <- bind_cols(year_month_df, flows=upper_sacramento_flows)

upper_sacramento_fr_fry <-
  upper_sac_input_df %>% 
  mutate(fry_habitat = case_when(
    month %in% 4:10 ~ upper_sac_IN_fr_fry_approx(flows),
    TRUE ~ upper_sac_OUT_fr_fry_approx(flows))) %>% 
  select(upper_sacramento=fry_habitat) %>% pull()

upper_sacramento_fr_juv <-
  upper_sac_input_df %>% 
  mutate(fry_habitat = case_when(
    month %in% 4:10 ~ upper_sac_IN_fr_juv_approx(flows),
    TRUE ~ upper_sac_OUT_fr_juv_approx(flows))) %>% 
  select(upper_sacramento=fry_habitat) %>% pull()

# Lower Sacramento ------------------------------------------------------------
lower_sacramento_river_flows <- get_flow("Lower Sacramento River")

lower_sacramento_river_fr_fry <- cvpiaHabitat::set_instream_habitat("Lower Sacramento River", 
                                                              species = "fr", 
                                                              life_stage = "fry", 
                                                              flow = lower_sacramento_river_flows)

lower_sacramento_river_fr_juv <- cvpiaHabitat::set_instream_habitat("Lower Sacramento River", 
                                                                    species = "fr", 
                                                                    life_stage = "juv", 
                                                                    flow = lower_sacramento_river_flows)

# Upper Mid Sacramento --------------------------------------------------------
upper_mid_sacramento_flows <- get_flow("Upper-mid Sacramento River")

upper_mid_sacramento_fr_fry <- cvpiaHabitat::set_instream_habitat("Upper-mid Sacramento River", 
                                                                  species = "fr", 
                                                                  life_stage = "fry", 
                                                                  flow = upper_mid_sacramento_flows)

upper_mid_sacramento_fr_juv <- cvpiaHabitat::set_instream_habitat("Upper-mid Sacramento River", 
                                                                  species = "fr", 
                                                                  life_stage = "juv", 
                                                                  flow = upper_mid_sacramento_flows)



# Lower Mid Sacramento --------------------------------------------------------
# this requires the balance between two flows

lower_mid_sacramento_flows_1 <- get_flow("Lower-mid Sacramento River1") 
lower_mid_sacramento_flows_2 <- get_flow("Lower-mid Sacramento River2") 

lower_mid_sacramento_river_fr_fry_1 <- cvpiaHabitat::set_instream_habitat("Lower-mid Sacramento River", 
                                                                          species = "fr", 
                                                                          life_stage = "fry", 
                                                                          flow = lower_mid_sacramento_flows_1)

lower_mid_sacramento_river_fr_fry_2 <- cvpiaHabitat::set_instream_habitat("Lower-mid Sacramento River", 
                                                                          species = "fr", 
                                                                          life_stage = "fry", 
                                                                          flow = lower_mid_sacramento_flows_2)


lower_mid_sacramento_river_fr_fry <- (35.6/58 * lower_mid_sacramento_river_fr_fry_1) + (22.4/58 * lower_mid_sacramento_river_fr_fry_2)


lower_mid_sacramento_river_fr_juv_1 <- cvpiaHabitat::set_instream_habitat("Lower-mid Sacramento River", 
                                                                          species = "fr", 
                                                                          life_stage = "juv", 
                                                                          flow = lower_mid_sacramento_flows_1)

lower_mid_sacramento_river_fr_juv_2 <- cvpiaHabitat::set_instream_habitat("Lower-mid Sacramento River", 
                                                                          species = "fr", 
                                                                          life_stage = "juv", 
                                                                          flow = lower_mid_sacramento_flows_2)


lower_mid_sacramento_river_fr_juv <- (35.6/58 * lower_mid_sacramento_river_fr_juv_1) + (22.4/58 * lower_mid_sacramento_river_fr_juv_2)



# Inchannel Bind to Dataframe -------------------------------------------

# fry
fall_run_inchannel_fry_habitat <- bind_cols(
  year_month_df, 
  "Upper Sacramento River"=upper_sacramento_fr_fry,
  "Antelope Creek"=antelope_creek_fr_fry, 
  "Battle Creek"=battle_creek_fr_fry, 
  "Bear Creek"=bear_creek_fr_fry, 
  "Big Chico Creek"=big_chico_fr_fry, 
  "Butte Creek"=butte_creek_fr_fry, 
  "Clear Creek"=clear_creek_fr_fry, 
  "Cottonwood Creek"=cottonwood_creek_fr_fry, 
  "Cow Creek"=cow_creek_fr_fry,
  "Deer Creek"=deer_creek_fr_fry, 
  "Elder Creek"=elder_creek_fr_fry, 
  "Mill Creek"=mill_creek_fr_fry,
  "Paynes Creek"=paynes_creek_fr_fry,
  "Stony Creek"=stony_creek_fr_fry,
  "Thomes Creek"=thomes_creek_fr_fry,
  "Upper-mid Sacramento River"=upper_mid_sacramento_fr_fry,
  "Sutter Bypass" = rep(NA, nrow(year_month_df)),
  "Bear River"=bear_river_fr_fry,
  "Feather River"=feather_river_fr_fry,
  "Yuba River"=yuba_river_fr_fry,
  "Lower-mid Sacramento River"=lower_mid_sacramento_river_fr_fry,
  "Yolo Bypass" = rep(NA, nrow(year_month_df)), 
  "American River" = american_river_fr_fry,
  "Lower Sacramento River"=lower_sacramento_river_fr_fry,
  "Calaveras River"=calaveras_river_fr_fry,
  "Cosumnes River"=cosumnes_river_fr_fry,
  "Mokelumne River"=mokelumne_river_fr_fry,
  "Merced River"=merced_river_fr_fry,
  "Stanislaus River"=stanislaus_river_fr_fry,
  "Tuolumne River"=tuolumne_river_fr_fry,
  "San Joaquin River"=san_joaquin_river_fr_fry
) %>% tidyr::gather(watershed, habitat, -c(year, month)) %>% 
  mutate(life_stage="fry", species = "fr")


#juv

fall_run_inchannel_juv_habitat <- bind_cols(
  year_month_df, 
  "Upper Sacramento"=upper_sacramento_fr_juv,
  "Antelope Creek"=antelope_creek_fr_juv, 
  "Battle Creek"=battle_creek_fr_juv, 
  "Bear Creek"=bear_creek_fr_juv, 
  "Big Chico Creek"=big_chico_fr_juv, 
  "Butte Creek"=butte_creek_fr_juv, 
  "Clear Creek"=clear_creek_fr_juv, 
  "Cottonwood Creek"=cottonwood_creek_fr_juv, 
  "Cow Creek"=cow_creek_fr_juv,
  "Deer Creek"=deer_creek_fr_juv, 
  "Elder Creek"=elder_creek_fr_juv, 
  "Mill Creek"=mill_creek_fr_juv,
  "Paynes Creek"=paynes_creek_fr_juv,
  "Stony Creek"=stony_creek_fr_juv,
  "Thomes Creek"=thomes_creek_fr_juv,
  "Upper-mid Sacramento River"=upper_mid_sacramento_fr_juv,
  "Sutter Bypass" = rep(NA, nrow(year_month_df)), #TODO make it better -emanuel
  "Bear River"=bear_river_fr_juv,
  "Feather River"=feather_river_fr_juv,
  "Yuba River"=yuba_river_fr_juv,
  "Lower-mid Sacramento River"=lower_mid_sacramento_river_fr_juv,
  "Yolo Bypass" = rep(NA, nrow(year_month_df)), #TODO make it better -emanuel
  "American River" = american_river_fr_juv,
  "Lower Sacramento River"=lower_sacramento_river_fr_juv,
  "Calaveras River"=calaveras_river_fr_juv,
  "Cosumnes River"=cosumnes_river_fr_juv,
  "Mokelumne River"=mokelumne_river_fr_juv,
  "Merced River"=merced_river_fr_juv,
  "Stanislaus River"=stanislaus_river_fr_juv,
  "Tuolumne River"=tuolumne_river_fr_juv,
  "San Joaquin River"=san_joaquin_river_fr_juv
) %>% tidyr::gather(watershed, habitat, -c(year, month)) %>% 
mutate(life_stage="juv", species = "fr")


inchannel_habitat <- bind_rows(
  fall_run_inchannel_fry_habitat,
  fall_run_inchannel_juv_habitat
)

devtools::use_data(inchannel_habitat, overwrite = TRUE)


# INCHANNEL FALL RUN SPAWNING Years: 1979 to 1999 -----------------------------

year_month_df_spawning <- tibble::as_tibble(expand.grid(year = 1979:1999, month = 1:12)) %>% 
  arrange(year, month)


get_flow_spawning <- purrr::partial(get_flow, years=c(1979,1999))


# upper sac (board in/out logic here)
upper_sac_flows_spawning <- get_flow_spawning("Upper Sacramento River")

# for fall run fry modeling does not exist so we use fall run juv
upper_sac_IN_fr_spawn_approx <- approxfun(cvpiaHabitat::upper_sac_ACID_boards_in$flow_cfs, 
                                        cvpiaHabitat::upper_sac_ACID_boards_in$FR_spawn_wua, rule=2)

upper_sac_OUT_fr_spawn_approx <- approxfun(cvpiaHabitat::upper_sac_ACID_boards_out$flow_cfs, 
                                         cvpiaHabitat::upper_sac_ACID_boards_out$FR_spawn_wua, rule=2)

upper_sac_spawning_input_df <- bind_cols(year_month_df_spawning, flows = upper_sac_flows_spawning)

upper_sac_spawning <- upper_sac_spawning_input_df %>% 
  mutate(habitat = case_when(
    month %in% 4:10 ~ upper_sac_IN_fr_spawn_approx(flows), 
    TRUE ~ upper_sac_OUT_fr_spawn_approx(flows)
  )) %>% pull(habitat)



# antelope 
antelope_creek_flows_spawning <- get_flow_spawning("Antelope Creek")

antelope_creek_spawning <- purrr::map_dbl(antelope_creek_flows_spawning, ~set_spawning_habitat("Antelope Creek", "fr", .))


# battle creek 
battle_creek_flows_spawning <- get_flow_spawning("Battle Creek")
battle_creek_spawning <- purrr::map_dbl(battle_creek_flows_spawning, 
                                          ~set_spawning_habitat("Battle Creek", "fr", .))


# bear creek 
bear_creek_flows_spawning <- get_flow_spawning("Bear Creek")
bear_creek_spawning <- purrr::map_dbl(bear_creek_flows_spawning, 
                                          ~set_spawning_habitat("Bear Creek", "fr", .))

# Big Chico creek 
big_chico_creek_flows_spawning <- get_flow_spawning("Big Chico Creek")
big_chico_creek_spawning <- purrr::map_dbl(big_chico_creek_flows_spawning, 
                                      ~set_spawning_habitat("Big Chico Creek", "fr", .))

# butte creek
butte_creek_flows_spawning <- get_flow_spawning("Butte Creek")
butte_creek_spawning <- purrr::map_dbl(butte_creek_flows_spawning, 
                                           ~set_spawning_habitat("Butte Creek", "fr", .))

# Clear creek
clear_creek_flows_spawning <- get_flow_spawning("Clear Creek")
clear_creek_spawning <- purrr::map_dbl(clear_creek_flows_spawning, 
                                       ~set_spawning_habitat("Clear Creek", "fr", .))



# Cottonwood creek
cottonwood_creek_flows_spawning <- get_flow_spawning("Cottonwood Creek")
cottonwood_creek_spawning <- purrr::map_dbl(cottonwood_creek_flows_spawning, 
                                       ~set_spawning_habitat("Cottonwood Creek", "fr", .))

# cow creek
cow_creek_flows_spawning <- get_flow_spawning("Cow Creek")
cow_creek_spawning <- purrr::map_dbl(cow_creek_flows_spawning, 
                                       ~set_spawning_habitat("Cow Creek", "fr", .))

# deer creek
deer_creek_flows_spawning <- get_flow_spawning("Deer Creek")
deer_creek_spawning <- purrr::map_dbl(deer_creek_flows_spawning, 
                                       ~set_spawning_habitat("Deer Creek", "fr", .))

# elder creek
elder_creek_flows_spawning <- get_flow_spawning("Elder Creek")
elder_creek_spawning <- purrr::map_dbl(elder_creek_flows_spawning, 
                                       ~set_spawning_habitat("Elder Creek", "fr", .))


# mill creek
mill_creek_flows_spawning <- get_flow_spawning("Mill Creek")
mill_creek_spawning <- purrr::map_dbl(mill_creek_flows_spawning, 
                                       ~set_spawning_habitat("Mill Creek", "fr", .))


# Paynes creek
paynes_creek_flows_spawning <- get_flow_spawning("Paynes Creek")
paynes_creek_spawning <- purrr::map_dbl(paynes_creek_flows_spawning, 
                                      ~set_spawning_habitat("Paynes Creek", "fr", .))

# stony  creek
stony_creek_flows_spawning <- get_flow_spawning("Stony Creek")
stony_creek_spawning <- purrr::map_dbl(stony_creek_flows_spawning, 
                                      ~set_spawning_habitat("Stony Creek", "fr", .))

# Thomes creek
thomes_creek_flows_spawning <- get_flow_spawning("Thomes Creek")
thomes_creek_spawning <- purrr::map_dbl(thomes_creek_flows_spawning, 
                                      ~set_spawning_habitat("Thomes Creek", "fr", .))

# Upper-mid Sacramento River
# According to modeling exists, this should be NA!
upper_mid_sac_flows_spawning <- get_flow_spawning("Upper-mid Sacramento River")
upper_mid_sac_spawning <- set_spawning_habitat("Upper-mid Sacramento River", "fr", upper_mid_sac_flows_spawning)
upper_mid_sac_spawning <- rep(NA, nrow(year_month_df_spawning))

# Sutter Bypass --- PASS
sutter_bypass_spawning <- rep(NA, nrow(year_month_df_spawning))

# Bear River
bear_river_flows_spawning <- get_flow_spawning("Bear River")
bear_river_spawning <- set_spawning_habitat("Bear River", "fr", bear_river_flows_spawning)

# Feather River
feather_river_flows_spawning <- get_flow_spawning("Feather River")
feather_river_spawning <- set_spawning_habitat("Feather River", "fr", feather_river_flows_spawning)

# Yuba River
yuba_river_flows_spawning <- get_flow_spawning("Yuba River")
yuba_river_spawning <- set_spawning_habitat("Yuba River", "fr", yuba_river_flows_spawning)

# lower-mid sacramento
# according to the modeling exists this should be NA!
lower_mid_sac_spawning_flows1 <- get_flow_spawning("Lower-mid Sacramento River1")
lower_mid_sac_spawning_flows2 <- get_flow_spawning("Lower-mid Sacramento River2")


lower_mid_sac_spawning_1 <- set_spawning_habitat("Lower-mid Sacramento River", "fr", 
                                                 lower_mid_sac_spawning_flows1)
lower_mid_sac_spawning_2 <- set_spawning_habitat("Lower-mid Sacramento River", "fr", 
                                                 lower_mid_sac_spawning_flows2)
lower_mid_sac_spawnning <- rep(NA, nrow(year_month_df_spawning))

# Yolo Bypass 
yolo_bypass_spawning <- rep(NA, nrow(year_month_df_spawning))

# American River
american_river_spawning_flows <- get_flow_spawning("American River")
american_river_spawning <- set_spawning_habitat("American River", "fr", 
                                                american_river_spawning_flows)


# lower sacramento River 
lower_sacramento_river_spawning <- rep(NA, nrow(year_month_df_spawning))

# Calaveras River
calaveras_river_spawning_flows <- get_flow_spawning("Calaveras River")
calaveras_river_spawning <- set_spawning_habitat("Calaveras River", "fr", 
                                                calaveras_river_spawning_flows)


# Cosumnes River
# Warning: only 2 approx functions within the region used for estimate
cosumnes_river_spawning_flows <- get_flow_spawning("Cosumnes River")
cosumnes_river_spawning <- purrr::map_dbl(cosumnes_river_spawning_flows, 
                                       ~set_spawning_habitat("Cosumnes River", "fr", .))



# mokelumne 
mokelumne_river_spawning_flows <- get_flow_spawning("Mokelumne River")
mokelumne_river_spawning <- set_spawning_habitat("Mokelumne River", "fr", 
                                                 mokelumne_river_spawning_flows)


# merced 
merced_river_spawning_flows <- get_flow_spawning("Merced River")
merced_river_spawning <- set_spawning_habitat("Merced River", "fr", 
                                                 merced_river_spawning_flows)

# stanislaus
stanislaus_river_spawning_flows <- get_flow_spawning("Stanislaus River")
stanislaus_river_spawning <- set_spawning_habitat("Stanislaus River", "fr", 
                                                 stanislaus_river_spawning_flows)

# tuolumne 
tuolumne_river_spawning_flows <- get_flow_spawning("Tuolumne River")
tuolumne_river_spawning <- set_spawning_habitat("Tuolumne River", "fr",
                                                tuolumne_river_spawning_flows)

# san joaquin is NA
san_joaquin_river_spawning <- rep(NA, nrow(year_month_df_spawning))

fall_run_inchannel_spawning_habitat <-  bind_cols(
  year_month_df_spawning,
  "Upper Sacramento River" = upper_sac_spawning,
  "Antelope Creek" = antelope_creek_spawning,
  "Battle Creek" = battle_creek_spawning, 
  "Bear Creek" = bear_creek_spawning,
  "Big Chico Creek" = big_chico_creek_spawning,
  "Butte Creek" = butte_creek_spawning,
  "Clear Creek" = clear_creek_spawning,
  "Cottonwood Creek" = cottonwood_creek_spawning,
  "Cow Creek" = cow_creek_spawning,
  "Deer Creek" = deer_creek_spawning,
  "Elder Creek" = elder_creek_spawning,
  "Mill Creek"= mill_creek_spawning,
  "Paynes Creek" = paynes_creek_spawning,
  "Stony Creek" = stony_creek_spawning,
  "Thomes Creek" = thomes_creek_spawning,
  "Upper-mid Sacramento River" = upper_mid_sac_spawning,
  "Sutter Bypass" = sutter_bypass_spawning,
  "Bear River" = bear_river_spawning,
  "Feather River" = feather_river_spawning,
  "Yuba River" = yuba_river_spawning,
  "Lower-mid Sacramento River" = lower_mid_sac_spawnning,
  "Yolo Bypass" = yolo_bypass_spawning,
  "American River" = american_river_spawning,
  "Lower Sacramento River" = lower_sacramento_river_spawning,
  "Calaveras River" = calaveras_river_spawning,
  "Cosumnes River" = cosumnes_river_spawning,
  "Mokelumne River" = mokelumne_river_spawning,
  "Merced River" = merced_river_spawning,
  "Stanislaus River" = stanislaus_river_spawning,
  "Tuolumne River" = tuolumne_river_spawning,
  "San Joaquin River" = san_joaquin_river_spawning
) %>% 
  tidyr::gather(watershed, habitat, -c(year, month)) %>% 
  mutate(species = "fr")


inchannel_spawning_habitat <- bind_rows(
  fall_run_inchannel_spawning_habitat
)


devtools::use_data(inchannel_spawning_habitat, overwrite = TRUE)

# cvpiaData::inchannel_spawning_habitat %>% 
#   mutate(date = ymd(paste(year, month, 1, sep = '-'))) %>% 
#   select(date, watershed, habitat) %>% 
#   spread(date, habitat) %>% View
# 
# wat <- cvpiaData::inchannel_spawning_habitat$watershed %>% unique
# cvpiaData::watershed_ordering$watershed[!(cvpiaData::watershed_ordering$watershed %in% wat)]
# 
# cvpiaData::inchannel_habitat
# am <- year_month_df
# am$watershd = 'American River'
# 
# amj <- am
# am$habitat = american_river_fr_fry
# am$life_stage = 'fry'
# am$species = 'fr'
# 
# amj$habitat = american_river_fr_juv
# amj$life_stage = 'juv'
# amj$species = 'fr'
# 
# bind_rows(am, amj)
