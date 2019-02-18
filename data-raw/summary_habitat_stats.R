wo <- cvpiaData::watershed_ordering
wo$fr_spw <- cvpiaHabitat::square_meters_to_acres(apply(cvpiaData::fr_spawn[ ,10:12, ], 1, median))
wo$fr_fry <- cvpiaHabitat::square_meters_to_acres(apply(cvpiaData::fr_fry[ ,1:6, ], 1, median))
wo$fr_juv <- cvpiaHabitat::square_meters_to_acres(apply(cvpiaData::fr_juv[ , 1:6, ], 1, median))
# write.csv(wo, 'median_hab_fall_run_acres.csv')


library(cvpiaHabitat)
library(tidyverse)
