library(tidyverse)

#TODO 
# we want values for each of the watersheds these are here: 
cvpiaData::watershed_ordering

# * the dataset has watersheds under the location column
# * battle creek is split into different locations in the dataset please consolidate into one
# * rename mainstem as upper sacramento river 
# * keep only the watersheds that are in the watershed_ordering dataset above
# * the values you get here should be somewhat similar to the SimulationSeed_FallRunAdults.csv that I just added

grandtab_raw <- read_csv("data-raw/grandtab/Grandtab_Modified.csv")

grandtab_column_clean <- select(grandtab_raw, "majorbasin", "timeperiod", "count")
 
grandtab_yr_filter <- filter(grandtab_column_clean, timeperiod >= '2013', timeperiod <= '2017')

grandtab_clean <- aggregate(grandtab_yr_filter$count, by=list(grandtab_yr_filter$majorbasin), FUN=mean, na.rm=TRUE)




  

# usethis::use_data(grandtab_clean, overwrite=TRUE)



