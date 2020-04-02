library(tidyverse)

#TODO 
# we want values for each of the watersheds these are here: 
example <- cvpiaData::watershed_ordering

# * the dataset has watersheds under the location column-- ADDRESSED
# * battle creek is split into different locations in the dataset please consolidate into one ---
# * rename mainstem as upper sacramento river 
# * keep only the watersheds that are in the watershed_ordering dataset above
# * the values you get here should be somewhat similar to the SimulationSeed_FallRunAdults.csv that I just added

#--------------------------------------------------------------------------------------------------------------------------------------
#Read in csv
grandtab_raw <- read_csv("data-raw/grandtab/Grandtab_Modified.csv")

#Select columns
grandtab_column_clean <- select(grandtab_raw, "location", "timeperiod", "count")
 
#Filter data for last five years (2013 to 2017)
grandtab_yr_filter <- filter(grandtab_column_clean, timeperiod >= '2013', timeperiod <= '2017')

#Mutate "adds" column, if you give it existing column (location), it will overwrite
#If "Battle Creek" is anywhere in location column (old), then new column is renamed to just "Battle Creek"
#Otherwise, it's whatever it was before (location old)
grandtab_transform_1 <- mutate(grandtab_yr_filter, location = ifelse(grepl("Battle Creek", location), "Battle Creek", location) )

#Mutate "adds" column, if you give it existing column (location), it will overwrite
#If "Mainstem" is anywhere in location column (old), then new column is renamed to just "Upper Sacramento River"
#Otherwise, it's whatever it was before (location old)

grandtab_transform_2 <- mutate(grandtab_transform_1, location = ifelse(grepl("Mainstem", location), "Upper Sacramento River", location) )

#From grandtab_transform_2, using location column and from cvpiaData::watershed_ordering, using watershed column
#Semi_join: Only take rows and columns from left dataset (grandtab_transform_2) that appear in cvpiaData::watershed_ordering

grandtab_joined <- left_join(grandtab_transform_2, cvpiaData::watershed_ordering, by = c("location" = "watershed")) %>% 
  filter(!is.na(order)) %>% 
  transmute(order, watershed = location, year = timeperiod, seed = round(count)) %>% 
  arrange(order, year)

#Aggregating count by location
#Give the mean count per location 
#na.rm=TRUE, add  to ignore NA values when calculating the mean
grandtab_clean <- grandtab_joined %>% 
  group_by(watershed) %>% 
  summarise(
    order = first(order),
    seed = mean(seed, na.rm = TRUE)
  ) %>% 
  arrange(order)


adam_version <- read_csv("data-raw/grandtab/SimulaitonSeed_FallRunAdults.csv")


  
#For adding to package
# usethis::use_data(grandtab_clean, overwrite=TRUE)



