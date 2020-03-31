install.packages("tidyverse")
library(tidyverse)

grandtab_raw <- read_csv("data-raw/grandtab/Grandtab_Modified.csv")

grandtab_column_clean <- select(grandtab_raw, "majorbasin", "timeperiod", "count")
 
grandtab_yr_filter <- filter(grandtab_column_clean, timeperiod >= '2013', timeperiod <= '2017')

grandtab_clean <- aggregate(grandtab_yr_filter$count, by=list(grandtab_yr_filter$majorbasin), FUN=mean, na.rm=TRUE)




  

usethis::use_data(grandtab_clean, overwrite=TRUE)



