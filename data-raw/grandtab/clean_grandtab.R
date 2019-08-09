library(tidyverse)

gt <- read_csv('data-raw/grandtab/Grandtab_Modified_sacpass.csv')

glimpse(gt)
gt %>% 
  filter(location != 'All', run == 'Fall', type == 'In-River') %>% 
  rename(year = endyear) %>% 
  group_by(location, year) %>% 
  summarise(count = sum(count, na.rm = TRUE)) %>% 
  spread(year, count) %>% View
  

# between(endyear, 1998, 2017)

gt %>% 
  filter(minorbasin != 'All', between(endyear, 1998, 2017), run == 'Fall', 
         location == 'American River') %>% View
  select(location, endyear, count)

library(grandTab)
grandtab %>% 
  filter(between(year, 1998, 2017), run == 'Fall', watershed == 'American River')
