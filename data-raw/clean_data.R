library(devtools)
library(tidyverse)
library(grandTab)
library(usethis)


watershed_ordering <- readr::read_csv('data-raw/All inputs.csv') %>%
  dplyr::select(order = Order, watershed = Watershed)

usethis::use_data(watershed_ordering)

prev_escap <- read_csv('data-raw/All inputs.csv') %>% 
  select(watershed = Watershed, init.adult)

# Feather and Yuba Rivers do not seperate spring and fall run in totals, 
# here we apply the proportion of spring hatchery return adults that are used to 
# adjust the totals for 2010 - 2012 Data used were from GrandTab 2017 version
prop_spring <- mean(c(0.076777295, 0.056932196, 0.081441457)) #2010-2012 

 fall_run_init_adult <- grandTab::grandtab %>% 
  filter(year >= 2010, type == 'Natural', run == 'Fall') %>% 
  mutate(watershed = replace(watershed, watershed == 'Upper-mid Sacramento River', 'Upper Sacramento River')) %>% 
  group_by(watershed) %>% 
  summarise(mean_escap = mean(count, na.rm = TRUE)) %>% 
  full_join(watershed_ordering) %>% 
  arrange(order) %>% 
  ungroup() %>% 
  left_join(prev_escap) %>% 
  mutate(count = case_when(
    init.adult == 0 ~ as.numeric(NA),
    mean_escap < 10 | is.na(mean_escap) ~ init.adult,
    watershed %in% c('Yuba River', 'Feather River') ~ mean_escap * (1 - prop_spring),
    TRUE ~ mean_escap
  )) %>% 
  select(watershed, init.adult = count)

feat_yuba_fall <-  fall_run_init_adult %>% 
  filter(watershed %in% c('Yuba River', 'Feather River'))
 
has_spring_run <- cvpiaHabitat::modeling_exist %>% 
  filter(!is.na(SR_spawn)) %>% 
  pull(Watershed)

spring_run_init_adult <- grandTab::grandtab %>%
  filter(year >= 2010, type == 'Natural', run == 'Spring') %>% 
  mutate(watershed = replace(watershed, watershed == 'Upper-mid Sacramento River', 'Upper Sacramento River')) %>% 
  group_by(watershed) %>% 
  summarise(mean_escap = mean(count, na.rm = TRUE)) %>% 
  full_join(watershed_ordering) %>% 
  arrange(order) %>% 
  ungroup() %>% 
  mutate(count = case_when(
    watershed == 'Yuba River' ~ feat_yuba_fall$init.adult[2] * prop_spring,
    watershed == 'Feather River' ~ feat_yuba_fall$init.adult[1] * prop_spring,
    watershed %in% has_spring_run & is.na(mean_escap) ~ 49,
    watershed %in% has_spring_run & mean_escap < 10 ~ 49,
    watershed %in% has_spring_run ~ mean_escap,
    TRUE ~ as.numeric(NA)
  )) %>% 
  select(watershed, init.adult = count)

# source: https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=84381&inline=1 pg 7
winter_run_init_adult <- tibble(
  watershed = rep('Upper Sacramento River', 7),
  count = c(1533, 738, 2578, 5920, 2627, 3182, 1409)) %>% 
  group_by(watershed) %>% 
  summarise(init.adult = mean(count)) %>% 
  full_join(watershed_ordering)
  
  
# init.adult update for new date range TODO
# SCDELT
# hatch.alloc
# TISD
# YOLO
# p.tempMC2025 - with degday script
# A.HARV - 3 reports summarized for the whole years available period, Dan Kratvile :)
# P.scour.nst - expert opinion, could improve with flow + sheer
# P.strand.early - need function
# P.strand.late - need function
# High.pred
# contact
# prop.nat

#DegDay -temperature, yearly value per watershed
#egg.tmp.eff -temperature?, one value per watershed Chris hammersmark

fr <- read_csv('data-raw/All inputs FALL RUN.csv')  %>% 
  select(order = Order, sort = Srt, watershed = Watershed, SCDELT, hatch.alloc, 
         TISD, YOLO, p.tempMC2025, A.HARV, P.scour.nst, P.strand.early, P.strand.late, 
         High.pred, contact, prop.nat.remov) %>% 
    mutate(run = 'fall') %>% 
  left_join(fall_run_init_adult)

wr <- read_csv('data-raw/All inputs WINTER RUN.csv')  %>% 
  select(order = Order, sort = Srt, watershed = Watershed, SCDELT, hatch.alloc, 
         TISD, YOLO, p.tempMC2025, A.HARV, P.scour.nst, P.strand.early, P.strand.late, 
         High.pred, contact, prop.nat.remov) %>% 
  mutate(run = 'winter') %>% 
  left_join(winter_run_init_adult)

sr <- read_csv('data-raw/All inputs SPRING RUN.csv')  %>% 
  select(order = Order, sort = Srt, watershed = Watershed, SCDELT, hatch.alloc, 
         TISD, YOLO, p.tempMC2025, A.HARV, P.scour.nst, P.strand.early, P.strand.late, 
         High.pred, contact, prop.nat.remov) %>% 
  mutate(run = 'spring') %>% 
  left_join(spring_run_init_adult)

misc_data <- bind_rows(fr, wr, sr)

use_data(misc_data, overwrite = TRUE)




