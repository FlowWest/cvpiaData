library(devtools)
library(tidyverse)
library(grandTab)


watershed_ordering <- readr::read_csv('data-raw/All inputs.csv') %>%
  dplyr::select(order = Order, watershed = Watershed)

devtools::use_data(watershed_ordering)

grandTab::grandtab %>% 
  filter(year >= 2010, type == 'Natural') %>% 
  mutate(watershed = replace(watershed, watershed == 'Upper-mid Sacramento River', 'Upper Sacramento River')) %>% 
  group_by(watershed) %>% 
  summarise(`2010-2015 mean` = mean(count, na.rm = TRUE)) %>% 
  full_join(watershed_ordering) %>% 
  arrange(order) %>% 
  left_join(misc_data) %>% 
  select(watershed, `previous value` = init.adult, `2010-2015 mean`) %>% 
  gather(type, count, - watershed) %>% 
  ggplot(aes(x = factor(watershed, levels = watershed_ordering$watershed), y = count, fill = type)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  theme_minimal() +
  labs(x = '', y = 'initial adults') +
  scale_fill_brewer(palette = 'Set2') +
  theme(text = element_text(size = 20))


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
  select(order = Order, watershed = Watershed, init.adult, SCDELT, hatch.alloc, 
         TISD, YOLO, p.tempMC2025, A.HARV, P.scour.nst, P.strand.early, P.strand.late, 
         High.pred, contact, prop.nat.remov) %>% 
    mutate(run = 'fall')

wr <- read_csv('data-raw/All inputs WINTER RUN.csv')  %>% 
  select(order = Order, watershed = Watershed, init.adult, SCDELT, hatch.alloc, 
         TISD, YOLO, p.tempMC2025, A.HARV, P.scour.nst, P.strand.early, P.strand.late, 
         High.pred, contact, prop.nat.remov) %>% 
  mutate(run = 'winter')

sr <- read_csv('data-raw/All inputs SPRING RUN.csv')  %>% 
  select(order = Order, watershed = Watershed, init.adult, SCDELT, hatch.alloc, 
         TISD, YOLO, p.tempMC2025, A.HARV, P.scour.nst, P.strand.early, P.strand.late, 
         High.pred, contact, prop.nat.remov) %>% 
  mutate(run = 'spring')

misc_data <- bind_rows(fr, wr, sr)

use_data(misc_data, overwrite = TRUE)
