watershed_ordering <- readr::read_csv('data-raw/All inputs.csv') %>%
  dplyr::select(order = Order, watershed = Watershed)

devtools::use_data(watershed_ordering)

misc_data <- read_csv('data-raw/All inputs.csv') %>% 
  select(order = Order, watershed = Watershed, init.adult, SCDELT, hatch.alloc, 
         TISD, YOLO, p.tempMC2025, A.HARV, P.scour.nst, P.strand.early, P.strand.late, 
         High.pred, contact, prop.nat)

use_data(misc_data, overwrite = TRUE)

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

