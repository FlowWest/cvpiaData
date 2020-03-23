# create input for known adult seeds 


# Known Adult Seeds --------------------------------------------------------------
# the model is seeded by getting known adults at watersheds for each 
# fall spring and winter.

grandtab_raw <- read_csv("data-raw/grandtab/Grandtab_Modified.csv", skip = 1,
                         col_names = c("species", "run", "majorbasin",
                                       "minorbasin", "watershed", "timeperiod",
                                       "startmonth", "startyear", "endmonth",
                                       "year", "count", "data_status",
                                       "source", "data_level", "record_type",
                                       "div_group", "count_type"))


# FALL RUN -----

grandtab_fall_raw <- grandtab_raw %>% 
  filter(count_type == "In-River", 
         run == "Fall", 
         watershed != "All") %>% 
  select(run, year, watershed, count)

# feather and yuba do not seperate counts as is needed for the model.
# TODO come up with a better method than this eventually
# use proportions for when the seperation was done year (2010-2012) 
# extrapolate this proportion to all years needed in the model 

# proportions for known years (2010, 2011, 2012)
yuba_feather_prop_sr <- mean(c(0.076777295, 0.056932196, 0.081441457))
yuba_feather_prop_fr <- 1 - yuba_feather_prop_sr

yuba_and_feather_computed <- grandtab_raw %>% 
  filter(watershed %in% c("Yuba River", "Feather River"), 
         run %in% c("Spring", "Fall"), 
         count_type == "In-River") %>% 
  group_by(watershed, year) %>% 
  summarise(
    fall_run_total = yuba_feather_prop_fr * sum(count, na.rm = TRUE)
  ) %>% ungroup() %>% 
  transmute(
    run = "Fall", 
    year, 
    watershed,
    count = fall_run_total
  )

# add in these new computed values instead of the old ones
grandtab_fall_raw_2 <- grandtab_fall_raw %>% 
  filter(!(watershed %in% c("Yuba River", "Feather River"))) %>% 
  bind_rows(yuba_and_feather_computed)
  

