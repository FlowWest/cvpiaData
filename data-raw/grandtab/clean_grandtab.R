library(tidyverse)

# note in order to check work here I am working along with the `grandTab` object
# that lives in the processGrandTab.R script

gt <- read_csv('data-raw/grandtab/Grandtab_Modified_sacpass.csv')
adamg_gt_final <- read_csv('data-raw/grandtab/Calibration2018_NaturalAdults_FallRun_NoFillIns.csv')
hatchery_expansion <- read_csv("data-raw/grandtab/hatchery expansion.csv", 
                               col_names = c("trib", "hatchery", "natural", "total_escape", "year", "no_hatch_ret", 
                                             "source"), 
                               skip = 1)

sacpas_fall_run <- gt %>% 
  filter(
    type == "In-River", 
    run == "Fall", 
    location != "All"
  ) %>% 
  select(
    run, watershed = location, minorbasin, year = endyear, count
  )


# Yuba/Feather Imputation ------------------------------------------------------

# from Adam's script: Feather and Yuba Rivers do not seperate spring and fall run
# here uses years 2010-2012 props of fall run to obtain values for all other years

prop_sr <- tibble(
  prp=c(0.076777295, 0.056932196, 0.081441457), 
  Year=c(2010,2011,2012)
)

# prop of spring run
prp <-mean(prop.spring[,1])

sacpas_yuba_feather <- gt %>% 
  filter(
    location %in% c("Yuba River", "Feather River"), 
    type == "In-River", 
    run %in% c("Spring", "Fall")
  ) %>% 
  select(location, minorbasin, year=endyear, run, count)

# sum over each watershed and year to get total Fall+Spring counts
yuba_feather_computed_counts <- sacpas_yuba_feather %>% 
  group_by(location, year) %>% 
  summarise(count = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    count = ceiling(count * (1 - prp)), 
    run = "Fall"
  ) %>% 
  rename(watershed = location)

sacpas_fall_imputed <- sacpas_fall_run %>% 
  filter(
    !(watershed %in% c("Yuba River", "Feather River"))
  ) %>% 
  bind_rows(yuba_feather_computed_counts)

# making sure counts match up with the processing from Adam's script
sacpas_fall_imputed %>% filter(watershed == "Feather River", between(year, 1998, 2018)) %>% 
  pull(count) %>% sum()

stuff %>% filter(watershed == "Feather River") %>% pull(count) %>% sum()


# Fix duplicate watersheds -----------------------------------------------------

# battle creek appears as two watersheds we can tream them as one
# the mainstem is the upper sacramento river, fix them both here
sacpas_names_fixed <- sacpas_fall_imputed %>% 
  mutate(watershed = case_when(
    str_detect(watershed, "Battle Creek") ~ "Battle Creek", 
    watershed == "Mainstem" ~ "Upper Sacramento River",
    TRUE ~ watershed
  ))

# id the duplicate watersheds; these will have total > 1 
sacpas_names_fixed %>% 
  group_by(watershed, year) %>% 
  summarise(
    total = n()
  ) %>% ungroup() %>% 
  filter(total > 1) %>% 
  distinct(watershed)

# the solution implemented by Adam is to just sum these up 
sacpas_dups_fixed <- sacpas_names_fixed %>% 
  group_by(run, watershed, year) %>% 
  summarise(
    count = sum(count, na.rm = TRUE)
  ) %>% ungroup()

# check against adam's counts; need to run grandtab object up to line 73
grandTab %>% filter(watershed == "Upper Sacramento River") %>% pull(count) %>% sum()
sacpas_dups_fixed %>% filter(watershed == "Upper Sacramento River", 
                             between(year, 1998, 2018)) %>% pull(count) %>% sum()

grandTab %>% filter(watershed == "Battle Creek") %>% pull(count) %>% sum()
sacpas_dups_fixed %>% filter(watershed == "Battle Creek", 
                             between(year, 1998, 2018)) %>% pull(count) %>% sum()


# Set Hatchery Fish ------------------------------------------------------------

all_watersheds <- cvpiaData::load_baseline_data("fall")

hatchery_exp_real <- 
  hatchery_expansion %>% filter(source != "made up")

# this dataframe gets written out as a csv
proportion_hatched <- hatchery_exp_real %>% 
  group_by(watershed = trib) %>% 
  summarise(
    prop_hatched = mean(hatchery)
  ) %>% 
  right_join(select(all_watersheds$inps, order, sort, watershed)) %>% 
  mutate(prop_hatched = ifelse(is.na(prop_hatched), mean(prop_hatched, na.rm = TRUE), 
                              prop_hatched)) %>% 
  arrange(order) %>% 
  select(watershed, order, sort, prop.Hatch = prop_hatched)

# test this matches with Adam's should return 0
sum(prop.hatch$prop.Hatch != proportion_hatched$prop.Hatch)


# Fill in missing data ---------------------------------------------------------

