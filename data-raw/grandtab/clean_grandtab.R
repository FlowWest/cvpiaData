library(tidyverse)

# note in order to check work here I am working along with the `grandTab` object
# that lives in the processGrandTab.R script

gt <- read_csv('data-raw/grandtab/Grandtab_Modified_sacpass.csv')
hatchery_expansion <- read_csv("data-raw/grandtab/hatchery expansion.csv", 
                               col_names = c("trib", "hatchery", "natural", "total_escape", "year", "no_hatch_ret", 
                                             "source"), 
                               skip = 1)

watersheds <- cvpiaData::watershed_ordering$watershed

# FALL RUN ===========================================================================

sacpas_fall_run <- gt %>% 
  filter(
    type == "In-River", 
    run == "Fall", 
    location != "All"
  ) %>% 
  select(run, watershed = location, 
         minorbasin, year = endyear, count)


# Yuba/Feather Imputation ------------------------------------------------------

# from Adam's script: Feather and Yuba Rivers do not seperate spring and fall run
# here uses years 2010-2012 props of fall run to obtain values for all other years

prop_sr <- tibble(
  prp=c(0.076777295, 0.056932196, 0.081441457), 
  Year=c(2010,2011,2012)
)

# prop of spring run
prp <-mean(prop_sr$prp)

sacpas_yuba_feather <- gt %>% 
  filter(
    location %in% c("Yuba River", "Feather River"), 
    type == "In-River", 
    run %in% c("Spring", "Fall"), 
    !is.na(count)
  ) %>% 
  select(location, minorbasin, year=endyear, run, count)

# sum over each watershed and year to get total Fall+Spring counts
yuba_feather_computed_counts <- sacpas_yuba_feather %>% 
  group_by(location, year) %>% 
  summarise(count = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    count = ceiling(count * (1 - prp)), # apply known fall run proportion
    run = "Fall"
  ) %>% 
  rename(watershed = location)

sacpas_fall_imputed <- sacpas_fall_run %>% 
  filter(
    run == "Fall",
    !(watershed %in% c("Yuba River", "Feather River")) # remove old ones
  ) %>% 
  bind_rows(yuba_feather_computed_counts) %>% # add in the new ones 
  select(run, watershed, year, count)


# making sure counts match up with the processing from Adam's script
sacpas_fall_imputed %>% filter(watershed == "Feather River", between(year, 1998, 2018)) %>% 
  pull(count) %>% sum()


stuff %>% filter(watershed == "Feather River") %>% pull(count) %>% sum()


# Fix duplicate watersheds -----------------------------------------------------

# battle creek appears as two watersheds we can tream them as one
# the mainstem is the upper sacramento river, fix them both here
sacpas_batcreek_upsac <- sacpas_fall_imputed %>% 
  filter(
    str_detect(watershed, "Battle Creek") | watershed == "Mainstem"
  ) %>% 
  mutate(watershed = case_when(
    str_detect(watershed, "Battle Creek") ~ "Battle Creek", 
    watershed == "Mainstem" ~ "Upper Sacramento River"
  )) %>% 
  group_by(run, watershed, year) %>% 
  summarise(
    count = sum(count, na.rm = TRUE)
  ) %>% ungroup()

# confirm values match up with Adam's
# should be 957790
sacpas_batcreek_upsac %>% filter(watershed == "Battle Creek", 
                                 between(year, 1998, 2018)) %>% pull(count) %>% sum()
# should be 865218
sacpas_batcreek_upsac %>% filter(watershed == "Upper Sacramento River", 
                                 between(year, 1998, 2018)) %>% pull(count) %>% sum()

# the solution implemented by Adam is to just sum these up 
sacpas_dups_fixed <- sacpas_fall_imputed %>% 
  filter(
    !(str_detect(watershed, "Battle Creek") | watershed == "Mainstem")
  ) %>% 
  bind_rows(sacpas_batcreek_upsac)

# check again 
# should be 957790
sacpas_dups_fixed %>% filter(watershed == "Battle Creek", 
                                 between(year, 1998, 2018)) %>% pull(count) %>% sum()
# should be 865218
sacpas_dups_fixed %>% filter(watershed == "Upper Sacramento River", 
                                 between(year, 1998, 2018)) %>% pull(count) %>% sum()


# there are some years that are just left out, here I introduce these back into 
# the dataset so we can count them as missing and fill them in later


# 2017-1975 is 43
sacpas_dups_fixed %>% 
  filter(watershed %in% cvpiaData::watershed_ordering$watershed) %>%
  group_by(watershed) %>% 
  summarise(years_of_record = n_distinct(year)) %>% 
  filter(years_of_record < 42)

year_to_watershed_expected <- 
  expand.grid(
    watershed = unique(sacpas_dups_fixed$watershed), 
    year = 1975:2017, 
    stringsAsFactors = FALSE
  ) %>% arrange(watershed)

shed_escapees <- 
  year_to_watershed_expected %>% 
  left_join(sacpas_dups_fixed %>% filter(watershed %in% cvpiaData::watershed_ordering$watershed)) %>% 
  as_tibble() %>% 
  fill(run) %>% 
  filter(watershed %in% cvpiaData::watershed_ordering$watershed)

# should now be 0
shed_escapees %>% 
  filter(watershed %in% cvpiaData::watershed_ordering$watershed) %>%
  group_by(watershed) %>% 
  summarise(years_of_record = n_distinct(year)) 

# Set Hatchery Fish ------------------------------------------------------------

all_watersheds <- cvpiaData::load_baseline_data("fall")

hatchery_exp_real <- 
  hatchery_expansion %>% filter(source != "made up")

# this dataframe gets written out as a csv
prop_hatch <- hatchery_exp_real %>% 
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
sum(prop.hatch$prop.Hatch != prop_hatch$prop.Hatch)



usethis::use_data(prop_hatch, overwrite = TRUE)


# Fill in missing data ---------------------------------------------------------
# the overall processing task is to get total number of valley of escapees for years
# of complete record (no missing data on any trib), use this to get the proportion
# of escapees from each of the tribs
prop_escapees_in_complete_years <- shed_escapees %>% 
  group_by(year) %>% 
  summarise(
    missing_years = sum(is.na(count)), 
    valley_total = sum(count, na.rm = TRUE)) %>% 
  filter(missing_years == 2) %>% 
  left_join(shed_escapees, by = c("year" = "year")) %>% 
  mutate(prop_escapees = count/valley_total) %>% 
  group_by(watershed) %>% 
  summarise(
    avg_prop_escapees = mean(prop_escapees)
  )


reference_escapees <- shed_escapees %>% 
  filter(watershed == "Upper Sacramento River") %>% 
  left_join(prop_escapees_in_complete_years) %>% 
  mutate(total_escapees = ceiling(count/avg_prop_escapees)) %>% 
  select(
    year, total_escapees
  )


# plot against Adam's reference line
ggplot() + 
  geom_line(data=reference_escapees, aes(year, total_escapees)) + 
  geom_line(data=tot.escape, aes(year, total.Escape, color="adam"))


estimated_watershed_escapees <- tibble(
  watershed = all_watersheds$inps$watershed,
  order = all_watersheds$inps$order, 
  s_hab = all_watersheds$IChab.spawn[, 1, 1]
) %>% 
  filter(!is.na(s_hab)) %>% 
  pull(watershed) %>% 
  expand.grid(watershed = ., year = 1975:2017, stringsAsFactors = FALSE) %>% 
  left_join(prop_escapees_in_complete_years) %>% 
  arrange(watershed) %>% 
  as_tibble() %>% 
  mutate(avg_prop_escapees = ifelse(is.na(avg_prop_escapees), 
                                    min(avg_prop_escapees, na.rm = TRUE), 
                                    avg_prop_escapees)) %>% 
  left_join(reference_escapees) %>% 
  left_join(shed_escapees) %>% 
  mutate(
    estimated_count = ifelse(is.na(count), 
                             round(total_escapees*avg_prop_escapees, 0), 
                             count)
  ) %>% 
  left_join(cvpiaData::watershed_ordering) %>% 
  select(order, watershed, year, estimated_count, raw_count = count)


estimated_watershed_escapees %>% 
  filter(watershed == "American River") %>% 
  ggplot(aes(year, estimated_count, color=watershed)) + geom_line()


# same watersheds?
adam_ws <- unique(all.comb$watershed)
ws <- unique(estimated_watershed_escapees$watershed)
setdiff(adam_ws, ws) # good
setdiff(ws, adam_ws) # good

# good means these are all zeros
ws %>% 
  map_int(function(w) {
    x <- all.comb %>% 
      filter(watershed == w) %>% 
      pull(est.count)
    
    y <- estimated_watershed_escapees %>% 
      filter(watershed == w, between(year, 1998, 2017)) %>% 
      pull(estimated_count)
    
    sum(x != y)
  })


# Fill in Matrix --------------------------------------------------------------
fr_known_adults <- estimated_watershed_escapees %>% 
  mutate(
    estimated_count = ifelse(
      watershed %in% c("Upper-mid Sacramento River", "Sutter Bypass", 
                       "Lower-mid Sacramento River", "Yolo Bypass", 
                       "Lower Sacramento River", "San Joaquin River"), 
      0, estimated_count
    )
  ) %>% 
  select(order, watershed, year, estimated_count) %>%  
  filter(between(year, 1980, 1999)) %>% 
  spread(year, estimated_count)


usethis::use_data(fr_known_adults, overwrite = TRUE)


# SPRING RUN ===================================================================

sacpas_spring_run <- gt %>% 
  filter(
    type == "In-River", 
    run == "Spring", 
    location != "All"
  ) %>% 
  select(run, watershed = location, 
         minorbasin, year = endyear, count)


# for feather and yuba apply the same processing as fall run but use proportion to extract spring run  

prop_sr <- tibble(
  prp=c(0.076777295, 0.056932196, 0.081441457), 
  Year=c(2010,2011,2012)
)

# prop of spring run
prp <-mean(prop_sr$prp)

sacpas_yuba_feather_spring <- gt %>% 
  filter(
    location %in% c("Yuba River", "Feather River"), 
    type == "In-River", 
    run %in% c("Spring", "Fall"), 
    !is.na(count)
  ) %>% 
  select(location, minorbasin, year=endyear, run, count)

# sum over each watershed and year to get total Fall+Spring counts
yuba_feather_computed_counts_spring <- sacpas_yuba_feather_spring %>% 
  group_by(location, year) %>% 
  summarise(count = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    count = ceiling(count * (prp)), # apply known spring run proportion
    run = "Spring"
  ) %>% 
  rename(watershed = location)


sacpas_spring_imputed <- sacpas_spring_run %>% 
  filter(
    run == "Spring",
    !(watershed %in% c("Yuba River", "Feather River")) # remove old ones
  ) %>% 
  bind_rows(yuba_feather_computed_counts_spring) %>% # add in the new ones 
  select(run, watershed, year, count)

sacpas_spring_imputed %>% filter(between(year, 1998, 2017)) %>% 
  filter(watershed %in% cvpiaData::watershed_ordering$watershed) %>% 
  select(watershed, year, count) %>% 
  spread(year, count)  %>% View()


sr_known_adults <- 
  expand.grid(
    watershed = cvpiaData::watershed_ordering$watershed, 
    year = 1975:2017, 
    stringsAsFactors = FALSE
  ) %>% arrange(watershed) %>% 
  mutate(count = as.numeric(NA)) %>% 
  left_join(sacpas_spring_imputed, by=c("watershed"="watershed", "year"="year")) %>% 
  transmute(
    run = "Spring", 
    watershed,
    year, 
    count = count.y
  ) %>%
  left_join(cvpiaData::watershed_ordering) %>% 
  select(order, watershed, year, count) %>% 
  filter(between(year, 1980, 1999)) %>% 
  spread(year, count)

usethis::use_data(sr_known_adults, overwrite = TRUE)

# WINTER RUN ===================================================================

wr_known_adults <- gt %>% 
  filter(
    type == "In-River", 
    run == "Winter", 
    location != "All"
  ) %>% 
  select(run, watershed = location, 
        year = endyear, count) %>% 
  group_by(year) %>% 
  summarise(
    count = sum(count, na.rm = TRUE)
  ) %>% 
  filter(between(year, 1980, 1999))

usethis::use_data(wr_known_adults, overwrite = TRUE)



