
start_year = 1970
end_year = 1989


# create temp input
temp <- CVPIAdata::temperatures %>%
  dplyr::filter(!(watershed %in% c('SC.Delta', 'N.Delta')),
                year >= start_year & year <= end_year) %>%
  tidyr::unite(date, year, month) %>%
  tidyr::spread(date, avg_temp) %>%
  dplyr::left_join(CVPIAdata::watershed_ordering) %>%
  dplyr::arrange(order) %>%
  dplyr::select(-watershed, -order)

juv_tmp <- create_DSM_array(temps)
