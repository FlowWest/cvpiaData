test_flows <- rnorm(n = 31 * 20 * 8, mean = 1200, sd = 100)
test_in <- CVPIAdata::monthly_reach_data

d <- CVPIAdata::watershed_ordering %>% dplyr::pull(watershed)

# there are two levels of iteration
# 1. iterate through the watersheds
# 2. iterate through the flow inputs

test_out <- list()

# this iteration makes it such that columns are months (Oct-Sep) rows are years 1921-2003
# each of the watersheds are within an element of the resulting list, calling bind_rows
# will collapse them down to a single dataframe
for (i in seq_along(d)){
  # so yeah all these tribs dont have a flow to area relationship
  if (d[i] == "Antelope Creek") next
  if (d[i] == "Battle Creek") next
  if (d[i] == "Bear Creek") next
  if (d[i] == "Clear Creek") next
  if (d[i] == "Cow Creek") next
  if (d[i] == "Mill Creek") next
  if (d[i] == "Paynes Creek") next
  if (d[i] == "Stony Creek") next
  if (d[i] == "Thomes Creek") next
  if (d[i] == "Sutter Bypass") next
  if (d[i] == "Cosumnes River") next
  if (d[i] == "Mokelumne River") next # <- this one has just one non-na
  if (d[i] == "Merced River") next

  x <- filter(test_in, watershed == d[i])
  temp_out <- set_floodplain_habitat(d[i], species = "fr", flow = x$flow)
  temp_tibble <- tibble::as_tibble(matrix(temp_out, ncol = 12, byrow = TRUE))# here the shape is
  colnames(temp_tibble) <- month.abb[c(10:12, 1:9)]
  temp_tibble$year <- as.character(1921:2002)
  temp_tibble$watershed <- d[i]
  test_out[[d[i]]] <- temp_tibble
}


t <- bind_rows(test_out)
