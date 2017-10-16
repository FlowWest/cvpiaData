test_flows <- rnorm(n = 31 * 20 * 8, mean = 1200, sd = 100)
test_in <- CVPIAdata::monthly_reach_data %>% filter(year > 1921)


d <- CVPIAdata::watershed_ordering %>% dplyr::pull(watershed)

# there are two levels of iteration
# 1. iterate through the watersheds
# 2. iterate through the flow inputs

test_out <- list()

# this implementation assumes we have a dataframe like CVPIAdata::monthly_reach_data as input
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
  temp_tibble <- tibble::as_tibble(matrix(temp_out, ncol = 12, byrow = TRUE))
  colnames(temp_tibble) <- month.abb[c(10:12, 1:9)]
  temp_tibble$year <- as.character(1922:2003)
  temp_tibble$watershed <- d[i]
  test_out[[d[i]]] <- temp_tibble
}


t <- bind_rows(test_out)


make_floodplain_input <- function(flows_df, sp, period = "wet") {
  if (period == "wet") {
    period <- 1970:1989
  } else {
    period <-  1970:1989
  }

  d <- CVPIAdata::watershed_ordering %>%
    dplyr::pull(watershed)

  floodplains <- data.frame()

  for (i in d) {
    #if (watershed_not_defined(i)) next # <- this is the ideal implementation..not whats below
    if (i == "Antelope Creek") next
    if (i == "Battle Creek") next
    if (i == "Bear Creek") next
    if (i == "Clear Creek") next
    if (i == "Cow Creek") next
    if (i == "Mill Creek") next
    if (i == "Paynes Creek") next
    if (i == "Stony Creek") next
    if (i == "Thomes Creek") next
    if (i == "Sutter Bypass") next
    if (i == "Cosumnes River") next
    if (i == "Mokelumne River") next # <- this one has just one non-na
    if (i == "Merced River") next

    x <- filter(flows_df, lubridate::year(date) %in% period, watershed == i)
    temp_out <- set_floodplain_habitat(i, species = sp, flow = x$flow) # one vector for all flows
    #temp_tibble <- tibble::as_tibble(matrix(temp_out, ncol = 20, byrow = TRUE))
    #colnames(temp_tibble) <- month.abb
    #temp_tibble$year <- as.character(period)
    temp_tibble <- data.frame(matrix(temp_out, ncol = 240))
    temp_tibble$watershed <- i

    # temp_tibble <- tibble::tibble(watershed = i, temp_out)
    floodplains <- rbind(floodplains, temp_tibble)
  }

  return(floodplains)
}

test_in %>% select(watershed, year) %>% group_by(watershed) %>% summarise(n())

# so right now this is months X year
floodplain_fall_run_out <- make_floodplain_input(test_in, "fr")

t1 <- floodplain_fall_run_out %>% filter(year == "2002")
array(t1, dim = c(31, 12, 1))

# this version has columns as watersheds, and rows are 12 * 82 years in order starting
# at october and first year.....until september last year
make_floodplain_input2 <- function(flows_df, sp) {
  d <- CVPIAdata::watershed_ordering %>%
    dplyr::pull(watershed)

  list_out <- list()

  for (i in d) {
    #if (watershed_not_defined(i)) next # <- this is the ideal implementation..not whats below
    if (i == "Antelope Creek") next
    if (i == "Battle Creek") next
    if (i == "Bear Creek") next
    if (i == "Clear Creek") next
    if (i == "Cow Creek") next
    if (i == "Mill Creek") next
    if (i == "Paynes Creek") next
    if (i == "Stony Creek") next
    if (i == "Thomes Creek") next
    if (i == "Sutter Bypass") next
    if (i == "Cosumnes River") next
    if (i == "Mokelumne River") next # <- this one has just one non-na
    if (i == "Merced River") next

    x <- filter(test_in, watershed == i)
    temp_out <- set_floodplain_habitat(i, species = sp, flow = x$flow)
    print(length(temp_out))
    array_out <- array(temp_out, dim = c(1, 12, 82))  # per watershed this is one watershed
                                                      # by the 12 months
                                                      # by the 82 years
    list_out[[i]] <- array_out
  }


  return(list_out)
}

arr <- array(dim = c(18, 12, 20))
for (i in seq_len(length(z))) {
  arr[i, , ] <- z[[i]]
}
