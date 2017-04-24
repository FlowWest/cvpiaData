#' takes tidy data and transforms to array data structure for model input
#' @name create_DSM_array
#' @param input a vector of data, length = 252 for 12 months and 20 years of data
#' @export

create_DSM_array <- function(input) {

  output <- array(NA, dim = c(nrow(input), 12, ncol(input) / 12))
  index <-  1
  for (i in seq(1, ncol(input), 12)) {
    output[ , , index] <- as.matrix(input[ , i:(i + 11)])
    index <- index + 1
  }
  return(output)

}


#' returns DSM array of temperature data for given year period
#' @name get_temperature_data
#' @param start_year int number between 1922 and 2003
#' @param end_year int number between 1922 and 2003
#' @param location chr either 'watershed' or 'delta'
#' @export
get_temperature_data <- function(start_year, end_year, location = 'watershed') {

  if (location == 'watershed') {
    temp <- CVPIAdata::temperatures %>%
      dplyr::filter(!(watershed %in% c('SC.Delta', 'N.Delta')),
                    year >= start_year & year <= end_year) %>%
      tidyr::unite(date, year, month) %>%
      tidyr::spread(date, avg_temp) %>%
      dplyr::left_join(CVPIAdata::watershed_ordering) %>%
      dplyr::arrange(order) %>%
      dplyr::select(-watershed, -order, -sort)
  } else {
    temp <- CVPIAdata::temperatures %>%
      dplyr::filter(watershed %in% c('SC.Delta', 'N.Delta'),
                    year >= start_year & year <= end_year) %>%
      tidyr::unite(date, year, month) %>%
      tidyr::spread(date, avg_temp) %>%
      dplyr::left_join(CVPIAdata::watershed_ordering) %>%
      dplyr::arrange(order) %>%
      dplyr::select(-watershed, -order, -sort)
  }

  create_DSM_array(temp)
}

