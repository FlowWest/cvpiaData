#' Generate SIT Model Compatible Array
#' @description takes output from spread_for_array and transforms to array data structure for SIT model input
#' @name create_Sit_array
#' @param input a vector of data, length = 252 for 12 months and 20 years of data
#' @return 3 dimension array [location, month, year]
#' @export

create_SIT_array <- function(input) {

  output <- array(NA, dim = c(nrow(input), 12, ncol(input) / 12))
  index <-  1
  for (i in seq(1, ncol(input), 12)) {
    output[ , , index] <- as.matrix(input[ , i:(i + 11)])
    index <- index + 1
  }
  return(output)

}

#' Prepare data for use with create_SIT_array
#' @description takes tidy data and transforms to dataframe ready to be placed in data structure for model input
#' @name spread_for_array
#' @param df a dataframe from CVPIAdata package
#' @param variable variable of interest from df ('flow', 'diversion', 'prop_diversion', 'avg_tmp')
#' @param start_year int number between 1922 and 2003
#' @param end_year int number between 1922 and 2003
#' @param deltas bol TRUE if retrieving data for deltas, FALSE for all other reaches
#' @return dataframe with dim(number of reaches, 240)
#' @export

spread_for_array <- function(df, variable, start_year, end_year, deltas = FALSE) {
  if (deltas) {
    df %>%
      dplyr::filter(watershed %in% c('SC.Delta', 'N.Delta'),
                    year >= start_year & year <= end_year) %>%
      tidyr::unite(date, year, month) %>%
      dplyr::select_('watershed', 'date', variable) %>%
      tidyr::spread_('date', variable) %>%
      dplyr::left_join(CVPIAdata::watershed_ordering) %>%
      dplyr::arrange(order) %>%
      dplyr::select(-watershed, -order, -sort)
  } else {
    df %>%
      dplyr::filter(!(watershed %in% c('SC.Delta', 'N.Delta')),
                    year >= start_year & year <= end_year) %>%
      tidyr::unite(date, year, month) %>%
      dplyr::select_('watershed', 'date', variable) %>%
      tidyr::spread_('date', variable) %>%
      dplyr::left_join(CVPIAdata::watershed_ordering) %>%
      dplyr::arrange(order) %>%
      dplyr::select(-watershed, -order, -sort)
  }

}

#' Generate SIT Model Inputs for Given 20 Year Period
#' @description returns SIT array of temperature data for given year period
#' @name set_SIT_data
#' @param start_year int number between 1922 and 2003
#' @param end_year int number between 1922 and 2003
#' @return list of model inputs
#' @export

set_SIT_data <- function(start_year, end_year) {

  df <- CVPIAdata::monthly_reach_data

  prop_diversion <- create_SIT_array(spread_for_array(df, 'prop_diversion', start_year, end_year))
  total_diversion <- create_SIT_array(spread_for_array(df, 'diversion', start_year, end_year))
  prop_diversion_delta <- create_SIT_array(spread_for_array(df, 'prop_diversion', start_year, end_year, TRUE))
  total_diversion_delta <- create_SIT_array(spread_for_array(df, 'diversion', start_year, end_year, TRUE))

  temperature <- create_SIT_array(spread_for_array(df, 'avg_temp', start_year, end_year))
  temperature_delta <- create_SIT_array(spread_for_array(df, 'avg_temp', start_year, end_year, TRUE))

  # two deltas?
  delta_inflow <- df %>%
    dplyr::filter(watershed == 'N.Delta',
                  year >= start_year & year <= end_year) %>%
    dplyr::select(year, month, flow) %>%
    tidyr::spread(year, flow) %>%
    dplyr::select(-month)



  return(list(p.diver = prop_diversion, t.diver = total_diversion,
              dlt.divers = prop_diversion_delta, dlt.divers.tot = total_diversion_delta,
              juv.tmp = temperature, juv.tmp.dlt = temperature_delta, Dlt.inf = delta_inflow))
}




