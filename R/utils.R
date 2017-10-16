#' Generate SIT Model Compatible Array
#' @description takes output from spread_for_array and transforms to array data structure for SIT model input
#' @name create_Sit_array
#' @param input a vector of data, length = 252 for 12 months and 20 years of data
#' @return 3 dimension array [location, month, year]

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

spread_for_array <- function(df, variable, start_year, end_year, deltas = FALSE) {
  if (deltas) {
    df %>%
      dplyr::filter(watershed %in% c('SC.Delta', 'N.Delta')) %>%
      tidyr::unite(date, year, month) %>%
      dplyr::select_('watershed', 'date', variable) %>%
      tidyr::spread_('date', variable) %>%
      dplyr::left_join(CVPIAdata::watershed_ordering) %>%
      dplyr::arrange(order) %>%
      dplyr::select(-watershed, -order)
  } else {
    df %>%
      dplyr::filter(!(watershed %in% c('SC.Delta', 'N.Delta'))) %>%
      tidyr::unite(date, year, month) %>%
      dplyr::select_('watershed', 'date', variable) %>%
      tidyr::spread_('date', variable) %>%
      dplyr::left_join(CVPIAdata::watershed_ordering) %>%
      dplyr::arrange(order) %>%
      dplyr::select(-watershed, -order)
  }

}

#' Function converts a weighted usable area (WUA) to area in square meters
#' @param wua
#' @param ws watershed
#' @param sp species
#' @param ls life stage
wua_to_area <- function(wua, ws, sp, ls) {
  d <- cvpiaHabitat::watershed_lengths

  watershed_length <- d %>% dplyr::filter(watershed == ws,
                                          species == sp,
                                          lifestage == ls) %>%
    dplyr::pull(feet)

  ((watershed_length/1000) * wua)/10.7639
}
