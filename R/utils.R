#' takes tidy data and transforms to array data structure for model input
#' @name create_DSM_array
#' @param input a vector of data, length = 252 for 12 months and 20 years of data
#' @export

create_DSM_array <- function(input) {

  output <- array(NA, dim = c(31, 12, ncol(input) / 12))
  index <-  1
  for (i in seq(1, ncol(input), 12)) {
    output[ , , index] <- as.matrix(input[ , i:(i + 11)])
    index <- index + 1
  }
  return(output)

}
