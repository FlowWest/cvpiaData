#' Watershed Data Resolved by Month
#'
#' @description A dataset containing flow, diversion, and temperature data for each stream reach
#' more info from Mike Wright regarding model used to generate data
#' Note about supplement model use for Sutter Bypass
#'
#' @format A data frame with 32472 rows and 7 variables:
#' \describe{
#'   \item{watershed}{Stream reach element}
#'   \item{year}{Year 1921-2003}
#'   \item{month}{Month January to August (1-8)}
#'   \item{flow}{Monthly average flow in cfs}
#'   \item{diversion}{Monthly average diversion in cfs}
#'   \item{prop_diversion}{Monthly average proportion diverted in cfs}
#'   \item{avg_temp}{Monthly average temperature in degrees celcius}
#'   }
#'
#' @source Mike Wright
#'
"monthly_reach_data"

#' Ordering of Watersheds
#'
#' @format A data frame with 31 rows and 2 variables:
#' \describe{
#'   \item{order}{The order of the streams, used for indexing in the SIT model}
#'   \item{watershed}{The name of the stream reach}
#' }
#'
#' @source James T. Peterson
#'
"watershed_ordering"

#' Proportion Flow at Sutter and Yolo Bypasses
#' @description The proportion of Lower Sacramento River flow at Sutter and Yolo Bypasses
#' Sutter data comes from Mike Urkov, Yolo data comes from Mike Wright
#'
#'@format A data frame with 1021 rows and 4 variables:
#' \describe{
#'   \item{year}{Year 1921-2003}
#'   \item{month}{Month January to August (1-8)}
#'   \item{prop_Q_yolo}{Proportion of Lower Sacramento River flow at Yolo Bypass}
#'   \item{prop_Q_sutter}{Proportion of Lower Sacramento River flow at Sutter Bypass}
#' }
#'
#' @source Mike Urkov, Mike Wright
"prop_Q_bypass"

#' Sutter and Yolo Bypass Over Topped
#' @description  Binary record of the bypasses over topped in October or November based on CALSIMS flows
#' data comes from Mike Urkov
#'
#' @format A data frame with 82 rows and 3 variables:
#' \describe{
#'    \item{year}{Year 1921-2002}
#'    \item{sutter}{1 = bypass over topped in October or November}
#'    \item{yolo}{1 = bypass over topped in October or November}
#' }
#'
#' @source Mike Urkov
"bypass_over_top"

#' Return Flow
#' @description The proportion flows at tributary junction coming from natal
#'   watershed.
#'
#' @format A data frame with 2542 rows and 4 variables: \describe{
#'   \item{watershed}{The name of the stream reach}
#'   \item{year}{Year 1921-2002}
#'   \item{retQ}{The proportion flows at tributary
#'   junction coming from natal watershed calculated from the average monthly
#'   flow in October}
#'   }
#'
#'@source James T. Peterson, Mike Urkov
'return_flow'

