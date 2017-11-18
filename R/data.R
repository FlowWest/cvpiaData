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

#' Delta Cross Channel Gates - Days Closed
#' @description The number of days the Delta Cross Channel gates are closed for each month based on typical operation.
#' @format dataframe with 12 rows and 2 variables:
#' \describe{
#' \item{month}{Integar representation of months}
#' \item{days_closed}{the number of days the delta cross channel gates are typically closed}
#' \item{prop_days_closed}{the proportion of days during the month that the delta cross channel gates are typically closed}
#' }
#' @details By rule, 45 days between November-January, based on real time monitoring.
#' For modeling purposes, the days closed where divided between December and January.
#'
#' Note: Some real-time changes possible based on:
#' \itemize{
#'  \item fish monitoring
#'  \item interior delta salinity
#'  \item flood operations
#' }
#'
#' In May, typically open for Memorial Day.
#'
#' @source \href{http://www.westcoast.fisheries.noaa.gov/central_valley/water_operations/ocap.html}{2009 NMFS BiOp Action IV.1 and D-1641}
#'
#' Compiled by Mike Urkov \email{mike.urkov@@gmail.com}
#'
"cross_channel_gates"

#' Delta Proportion Diverted
#' @description The proportion of delta inflow diverted
#' 
#' @format A 3 dimensional array: 12 by 20 by 2 (months, years, deltas)
#' 
#' @details 
#' [ , , 1] North Delta
#' 
#' [ , , 2] South Delta
#' 
#' The North Delta is defined as the area west of and including the Sacramento River below Freeport to Chips Island.
#'
#' The South Delta is defined as the area east of the Sacramento River below Freeport to Chips Island and the San Joaquin River
#' below Vernalis.
#' 
#' @source metadata contained in the cvpiaFlow package
#' 
"dlt_divers"