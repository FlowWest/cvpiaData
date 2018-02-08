#' Ordering of Watersheds
#'
#' @format A data frame with 31 rows and 2 variables:
#' \describe{
#'   \item{order}{The order of the streams, used for indexing in the SIT model}
#'   \item{watershed}{The name of the stream reach}
#' }
#'
#' @source James T. Peterson \email{jt.peterson@@oregonstate.edu}
#'
"watershed_ordering"

#' Proportion Flow at Yolo Bypasses
#' @description The proportion of Lower Sacramento River flow at Yolo Bypasses
#'
#' @format A data frame with 12 rows and 20 variables (months, years)
#' @details For more details see: 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{propQbypass}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/propQbypass.html}{link} if in a web browser
#' }
"prop_Q_yolo"

#' Proportion Flow at Sutter Bypasses
#' @description The proportion of Lower Sacramento River flow at Sutter Bypasses
#'
#'@format A data frame with 12 rows and 20 variables (months, years)
#'@details For more details see:
#'\itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{propQbypass}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/propQbypass.html}{link} if in a web browser
#' }
"prop_Q_sutter"

#' Sutter and Yolo Bypass Over Topped
#' @description  Binary record of the bypasses over topped based on CALSIM flows
#'
#' @format A data frame with 82 rows and 3 variables:
#' \describe{
#'    \item{month}{Integer representation of calendar month}
#'    \item{year}{Year 1979-1999}
#'    \item{tils.ove}{1 = sutter bypass over topped}
#'    \item{yolo.ovr}{1 = yolo bypass over topped}
#' }
#'
#' @source Mike Urkov \email{mike.urkov@@gmail.com}
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
#' @description The proportion of delta inflow diverted from 1980-1999.
#' 
#' @format A 3 dimensional array: 12 by 20 by 2 [months, years, deltas]
#' 
#' [ , , 1] North Delta
#' 
#' [ , , 2] South Delta
#' 
#' @details 
#' The North Delta is defined as the area west of and including the Sacramento River below Freeport to Chips Island.
#'
#' The South Delta is defined as the area east of the Sacramento River below Freeport to Chips Island and the San Joaquin River
#' below Vernalis.
#' 
#' For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{delta_flows}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/delta_flows.html}{link} if in a web browser
#' }
#' 
"dlt_divers"

#' Delta Total Diverted
#' @description The total diverted of delta inflow in cubic meters per second from 1980-1999.
#' 
#' @format A 3 dimensional array: 12 by 20 by 2 (months, years, deltas)
#' 
#' [ , , 1] North Delta
#' 
#' [ , , 2] South Delta
#' 
#' @details 
#' The North Delta is defined as the area west of and including the Sacramento River below Freeport to Chips Island.
#'
#' The South Delta is defined as the area east of the Sacramento River below Freeport to Chips Island and the San Joaquin River
#' below Vernalis.
#' 
#' For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{delta_flows}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/delta_flows.html}{link} if in a web browser
#' }
#' 
"dlt_divers_tot"

#' Delta Inflow
#' @description The delta inflow in cubic meters per second from 1980-1999.
#' 
#' @format A 3 dimensional array: 12 by 20 by 2 (months, years, deltas)
#' 
#' [ , , 1] North Delta
#' 
#' [ , , 2] South Delta
#' 
#' @details 
#' The North Delta is defined as the area west of and including the Sacramento River below Freeport to Chips Island.
#'
#' The South Delta is defined as the area east of the Sacramento River below Freeport to Chips Island and the San Joaquin River
#' below Vernalis.
#' 
#' For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{delta_flows}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/delta_flows.html}{link} if in a web browser
#' }
#' 
"dlt_inflow"

#' Flow at Freeport
#' @description The inflow at Freeport in cubic meters per second from 1980-1999.
#' 
#' @format A dataframe with 12 rows and 20 variables
#' Each row represents a month, each column a year from 1980-1999. 
#' This data is used to route fish into the delta.
#' 
#' @details For more details see:
#'  \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{freeportQ}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/freeportQ.html}{link} if in a web browser
#' }
#' 
#' 
"freeportQcms"

#' Upper Sacramento Flows
#' @description Flows at bend bridge in cubic meters per second from 1980-1999.
#' 
#' @format A dataframe with 12 rows and 20 variables
#' 
#' @details 
#' Each row represents a month, each column a year from 1980-1999. 
#' This data is used to route fish into the delta.
#' 
#' For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{upsacQ}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/upsacQ.html}{link} if in a web browser
#' } 
#' 
"upsac_flow"

#' Miscellaneous Model Inputs
#' @description Data used within the salmon population model that have a single value per watershed.
#' 
#' @format a dataframe with 31 rows and 15 variables
#' \describe{
#' \item{order}{Model consistent ordering of watersheds}
#' \item{watershed}{Name of watershed within CVPIA}
#' \item{init.adult}{Intial adult escapement used for the simulation}
#' \item{SCDELT}{Boolean value: 1 if the watershed feeds into the South Delta}
#' \item{hatch.alloc}{The proportion of hatchery fish spawning}
#' \item{TISD}{Boolean value: 1 if the watershed can enter Sutter Bypass}
#' \item{YOLO}{Boolean value: 1 if the watershed can enter Yolo Bypass}
#' \item{p.tempMC2025}{Proportion of time from October to November in which the temperature is between 20°C and 25°C}
#' \item{A.HARV}{Proportion of adults harvested from golden gate until they reach their natal shed}
#' \item{P.scour.nst}{Estimated Probability of nests scoured}
#' \item{P.strand.early}{Estimated Probability of stranding early}
#' \item{P.strand.late}{Estimated Probability of stranding late}
#' \item{High.pred}{Estimated Probability of high predation during rearing}
#' \item{contact}{Number of contact points, estimated using PAD}
#' \item{prop.nat}{Proportion natural spawners}
#' }
#' 
#' @source 
#' Various  \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#' 
#' \strong{Compiled by:} James T. Peterson \email{jt.peterson@@oregonstate.edu}
#' 
"misc_data"

#' Proportion of Flow Diverted
#' @description The proportion of flow diverted for each watershed every month of every year in the simulation (1980-1999).
#' @format a 3 dimensional array [31 watersheds, 12 months, 20 years]
#' @details For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{proportion_diverted}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/proportion_diverted.html}{link} if in a web browser
#' }
"prop_diversion"

#' Total Flow Diverted
#' @description The total flow diverted in cubic feet per second for each watershed every month of every year in the simulation (1980-1999).
#' @format a 3 dimensional array [31 watersheds, 12 months, 20 years]
#' @details For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{total_diverted}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/total_diverted.html}{link} if in a web browser
#' }
#'  
#' 
"total_diversion"

#' Return Flow
#' @description The proportion flows at tributary junction coming from natal watershed using October CALSIM II flows 
#' from 1979-1998.
#' 
#' @format A dataframe with 12 rows and 20 variables
#' 
#' @details 
#' Each row represents a month, each column a year from 1979-1998. 
#' 
#' For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{return_flow}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/return_flow.html}{link} if in a web browser
#' } 
#' 
"returnQ"