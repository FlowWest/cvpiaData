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

#' Proportion Flow at Yolo and Sutter Bypasses
#' @description The proportion of Lower Sacramento River flow at each bypass weir
#'
#' @format A 3 dimensional array: 12 by 20 by 6 [months, years, bypasses]
#' 
#' \itemize{
#'   \item [ , , 1] Sutter Bypass 1
#'   \item [ , , 2] Sutter Bypass 2
#'   \item [ , , 3] Sutter Bypass 3
#'   \item [ , , 4] Sutter Bypass 4
#'   \item [ , , 5] Yolo Bypass 1
#'   \item [ , , 6] Yolo Bypass 2
#' }

#' 
#' @details For more details see: 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{propQbypass}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/propQbypass.html}{link} if in a web browser
#' }
"bypass_prop_Q"


#' Sutter and Yolo Bypass Over Topped
#' @description  1979-1999 boolean record of the bypasses over topped based on CALSIM flows
#'
#' @format A 3 dimensional array: 12 by 21 by 2 [months, years, bypasses]
#' 
#' [ , , 1] Sutter Bypass
#' 
#' [ , , 2] Yolo Bypass
#' 
#'@details For more details see:
#'\itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{bypass_overtopped}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/bypass_overtopped.html}{link} if in a web browser
#' }
#' @source Sadie Gill \email{sgill@@flowwest.com}
"bypass_over"

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

#' Delta Inflow
#' @description The delta temperature in °C from 1980-1999.
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
#'   \item use this link within R \code{\link[cvpiaTemperature]{delta_temps}}
#'   \item use this \href{https://flowwest.github.io/cvpiaTemperature/reference/delta_temps.html}{link} if in a web browser
#' }
#' 
"dlt_temps"

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
#' Various \href{https://s3-us-west-2.amazonaws.com/cvpia-reference-docs/AWP+Attachment+1.pdf}{CVPIA Science Integration Team: FY18 Decision Support Model activities and FY17 priorities Memorandum}
#' 
#' 
#' Compiled by: James T. Peterson \email{jt.peterson@@oregonstate.edu}
#' 
"misc_data"

#' Monthly Mean Flow
#' @description The mean flow in cubic meters per second for each watershed every month of every year in the simulation (1980-1999).
#' @format a 3 dimensional array [31 watersheds, 12 months, 20 years]
#' @details For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{flows_cfs}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/flows_cfs.html}{link} if in a web browser
#' }
"prop_diversion"

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

#' Degree Days
#' @description The monthly accumulated degree days (celsius)
#' @format a 3 dimensional array [31 watersheds, 12 months, 21 years]
#' @details 
#' Accumulated degree days for 1979-1999
#' For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaTemperature]{deg_days}}
#'   \item use this \href{https://flowwest.github.io/cvpiaTemperature/reference/deg_days.html}{link} if in a web browser
#' }
#'  
#' 
"degday"

#' Delta Rearing Habitat
#' @description The delta high quality habitat area in square meters from 1980-1999.
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
#'   \item use this link within R \code{\link[cvpiaHabitat]{delta_habitat}}
#'   \item use this \href{https://flowwest.github.io/cvpiaHabitat/reference/delta_habitat.html}{link} if in a web browser
#' }
#' 
"dlt_hab"

#' Floodplain Habitat Activation Duration
#' @description The 1980-1999 floodplain rearing habitat event duration in number of weeks
#' @format a 3 dimensional array [31 watersheds, 12 months, 20 years]
#' @details 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaHabitat]{weeks_flooded}}
#'   \item use this \href{https://flowwest.github.io/cvpiaHabitat/reference/weeks_flooded.html}{link} if in a web browser
#' }
#' 
#' Additional growth benefit on floodplain max out after 2 weeks, apply survival
#' benefit for total duration.
#'  
"inundation_durations"

#' Rearing Temperature
#' @description The 1980-1999 rearing temperature in °C
#' @format a 3 dimensional array [31 watersheds, 12 months, 20 years]
#' @details 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaTemperature]{juv_temp}}
#'   \item use this \href{https://flowwest.github.io/cvpiaTemperature/reference/juv_temp.html}{link} if in a web browser
#' }
#'
"rearing_temps"

#' Migratory Corridor Temperature Exceedance
#' @description The mean monthly proportion of temperatures above 20°C
#' @format  A dataframe with 31 rows and 14 variables 
#' \describe{
#'   \item{order}{SIT model watershed ordering}
#'   \item{watershed}{watershed within CVPIA}
#'   \item{1}{January}
#'   \item{...}{}prop_temp_over_20_migr_cor
#'   \item{12}{December}
#' }
#' @details 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaTemperature]{prop_temp_over_20_migr_cor}}
#'   \item use this \href{https://flowwest.github.io/cvpiaTemperature/reference/prop_temp_over_20_migr_cor.html}{link} if in a web browser
#' }
#' 
"ptemp20mc"

#' Temperature Effect on Egg Mortality
#' @description The mean estimate of temperature effect used for egg to fry survival
#' @format  A dataframe with 31 rows and 2 variables 
#' \describe{
#'   \item{watershed}{watershed within CVPIA}
#'   \item{mean_temp_effect}{temperature effect}
#' }
#' @details 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaTemperature]{prop_temp_over_20_migr_cor}}
#'   \item use this \href{https://flowwest.github.io/cvpiaTemperature/reference/prop_temp_over_20_migr_cor.html}{link} if in a web browser
#' }
#' Was calculated by takeing the mean of dry and wet egg temp effects from previous model.
#' 
"egg_temp_effect"

#' Bypass Instream Habitat Area
#' @description The 1980-1999 suitable juvenile rearing habitat area in square meters
#' @format a 3 dimensional array [6 bypass sections, 12 months, 20 years]
#' @details 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaHabitat]{set_bypass_instream_habitat}}
#'   \item use this \href{https://flowwest.github.io/cvpiaHabitat/reference/set_bypass_instream_habitat.html}{link} if in a web browser
#' }
#' 
#' @section Bypass Sections:
#' 
#' \itemize{
#'   \item [1, , ] sutter1 = to Moulton Weir
#'   \item [2, , ] sutter2 = to Colusa Weir
#'   \item [3, , ] sutter3 = to Tisdale Weir
#'   \item [4, , ] sutter4 = below Tisdale Weir
#'   \item [5, , ] yolo1 = Fremont Weir to Sacramento Weir
#'   \item [6, , ] yolo2 = below Sacramento Weir
#' }
#' 
"inchannel_bypass"

#' Bypass Floodplain Habitat Area
#' @description The 1980-1999 total floodplain habitat area in square meters
#' @format a 3 dimensional array [6 bypass sections, 12 months, 20 years]
#' @details 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaHabitat]{set_bypass_floodplain_habitat}}
#'   \item use this \href{https://flowwest.github.io/cvpiaHabitat/reference/set_bypass_floodplain_habitat.html}{link} if in a web browser
#' }
#' 
#' Note: Remember to apply a suitability factor to downscale the total area.
#' 
#' @section Bypass Sections:
#' 
#' \itemize{
#'   \item [1, , ] sutter1 = to Moulton Weir
#'   \item [2, , ] sutter2 = to Colusa Weir
#'   \item [3, , ] sutter3 = to Tisdale Weir
#'   \item [4, , ] sutter4 = below Tisdale Weir
#'   \item [5, , ] yolo1 = Fremont Weir to Sacramento Weir
#'   \item [6, , ] yolo2 = below Sacramento Weir
#' }
#' 
"floodplain_bypass"

#' Delta Miscellaneous Inputs
#' @format a dataframe with 2 rows and 3 variables
#' \describe{
#'   \item{delta}{CVPIA Delta segment}
#'   \item{High.pred}{Expert estimated probability of high predation}
#'   \item{contct.pts}{Number of contact points estimated from PAD}
#' }
#' 
"misc_delta"

#' Proportion Pulse Flow
#' @format a dataframe with 31 rows and 13 variables 
#' \describe{
#'   \item{watershed}{CVPIA watershed}
#'   \item{1}{January proportion pulse}
#'   \item{2}{February proportion pulse}
#'   \item{3}{March proportion pulse}
#'   \item{4}{April proportion pulse}
#'   \item{5}{May proportion pulse}
#'   \item{6}{June proportion pulse}
#'   \item{7}{July proportion pulse}
#'   \item{8}{August proportion pulse}
#'   \item{9}{September proportion pulse}
#'   \item{10}{October proportion pulse}
#'   \item{11}{November proportion pulse}
#'   \item{12}{December proportion pulse}
#' }
#' @details prop_pulse = sd(flow)/median(flow)
#' 
#' For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{flows_cfs}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/flows_cfs.html}{link} if in a web browser
#' } 
#' 
"prop_pulse"

#' Median Flow
#' @description The monthly median flows of 1980-1999 in cubic feet per second
#' @format a dataframe with 31 rows and 13 variables 
#' \describe{
#'   \item{watershed}{CVPIA watershed}
#'   \item{1}{January median flow}
#'   \item{2}{February median flow}
#'   \item{3}{March median flow}
#'   \item{4}{April median flow}
#'   \item{5}{May median flow}
#'   \item{6}{June median flow}
#'   \item{7}{July median flow}
#'   \item{8}{August median flow}
#'   \item{9}{September median flow}
#'   \item{10}{October median flow}
#'   \item{11}{November median flow}
#'   \item{12}{December median flow}
#' }
#' @details 
#' 
#' For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaFlow]{flows_cfs}}
#'   \item use this \href{https://flowwest.github.io/cvpiaFlow/reference/flows_cfs.html}{link} if in a web browser
#' } 
#' 
"med_flow"

#' Pools
#' @description Area in square meters of pools for each watershed
#' 
#' @details
#' For more details see:
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaHabitat]{pools}}
#'   \item use this \href{https://flowwest.github.io/cvpiaHabitat/reference/pools.html}{link} if in a web browser
#' } 
#' 
'pools'

#' Spawning Habitat
#' @description The 1979-1999 suitable spawning habitat area in square meters
#' @format a 3 dimensional array [31 watersheds, 12 months, 21 years]
#' @details 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaHabitat]{set_spawning_habitat}}
#'   \item use this \href{https://flowwest.github.io/cvpiaHabitat/reference/set_spawning_habitat.html}{link} if in a web browser
#' }
#'  
#' @name spawn
#' @aliases NULL
NULL


#' Fry Inchannel Habitat
#' @description The 1980-1999 fry suitable inchannel rearing habitat area in square meters
#' @format a 3 dimensional array [31 watersheds, 12 months, 20 years]
#' @details 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaHabitat]{set_instream_habitat}}
#'   \item use this \href{https://flowwest.github.io/cvpiaHabitat/reference/set_instream_habitat.html}{link} if in a web browser
#' }
#'  
#' @name fry
#' @aliases NULL
NULL

#' Juvenile Inchannel Habitat
#' @description The 1980-1999 juvenile suitable inchannel rearing habitat area in square meters
#' @format a 3 dimensional array [31 watersheds, 12 months, 20 years]
#' @details 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaHabitat]{set_instream_habitat}}
#'   \item use this \href{https://flowwest.github.io/cvpiaHabitat/reference/set_instream_habitat.html}{link} if in a web browser
#' }
#'  
#' @name juvenile
#' @aliases NULL
NULL

#' Floodplain Habitat
#' @description The 1980-1999 total floodplain rearing habitat area in square meters
#' @format a 3 dimensional array [31 watersheds, 12 months, 20 years]
#' @details 
#' \itemize{
#'   \item use this link within R \code{\link[cvpiaHabitat]{set_floodplain_habitat}}
#'   \item use this \href{https://flowwest.github.io/cvpiaHabitat/reference/set_floodplain_habitat.html}{link} if in a web browser
#' }
#' Need to apply a suitability factor, recommend 27\%.
#'  
#' @name floodplain
#' @aliases NULL
NULL

#' @rdname fry
"fr_fry"

#' @rdname fry
"wr_fry"

#' @rdname fry
"sr_fry"

#' @rdname fry
"st_fry"

#' @rdname juvenile
"fr_juv"

#' @rdname juvenile
"wr_juv"

#' @rdname juvenile
"sr_juv"

#' @rdname juvenile
"st_juv"

#' @rdname floodplain
"fr_fp"

#' @rdname floodplain
"wr_fp"

#' @rdname floodplain
"sr_fp"

#' @rdname floodplain
"st_fp"

#' @rdname spawn
"fr_spawn"

#' @rdname spawn
"wr_spawn"

#' @rdname spawn
"sr_spawn"

#' @rdname spawn
"st_spawn"

#' Spring Run Present
#'
#' @format a dataframe with 31 rows and 2 variables
#' \describe{
#'   \item{watershed}{CVPIA watershed name}
#'   \item{has_spring_run}{TRUE if spring run exist within watershed}
#' }
#'
"has_spring_run"