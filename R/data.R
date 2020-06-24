
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


#' Delta Rearing Habitat
#' @description The delta high quality habitat area in square meters from 1980-2000.
#' 
#' @format A 3 dimensional array: 12 by 21 by 2 [months, years, deltas]
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
#' @description The 1980-2000 suitable juvenile rearing habitat area in square meters
#' @format a 3 dimensional array [6 bypass sections, 12 months, 21 years]
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
#' @description The 1980-2000 total floodplain habitat area in square meters
#' @format a 3 dimensional array [6 bypass sections, 12 months, 21 years]
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
#' @description The 1979-1999 suitable spawning habitat area in square meters for Steelhead
#' and 1979-2000 suitable spawning habitat area in square meters for Fall Run, Spring Run and Winter Run.
#' @format 
#' \itemize{
#' \item Steelhead: 3 dimensional array [31 watersheds, 12 months, 21 years]
#' \item Fall Run, Spring Run and Winter Run: 3 dimensional array [31 watersheds, 12 months, 22 years]
#' }
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
#' @description The 1980-1999 fry suitable inchannel rearing habitat area in square meters for Steelhead
#' and 1979-2000 suitable inchannel rearing habitat area in square meters for Fall Run, Spring Run and Winter Run.
#' @format
#' \itemize{
#' \item Steelhead: 3 dimensional array [31 watersheds, 12 months, 20 years]
#' \item Fall Run, Spring Run and Winter Run: 3 dimensional array [31 watersheds, 12 months, 21 years]
#' }
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
#' @description The 1980-1999 juvenile suitable inchannel rearing habitat area in square meters for  
#' Steelhead and 1980-2000 juvenile suitable inchannel rearing habitat area in square meters
#' for Fall Run, Spring Run and Winter Run.
#' @format
#' \itemize{
#' \item Steelhead: 3 dimensional array [31 watersheds, 12 months, 20 years]
#' \item Fall Run, Spring Run and Winter Run: 3 dimensional array [31 watersheds, 12 months, 21 years]
#' }
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
#' @description The 1980-1999 total floodplain rearing habitat area in square meters for 
#' Steelhead and 1980-2000 total floodplain rearing habitat area in square meters for Fall Run, Spring Run and
#' Winter Run
#' @format
#' \itemize{
#' \item Steelhead: 3 dimensional array [31 watersheds, 12 months, 20 years]
#' \item Fall Run, Spring Run and Winter Run: 3 dimensional array [31 watersheds, 12 months, 21 years]
#' }
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

#' Temperature Proportions
#' @description a set of the temperature proportions for both tribs and the delta.
#' See details below for more information.
#' @details 
#' \itemize{
#'   \item{\code{aveT20} are proportions of months for which stream temperature was greater than 20°C}
#'   \item{\code{aveT20D} are proportions of months for which delta temperature was greater than 20°C}
#'   \item{\code{aveT24} probability that max temps > 24°C in a month based on average monthly temp in streams (chinook only)}
#'   \item{\code{aveT29} probability that max temps > 29°C in a month based on average monthly temp in streams (steelhead only)}
#' }
#' @name tempprops
#' @aliases NULL
NULL

#' @rdname tempprops
"aveT20"

#' @rdname tempprops
"aveT20D"

#' @rdname tempprops
"maxT24"

#' @rdname tempprops
"maxT29"



