#' Load SIT Model Inputs for Years 1980-1999 
#' @description returns list of data values used by model
#' @name load_baseline_data
#' @param species Call function with one of the following options to load species specific model inputs: 'fall', 'winter', 'spring', 'steelhead'
#' @return list of model inputs
#' @details 
#' The list returned contains the following data:
#' \itemize{
#'   \item \link[=misc_data]{Miscellaneous model inputs}
#'   \item \link[=prop_diversion]{Proportion of flow diverted for each watershed}
#'   \item \link[=total_diversion]{Total flow diverted for each watershed}
#'   \item \link[=dlt_divers]{Proportion of flow diverted in the North and South deltas}
#'   \item \link[=dlt_divers_tot]{Total flow diverted in the North and South deltas}
#'   \item \link[= ]{Monthly mean temperature during juvenile outmigration in each watershed}
#'   \item \link[= ]{Monthly mean temperature during juvenile outmigration in the deltas}
#'   \item \link[=dlt_inflow]{Total inflow in the deltas}
#'   \item \link[=prop_Q_yolo]{Proportion of Sacramento River into Yolo Bypass}
#'   \item \link[=prop_Q_sutter]{Proportion of Sacramento River into Sutter Bypass}
#'   \item \link[= ]{Instream habitat areas for each watershed}
#'   \item \link[= ]{Habitat areas for the deltas}
#'   \item \link[=]{Floodplain habitat areas for each watershed}
#'   \item \link[=bypass_over_top]{Yearly record of bypasses overtopped in October of November  }
#'   \item \link[=]{Accumulated degree days for adults before spawning}
#'   \item \link[=returnQ]{Proportion of flows at tributary junction from natal watershed in October}
#'   \item \link[=upsac_flow]{Upper Sacramento flow }
#'   \item \link[=freeportQcms]{Flow at Georgiana slough}
#'   \item \link[=cross_channel_gates]{Total and proportion number of days the cross channel gates are typically closed per month}
#'   \item \link[=]{Temperature effect on egg mortality}
#' }
#' @examples 
#' # place these two lines within the SIT Salmon Population Model function to load the data
#' all_inputs <- cvpiaData::load_baseline_data('fall')
#' list2env(all_inputs, envir = environment())
#' @export

load_baseline_data <- function(species) {
  
  if(!(species %in% c('fall', 'winter', 'spring', 'steelhead'))) {
    stop("please use one of the following for the species argument: 'fall', 'winter', 'spring', 'steelhead'")
  }
  
  all_inputs <- list(p.tempMC20 = cvpiaData::ptemp20mc,
                     p.diver = cvpiaData::prop_diversion, 
                     t.diver = cvpiaData::total_diversion,
                     dlt.divers = cvpiaData::dlt_divers, 
                     dlt.divers.tot = cvpiaData::dlt_divers_tot,
                     juv.tmp = cvpiaData::rearing_temps, 
                     juv.tmp.dlt = cvpiaData::dlt_temps, 
                     Dlt.inf = cvpiaData::dlt_inflow,
                     DLThab = cvpiaData::dlt_hab, 
                     prop.Q.bypasses = cvpiaData::bypass_prop_Q, # replaced prop.Q.sutter and yolo
                     fp.weeks = cvpiaData::inundation_durations, # number of weeks inundated floodplain
                     gate.top = cvpiaData::bypass_over, # replaced gate.top
                     DegDay = cvpiaData::degday,
                     retQ = cvpiaData::returnQ, 
                     upSacQ = cvpiaData::upsac_flow,
                     freeportQ = cvpiaData::freeportQcms, #sac flow at georgiana slough and delta cross channel
                     dlt.gates = cvpiaData::cross_channel_gates, # replaced gate.top
                     egg.tmp.eff = cvpiaData::egg_temp_effect)
  
  switch(species,
         'fall' = {
           all_inputs$inps = cvpiaData::misc_data[cvpiaData::misc_data$run == 'fall', ]
           all_inputs$IChab.spawn = NA
           all_inputs$IChab.fry = NA
           all_inputs$IChab.juv = NA
           all_inputs$floodP = NA
         },
         'winter' = {
           all_inputs$inps = cvpiaData::misc_data[cvpiaData::misc_data$run == 'winter', ]
           all_inputs$IChab.spawn = NA
           all_inputs$IChab.fry = NA
           all_inputs$IChab.juv = NA
           all_inputs$floodP = NA
         },
         'spring' = {
           all_inputs$inps = cvpiaData::misc_data[cvpiaData::misc_data$run == 'spring', ]
           all_inputs$IChab.spawn = NA
           all_inputs$IChab.fry = NA
           all_inputs$IChab.juv = NA
           all_inputs$floodP = NA
         },
         'steelhead' = {
           all_inputs$inps = cvpiaData::misc_data[cvpiaData::misc_data$run == 'fall', ] # ?
           all_inputs$IChab.spawn = NA
           all_inputs$IChab.fry = NA
           all_inputs$IChab.juv = NA
           all_inputs$floodP = NA
         })
  
  return(all_inputs)
}
