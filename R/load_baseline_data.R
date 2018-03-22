#' Load SIT Model Inputs for Years 1980-1999 
#' @description returns list of data values used by model
#' @name load_baseline_data
#' @param species Call function with one of the following options to load species specific model inputs: 'fall', 'winter', 'spring', 'steelhead'
#' @return list of model inputs
#' @details 
#' The list returned contains the following data:
#' \itemize{
#'   \item \link[=ptemp20mc]{p.tempMC20} = Proportion of days in month exceeding 20°C
#'   \item \link[=prop_diversion]{p.diver} = Proportion of flow diverted for each watershed
#'   \item \link[=total_diversion]{t.diver} = Total flow diverted for each watershed
#'   \item \link[=dlt_divers]{dlt.divers} = Proportion of flow diverted in the North and South deltas
#'   \item \link[=dlt_divers_tot]{dlt.divers.tot} = Total flow diverted in the North and South deltas
#'   \item \link[=rearing_temps]{juv.tmp} = Monthly mean temperature during juvenile outmigration in each watershed
#'   \item \link[=dlt_temps]{juv.tmp.dlt} = Monthly mean temperature during juvenile outmigration in the deltas
#'   \item \link[=dlt_inflow]{Dlt.inf} = Total inflow in the deltas
#'   \item \link[=dlt_hab]{DLThab} = Habitat areas for the deltas
#'   \item \link[=bypass_prop_Q]{prop.Q.bypasses} = Proportion of Sacramento River into bypasses
#'   \item \link[=inchannel_bypass]{IChab.bypass} = Instream habitat areas for each bypass
#'   \item \link[=floodplain_bypass]{floodp.bypass} = Floodplain habitat areas for each bypass
#'   \item \link[=inundation_durations]{fp.weeks} = Floodplain habitat duration inundated for each watershed
#'   \item \link[=bypass_over]{gate.top} = Monthly record of bypasses overtopped
#'   \item \link[=degday]{DegDay} = Accumulated degree days for adults before spawning
#'   \item \link[=returnQ]{retQ} = Proportion of flows at tributary junction from natal watershed in October
#'   \item \link[=upsac_flow]{upSacQ} = Upper Sacramento flow
#'   \item \link[=freeportQcms]{freeportQ} = Flow at Georgiana slough
#'   \item \link[=cross_channel_gates]{dlt.gates} = Total and proportion number of days the cross channel gates are typically closed per month
#'   \item \link[=egg_temp_effect]{egg.tmp.eff} = Temperature effect on egg mortality
#'   \item \link[=misc_data]{inps} = Miscellaneous model inputs
#'   \item \link[=misc_delta]{Dlt.inp} = Miscellaneous delta model inputs
#'   \item \link[=prop_pulse]{prop.pulse} = Proportion pulse flows
#'   \item \link[=med_flow]{medQ} = Monthly median flows
#'   \item \link[=spawn_fall]{IChab.spawn} = Spawning habitat areas for each watershed
#'   \item \link[=inchannel_fry_fall]{IChab.fry} = Fry instream habitat areas for each watershed
#'   \item \link[=inchannel_juv_fall]{IChab.juv} = Juvenile instream habitat areas for each watershed
#'   \item \link[=floodplain_fall]{floodP} = Floodplain habitat areas for each watershed
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
                     IChab.bypass = cvpiaData::inchannel_bypass, # new bypass inchannel
                     floodp.bypass = cvpiaData::floodplain_bypass, # new bypass floodplain
                     fp.weeks = cvpiaData::inundation_durations, # number of weeks inundated floodplain
                     gate.top = cvpiaData::bypass_over, # replaced gate.top
                     DegDay = cvpiaData::degday,
                     retQ = cvpiaData::returnQ, 
                     upSacQ = cvpiaData::upsac_flow,
                     freeportQ = cvpiaData::freeportQcms, #sac flow at georgiana slough and delta cross channel
                     dlt.gates = cvpiaData::cross_channel_gates, # replaced gate.top
                     egg.tmp.eff = cvpiaData::egg_temp_effect,
                     Dlt.inp = cvpiaData::misc_delta,
                     prop.pulse = cvpiaData::prop_pulse,
                     medQ = cvpiaData::med_flow)
  
  switch(species,
         'fall' = {
           all_inputs$inps = cvpiaData::misc_data[cvpiaData::misc_data$run == 'fall', ]
           all_inputs$IChab.spawn = cvpiaData::fr_spawn
           all_inputs$IChab.fry = cvpiaData::fr_fry
           all_inputs$IChab.juv = cvpiaData::fr_juv
           all_inputs$floodP = cvpiaData::fr_fp
         },
         'winter' = {
           all_inputs$inps = cvpiaData::misc_data[cvpiaData::misc_data$run == 'winter', ]
           all_inputs$IChab.spawn = cvpiaData::fr_spawn
           all_inputs$IChab.fry = cvpiaData::fr_fry
           all_inputs$IChab.juv = cvpiaData::fr_juv
           all_inputs$floodP = cvpiaData::fr_fp
         },
         'spring' = {
           all_inputs$inps = cvpiaData::misc_data[cvpiaData::misc_data$run == 'spring', ]
           all_inputs$IChab.spawn = cvpiaData::sr_spawn
           all_inputs$IChab.fry = cvpiaData::sr_fry
           all_inputs$IChab.juv = cvpiaData::sr_juv
           all_inputs$floodP = cvpiaData::sr_fp
           all_inputs$SR.pools = cvpiaData::pools$SR_pools_sq_meters
         },
         'steelhead' = {
           all_inputs$inps = cvpiaData::misc_data[cvpiaData::misc_data$run == 'fall', ] # ?
           all_inputs$IChab.spawn = cvpiaData::st_spawn
           all_inputs$IChab.fry = cvpiaData::st_fry
           all_inputs$IChab.juv = cvpiaData::st_juv
           all_inputs$floodP = cvpiaData::st_fp
           all_inputs$ST.pools = cvpiaData::pools$ST_pools_sq_meters
         })
  
  return(all_inputs)
}
