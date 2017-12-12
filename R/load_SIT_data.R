#' Load SIT Model Inputs for Years 1980-1999 
#' @description returns list of data values used by model
#' @name load_SIT_data
#' @param scenario default 'baseline', more scenarios to come
#' @return list of model inputs
#' @export

load_SIT_data <- function(scenario = 'baseline') {

  #gates closed held constant at 31 in updated SIT model
  if (scenario == 'baseline') {
    inputs <- list(inps = cvpiaData::misc_data,
         p.diver = cvpiaData::prop_diversion, 
         t.diver = cvpiaData::total_diversion,
         dlt.divers = cvpiaData::dlt_divers, 
         dlt.divers.tot = cvpiaData::dlt_divers_tot,
         juv.tmp = NULL, 
         juv.tmp.dlt = NULL, 
         Dlt.inf = cvpiaData::dlt_inflow,
         prop.Q.yolo = cvpiaData::prop_Q_yolo, 
         prop.Q.sutter = cvpiaData::prop_Q_sutter,
         IChab = NULL, 
         DLThab = NULL, 
         floodP = NULL, 
         gate.top = cvpiaData::bypass_over_top, # replaced gate.top
         DegDay = NULL,
         retQ = cvpiaData::returnQ, 
         upSacQ = cvpiaData::upsac_flow,
         freeportQ = cvpiaData::freeportQcms, #sac flow at georgiana slough and delta cross channel
         dlt.gates = cvpiaData::cross_channel_gates, # replaced gate.top
         egg.tmp.eff = NULL)
  }
  return(inputs)
  
}
