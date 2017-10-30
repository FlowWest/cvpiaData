#' Load SIT Model Inputs for Years 1980-1999 
#' @description returns list of data values used by model
#' @name load_SIT_data
#' @param scenario default 'baseline', move scenarios to come
#' @return list of model inputs
#' @export

load_SIT_data <- function(scenario = 'baseline') {

  #gates closed held constant at 31 in updated SIT model
  if (scenario == 'baseline') {
    inputs <- list(inps = cvpiaData::misc_data,
         p.diver = cvpiaData::prop_diversion, 
         t.diver = cvpiaData::total_diversion,
         dlt.divers = NULL, 
         dlt.divers.tot = NULL,
         juv.tmp = NULL, 
         juv.tmp.dlt = NULL, 
         Dlt.inf = NULL,
         prop.Q.yolo = cvpiaData::prop_Q_yolo, 
         prop.Q.sutter = cvpiaData::prop_Q_sutter,
         prop.Q.dcc = cvpiaData::prop_Q_dcc, #prop sac flow at georgiana slough and delta cross channel
         IChab = NULL, 
         DLThab = NULL, 
         floodP = NULL, 
         gate.top = cvpiaData::bypass_over_top, 
         DegDay = NULL,
         retQ = cvpiaData::returnQ, 
         upSacQ = cvpiaData::upsac_flow,
         egg.tmp.eff = NULL)
  }
  return(inputs)
  
}
