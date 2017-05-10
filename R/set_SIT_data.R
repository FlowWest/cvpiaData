#' Generate SIT Model Inputs for Given 20 Year Period
#' @description returns SIT array of temperature data for given year period
#' @name set_SIT_data
#' @param start_year int number between 1922 and 2003
#' @param end_year int number between 1922 and 2003
#' @return list of model inputs
#' @export

set_SIT_data <- function(start_year, end_year) {

  if (end_year - start_year != 19) stop('years given are not a 20 year range')
  if (end_year < start_year) stop('second year given must be after first')
  if (end_year > 2002 | end_year < 1922) stop('last year is out of data range')
  if (start_year < 1922 | start_year > 2002) stop('first year is out of data range')

  df <- CVPIAdata::monthly_reach_data %>%
    dplyr::filter(year >= start_year & year <= end_year)

  prop_diversion <- create_SIT_array(spread_for_array(df, 'prop_diversion'))
  total_diversion <- create_SIT_array(spread_for_array(df, 'diversion'))
  prop_diversion_delta <- create_SIT_array(spread_for_array(df, 'prop_diversion', TRUE))
  total_diversion_delta <- create_SIT_array(spread_for_array(df, 'diversion', TRUE))

  temperature <- create_SIT_array(spread_for_array(df, 'avg_temp'))
  temperature_delta <- create_SIT_array(spread_for_array(df, 'avg_temp', TRUE))

  # two deltas?
  delta_inflow <- df %>%
    dplyr::filter(watershed == 'N.Delta',
                  year >= start_year & year <= end_year) %>%
    dplyr::select(year, month, flow) %>%
    tidyr::spread(year, flow) %>%
    dplyr::select(-month)

  prop_Q_sutter <- CVPIAdata::prop_Q_bypass %>%
    dplyr::filter(year >= start_year & year <= end_year) %>%
    dplyr::select(year, month, prop_Q_sutter) %>%
    tidyr::spread(year, prop_Q_sutter) %>%
    dplyr::select(-month)

  prop_Q_yolo <- CVPIAdata::prop_Q_bypass %>%
    dplyr::filter(year >= start_year & year <= end_year) %>%
    dplyr::select(year, month, prop_Q_yolo) %>%
    tidyr::spread(year, prop_Q_yolo) %>%
    dplyr::select(-month)

  #in-channel habitat -flow, three values per watershed (spawning, fry, parr)

  #floodplain habitat -flow, 3d array [watershed, month, year]

  #delta habitat -flow, one value per delta

  #gate.top
  #yolo and tisdale overtoped
  #gates closed held constant at 31 in updated SIT model
  gate <- CVPIAdata::bypass_over_top %>%
    dplyr::filter(year >= start_year & year <= end_year)

  #DegDay -temperature, yearly value per watershed

  #retQ
  returnQ <- CVPIAdata::return_flow %>%
    dplyr::filter(year >= start_year & year <= end_year) %>%
    tidyr::spread(year, retQ) %>%
    dplyr::left_join(CVPIAdata::watershed_ordering) %>%
    dplyr::arrange(order) %>%
    dplyr::select(-order)

  #upSacQ -flow, month value per year
  #upsacQ
  #montly average flow at upper sacramento
  #?
  upsac_flow <- flows %>%
    dplyr::filter(year >= start_year & year <= end_year,
                  watershed == 'Upper Sacramento River') %>%
    dplyr::mutate(cms = flow * 0.028316847) %>%
    dplyr::select(year, month, cms) %>%
    tidyr::spread(year, cms)

  #egg.tmp.eff -temperature?, one value per watershed Chris hammersmark

  #inps?
  # init.adult
  # SCDELT
  # hatch.alloc
  # TISD
  # YOLO
  # p.tempMC2025 - with degday script
  # A.HARV - 3 reports summarized for the whole years available period, Dan Kratvile :)
  # P.scour.nst - expert opinion, could improve with flow + sheer
  # P.strand.early - need function
  # P.strand.late - need function
  # High.pred
  # contact
  # prop.nat

  #### units? #####

  return(list(p.diver = prop_diversion, t.diver = total_diversion,
              dlt.divers = prop_diversion_delta, dlt.divers.tot = total_diversion_delta,
              juv.tmp = temperature, juv.tmp.dlt = temperature_delta, Dlt.inf = delta_inflow,
              prop.Q.yolo = prop_Q_yolo, prop.Q.sutter = prop_Q_sutter,
              IChab = NULL, DLThab = NULL, floodP = NULL, gate.top = gate, DegDay = NULL,
              retQ = returnQ, upSacQ = upsac_flow, egg.tmp.eff = NULL))
}
