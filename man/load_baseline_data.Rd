% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_baseline_data.R
\name{load_baseline_data}
\alias{load_baseline_data}
\title{Load SIT Model Inputs for Years 1980-1999}
\usage{
load_baseline_data(species)
}
\arguments{
\item{species}{Call function with one of the following options to load species specific model inputs: 'fall', 'winter', 'spring', 'steelhead'}
}
\value{
list of model inputs
}
\description{
returns list of data values used by model
}
\details{
The list returned contains the following data:
\itemize{
  \item \link[=ptemp20mc]{p.tempMC20} = Proportion of days in month exceeding 20°C
  \item \link[=prop_diversion]{p.diver} = Proportion of flow diverted for each watershed
  \item \link[=total_diversion]{t.diver} = Total flow diverted for each watershed
  \item \link[=dlt_divers]{dlt.divers} = Proportion of flow diverted in the North and South deltas
  \item \link[=dlt_divers_tot]{dlt.divers.tot} = Total flow diverted in the North and South deltas
  \item \link[=rearing_temps]{juv.tmp} = Monthly mean temperature during juvenile outmigration in each watershed
  \item \link[=dlt_temps]{juv.tmp.dlt} = Monthly mean temperature during juvenile outmigration in the deltas
  \item \link[=dlt_inflow]{Dlt.inf} = Total inflow in the deltas
  \item \link[=dlt_hab]{DLThab} = Habitat areas for the deltas
  \item \link[=bypass_prop_Q]{prop.Q.bypasses} = Proportion of Sacramento River into bypasses
  \item \link[=inchannel_bypass]{IChab.bypass} = Instream habitat areas for each bypass
  \item \link[=floodplain_bypass]{floodp.bypass} = Floodplain habitat areas for each bypass
  \item \link[=inundation_durations]{fp.weeks} = Floodplain habitat duration inundated for each watershed
  \item \link[=bypass_over]{gate.top} = Monthly record of bypasses overtopped
  \item \link[=degday]{DegDay} = Accumulated degree days for adults before spawning
  \item \link[=returnQ]{retQ} = Proportion of flows at tributary junction from natal watershed in October
  \item \link[=upsac_flow]{upSacQ} = Upper Sacramento flow
  \item \link[=freeportQcms]{freeportQ} = Flow at Georgiana slough
  \item \link[=cross_channel_gates]{dlt.gates} = Total and proportion number of days the cross channel gates are typically closed per month
  \item \link[=egg_temp_effect]{egg.tmp.eff} = Temperature effect on egg mortality
  \item \link[=misc_data]{inps} = Miscellaneous model inputs
  \item \link[=misc_delta]{Dlt.inp} = Miscellaneous delta model inputs
  \item \link[=prop_pulse]{prop.pulse} = Proportion pulse flows
  \item \link[=med_flow]{medQ} = Monthly median flows
}

Fall Run Specific Outputs:
\itemize{
  \item \link[=fr_spawn]{IChab.spawn} = Spawning habitat areas for each watershed
  \item \link[=fr_fry]{IChab.fry} = Fry instream habitat areas for each watershed
  \item \link[=fr_juv]{IChab.juv} = Juvenile instream habitat areas for each watershed
  \item \link[=fr_fp]{floodP} = Floodplain habitat areas for each watershed
}

Spring Run Specific Outputs:
\itemize{
  \item \link[=sr_spawn]{IChab.spawn} = Spawning habitat areas for each watershed
  \item \link[=sr_fry]{IChab.fry} = Fry instream habitat areas for each watershed
  \item \link[=sr_juv]{IChab.juv} = Juvenile instream habitat areas for each watershed
  \item \link[=sr_fp]{floodP} = Floodplain habitat areas for each watershed
  \item \link[=pools]{SR.pools} = Holding Habitat
  \item \link[=has_spring_run]{has.SR} = Spring Run are present in watershed
}

Steelhead Specific Outputs:
\itemize{
  \item \link[=st_spawn]{IChab.spawn} = Spawning habitat areas for each watershed
  \item \link[=st_fry]{IChab.fry} = Fry instream habitat areas for each watershed
  \item \link[=st_juv]{IChab.juv} = Juvenile instream habitat areas for each watershed
  \item \link[=st_fp]{floodP} = Floodplain habitat areas for each watershed
  \item \link[=pools]{ST.pools} = Holding Habitat
  \item \link[cvpiaHabitat]{IChab.adult} = Adult instream habitat areas for each watershed
  \item \link[=meanQ]{meanQ} = Monthly mean flow for each watershed
}

Winter Run Specific Outputs:
\itemize{
  \item \link[=wr_spawn]{IChab.spawn} = Spawning habitat areas for each Sacramento River segment
  \item \link[=wr_fry]{IChab.fry} = Fry instream habitat areas for each Sacramento River segment
  \item \link[=wr_juv]{IChab.juv} = Juvenile instream habitat areas for each Sacramento River segment
  \item \link[=wr_fp]{floodP} = Floodplain habitat areas for each Sacramento River segment
}
}
\examples{
# place these two lines within the SIT Salmon Population Model function to load the data
all_inputs <- cvpiaData::load_baseline_data('fall')
list2env(all_inputs, envir = environment())
}
