library(cvpiaData)
library(cvpiaHabitat)
library(cvpiaFlow)

context('Habitat inputs')

test_that('Elder Creek FR floodplain 03/1980', {
  flow_02_80 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1980-03-31', 'Elder Creek']
  
  expect_equal(
    cvpiaHabitat::set_floodplain_habitat('Elder Creek', 'fr', flow_02_80),
    cvpiaData::fr_fp[11, 3, 1],
    tolerance = .01
  )
})


test_that('Elder Creek FR floodplain 06/1990', {
  flow_06_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1980-06-30', 'Elder Creek']
  
  expect_equal(
    cvpiaHabitat::set_floodplain_habitat('Elder Creek', 'fr', flow_06_90),
    cvpiaData::fr_fp[11, 6, 11],
    tolerance = .01
  )
})


test_that('Elder Creek FR fry 03/1980', {
  flow_02_80 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1980-03-31', 'Elder Creek']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Elder Creek', 'fr', 'fry', flow_02_80),
    cvpiaData::fr_fry[11, 3, 1],
    tolerance = .01
  )
})


test_that('Elder Creek FR fry 06/1990', {
  flow_06_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1980-06-30', 'Elder Creek']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Elder Creek', 'fr', 'fry', flow_06_90),
    cvpiaData::fr_fry[11, 6, 11],
    tolerance = .01
  )
})

test_that('Elder Creek FR juv 03/1980', {
  flow_02_80 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1980-03-31', 'Elder Creek']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Elder Creek', 'fr', 'juv', flow_02_80),
    cvpiaData::fr_juv[11, 3, 1],
    tolerance = .01
  )
})


test_that('Elder Creek FR juv 06/1990', {
  flow_06_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1990-06-30', 'Elder Creek']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Elder Creek', 'fr', 'juv', flow_06_90),
    cvpiaData::fr_juv[11, 6, 11],
    tolerance = .01
  )
})

test_that('Clear Creek FR juv', {
  flow_03_80 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1980-03-31', 'Clear Creek']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Clear Creek', 'fr', 'juv', flow_03_80),
    cvpiaData::fr_juv[7, 3, 1],
    tolerance = .01
  )
})

test_that('Clear Creek FR fry', {
  flow_01_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1990-01-31', 'Clear Creek']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Clear Creek', 'fr', 'fry', flow_01_90),
    cvpiaData::fr_fry[7, 1, 11],
    tolerance = .01
  )
})

test_that('Clear Creek SR fry', {
  flow_01_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1990-01-31', 'Clear Creek']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Clear Creek', 'sr', 'fry', flow_01_90),
    cvpiaData::sr_fry[7, 1, 11],
    tolerance = .01
  )
})

test_that('Clear Creek ST fry', {
  flow_01_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1990-01-31', 'Clear Creek']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Clear Creek', 'st', 'fry', flow_01_90),
    cvpiaData::st_fry[7, 1, 11],
    tolerance = .01
  )
})

test_that('Calaveras FR fry', {
  flow_01_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1990-01-31', 'Calaveras River']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Calaveras River', 'fr', 'fry', flow_01_90),
    cvpiaData::st_fry[25, 1, 11],
    tolerance = .01
  )
  
})

test_that('Upper Sacramento FR spawn', {
  flow_01_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1990-01-31', 'Upper Sacramento River']
  
  expect_equal(
    cvpiaHabitat::set_spawning_habitat('Upper Sacramento River', 'fr', flow_01_90, month = 1),
    cvpiaData::fr_spawn[1, 1, 11],
    tolerance = .01
  )
  
})

test_that('Upper Sacramento FR fry', {
  flow_01_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1990-01-31', 'Upper Sacramento River']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Upper Sacramento River', 'fr', 'fry', flow_01_90),
    cvpiaData::fr_fry[1, 1, 11],
    tolerance = .01
  )
  
})

test_that('Lower-mid Sacramento FR fry', {
  flow_01_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1990-01-31', 'Lower-mid Sacramento River1']
  flow_01_90_2 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1990-01-31', 'Lower-mid Sacramento River2']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Lower-mid Sacramento River', 'fr', 'fry', flow_01_90, flow2 = flow_01_90_2),
    cvpiaData::fr_fry[21, 1, 11],
    tolerance = .01
  )
  
})

test_that('Lower-mid Sacramento FR fp', {
  flow_01_80 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1980-01-31', 'Lower-mid Sacramento River1']
  flow_01_80_2 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1980-01-31', 'Lower-mid Sacramento River2']
  
  expect_equal(
    cvpiaHabitat::set_floodplain_habitat('Lower-mid Sacramento River', 'fr', flow_01_80, flow2 = flow_01_80_2),
    cvpiaData::fr_fp[21, 1, 1],
    tolerance = .01
  )
  
})

test_that('Lower Sacramento FR fp', {
  flow_03_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1990-03-31', 'Lower Sacramento River']
  
  expect_equal(
    cvpiaHabitat::set_floodplain_habitat('Lower Sacramento River', 'fr', flow_03_90),
    cvpiaData::fr_fp[24, 3, 11],
    tolerance = .01
  )
  
})

test_that('Lower Sacramento FR fry', {
  flow_03_90 <- cvpiaFlow::flows_cfs[cvpiaFlow::flows_cfs$date == '1990-03-31', 'Lower Sacramento River']
  
  expect_equal(
    cvpiaHabitat::set_instream_habitat('Lower Sacramento River', 'fr', 'fry', flow_03_90),
    cvpiaData::fr_fry[24, 3, 11],
    tolerance = .01
  )
  
})

test_that('No NA values in Cached Values', {
  check_nas <- function(species) {
    x <- load_baseline_data(species = species)
    sum(any(is.na(x$IChab.fry)),
        any(is.na(x$IChab.spawn)),
        any(is.na(x$IChab.juv)),
        any(is.na(x$floodP)))
  }
  expect_equal(
    sum(check_nas('fall'), check_nas('spring'), check_nas('winter'), check_nas('steelhead')), 
    0
  )
})


