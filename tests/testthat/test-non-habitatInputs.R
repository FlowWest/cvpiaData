library(cvpiaData)
library(cvpiaHabitat)
library(cvpiaFlow)

context('Non-Habitat inputs')

test_that("No NA values in cached values", 
          expect_equal({
          x <- load_baseline_data("fall")
            sum(any(is.na(x$p.diver)),
                any(is.na(x$t.diver)),
                any(is.na(x$DegDay)),
                any(is.na(x$retQ)),
                any(is.na(x$p.tempMC20)),
                any(is.na(x$prop.pulse))
                )}, 
            0
          ))
