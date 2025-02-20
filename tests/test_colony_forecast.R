#------------------------------------------------------------------------------
# (C) Crown Copyright 2025 Defence Science and Technology Laboratory UK
#
# Approval for wider use or release must be sought from:
#         Intellectual Property Department
#         Defence Science and Technology Laboratory
#         Porton Down, Salisbury, Wiltshire, SP4 0JQ.
#
# The software/data, including any associated documentation, is supplied without
# warranty of performance, and in particular Dstl does not warrant, guarantee
# or make any representations regarding the use or the results of the use of the
# software in terms of currentness, accuracy, reliability, correctness,
# merchantability, fitness for purpose or otherwise.
#------------------------------------------------------------------------------
rm(list=ls()) # clear workspace

library(testthat) # this R package must be installed in order to run this script

#------------------------------------------------------------------------------
# USER INPUT: specify the working directory in which the model scripts are saved:
working_dir_path='insert/working/directory/path/here'

setwd(working_dir_path)

#------------------------------------------------------------------------------
# source model scripts:
source('./colony_forecast.R')
source('./helper_functions.R')

#------------------------------------------------------------------------------
# load dummy dataset for testing model arguments:
load('./tests/testing-dummydata.RData')

# vector to store 4 edge cases for the numeric arguments that need testing:
EDGE.CASES=list(NULL, NA, NaN, '')

#------------------------------------------------------------------------------
# start.date:

test_that("start.date that isn't a date fails with useful message", {
  expect_error(colony_forecast(start.date='TextNotDate', nmonth=NMONTH.TEST,
                                        nreps=NREPS.TEST,
                                        I0=I0.TEST, A0=A0.TEST,
                                        delta=DELTA.TEST,
                                        breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                        breeding.control=BREEDING.CONTROL.TEST,
                                        lsz.dist=LITTER.SIZE.DIST.TEST,
                                        transfer.rate=TRANSFER.RATE.TEST,
                                        issues=ISSUES.TEST,
                                        relocations=RELOCATIONS.TEST,
                                        quantiles=QUANTILES.TEST),
               regexp='start.date must be YYYY-MM-DD')
})

test_that("start.date that isn't in correct format fails with useful message", {
  expect_error(colony_forecast(start.date='190-01-01', nmonth=NMONTH.TEST,
                                        nreps=NREPS.TEST,
                                        I0=I0.TEST, A0=A0.TEST,
                                        delta=DELTA.TEST,
                                        breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                        breeding.control=BREEDING.CONTROL.TEST,
                                        lsz.dist=LITTER.SIZE.DIST.TEST,
                                        transfer.rate=TRANSFER.RATE.TEST,
                                        issues=ISSUES.TEST,
                                        relocations=RELOCATIONS.TEST,
                                        quantiles=QUANTILES.TEST),
               regexp='start.date must be YYYY-MM-DD')
})

#------------------------------------------------------------------------------
# nmonth:

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in nmonth argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=EDGE.CASES[[i]],
                         nreps=NREPS.TEST,
                         I0=I0.TEST, A0=A0.TEST,
                         delta=DELTA.TEST,
                         breeding.pairs.planned=BREEDING.PAIRS.TEST,
                         breeding.control=BREEDING.CONTROL.TEST,
                         lsz.dist=LITTER.SIZE.DIST.TEST,
                         transfer.rate=TRANSFER.RATE.TEST,
                         issues=ISSUES.TEST,
                         relocations=RELOCATIONS.TEST,
                         quantiles=QUANTILES.TEST))
    })
})

#------------------------------------------------------------------------------
# nreps:

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in nreps argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                          nreps=EDGE.CASES[[i]],
                                          I0=I0.TEST, A0=A0.TEST,
                                          delta=DELTA.TEST,
                                          breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                          breeding.control=BREEDING.CONTROL.TEST,
                                          lsz.dist=LITTER.SIZE.DIST.TEST,
                                          transfer.rate=TRANSFER.RATE.TEST,
                                          issues=ISSUES.TEST,
                                          relocations=RELOCATIONS.TEST,
                                          quantiles=QUANTILES.TEST))
  })
})

test_that("nreps=0 fails with useful message", {
  expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                        nreps=0,
                                        I0=I0.TEST, A0=A0.TEST,
                                        delta=DELTA.TEST,
                                        breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                        breeding.control=BREEDING.CONTROL.TEST,
                                        lsz.dist=LITTER.SIZE.DIST.TEST,
                                        transfer.rate=TRANSFER.RATE.TEST,
                                        issues=ISSUES.TEST,
                                        relocations=RELOCATIONS.TEST,
                                        quantiles=QUANTILES.TEST),
               regexp='nreps must be positive')
})

test_that("Function works fine for nreps=1", {
  expect_no_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                        nreps=1,
                                        I0=I0.TEST, A0=A0.TEST,
                                        delta=DELTA.TEST,
                                        breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                        breeding.control=BREEDING.CONTROL.TEST,
                                        lsz.dist=LITTER.SIZE.DIST.TEST,
                                        transfer.rate=TRANSFER.RATE.TEST,
                                        issues=ISSUES.TEST,
                                        relocations=RELOCATIONS.TEST,
                                        quantiles=QUANTILES.TEST))
})

#------------------------------------------------------------------------------
# I0 and A0:

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in I0 argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                          nreps=NREPS.TEST,
                                          I0=EDGE.CASES[[i]], A0=A0.TEST,
                                          delta=DELTA.TEST,
                                          breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                          breeding.control=BREEDING.CONTROL.TEST,
                                          lsz.dist=LITTER.SIZE.DIST.TEST,
                                          transfer.rate=TRANSFER.RATE.TEST,
                                          issues=ISSUES.TEST,
                                          relocations=RELOCATIONS.TEST,
                                          quantiles=QUANTILES.TEST))
  })
})

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in A0 argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                          nreps=NREPS.TEST,
                                          I0=I0.TEST, A0=EDGE.CASES[[i]],
                                          delta=DELTA.TEST,
                                          breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                          breeding.control=BREEDING.CONTROL.TEST,
                                          lsz.dist=LITTER.SIZE.DIST.TEST,
                                          transfer.rate=TRANSFER.RATE.TEST,
                                          issues=ISSUES.TEST,
                                          relocations=RELOCATIONS.TEST,
                                          quantiles=QUANTILES.TEST))
  })
})

test_that("Function works fine if I0=0", {
  expect_no_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                        nreps=NREPS.TEST,
                                        I0=0, A0=A0.TEST,
                                        delta=DELTA.TEST,
                                        breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                        breeding.control=BREEDING.CONTROL.TEST,
                                        lsz.dist=LITTER.SIZE.DIST.TEST,
                                        transfer.rate=TRANSFER.RATE.TEST,
                                        issues=ISSUES.TEST,
                                        relocations=RELOCATIONS.TEST,
                                        quantiles=QUANTILES.TEST))
})

test_that("Function works fine for A0=0", {
  expect_no_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                           nreps=NREPS.TEST,
                                           I0=I0.TEST, A0=0,
                                           delta=DELTA.TEST,
                                           breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                           breeding.control=BREEDING.CONTROL.TEST,
                                           lsz.dist=LITTER.SIZE.DIST.TEST,
                                           transfer.rate=TRANSFER.RATE.TEST,
                                           issues=ISSUES.TEST,
                                           relocations=RELOCATIONS.TEST,
                                           quantiles=QUANTILES.TEST))
})

test_that("Function works fine for I0=0 and A0=0", {
  expect_no_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                           nreps=NREPS.TEST,
                                           I0=0, A0=0,
                                           delta=DELTA.TEST,
                                           breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                           breeding.control=BREEDING.CONTROL.TEST,
                                           lsz.dist=LITTER.SIZE.DIST.TEST,
                                           transfer.rate=TRANSFER.RATE.TEST,
                                           issues=ISSUES.TEST,
                                           relocations=RELOCATIONS.TEST,
                                           quantiles=QUANTILES.TEST))
})

#------------------------------------------------------------------------------
# delta:

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in delta argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                          nreps=NREPS.TEST,
                                          I0=I0.TEST, A0=A0.TEST,
                                          delta=EDGE.CASES[[i]],
                                          breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                          breeding.control=BREEDING.CONTROL.TEST,
                                          lsz.dist=LITTER.SIZE.DIST.TEST,
                                          transfer.rate=TRANSFER.RATE.TEST,
                                          issues=ISSUES.TEST,
                                          relocations=RELOCATIONS.TEST,
                                          quantiles=QUANTILES.TEST))
  })
})

test_that("Error if delta is not of length 2, with useful message", {
  expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                           nreps=NREPS.TEST,
                                           I0=I0.TEST, A0=A0.TEST,
                                           delta=0.02,
                                           breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                           breeding.control=BREEDING.CONTROL.TEST,
                                           lsz.dist=LITTER.SIZE.DIST.TEST,
                                           transfer.rate=TRANSFER.RATE.TEST,
                                           issues=ISSUES.TEST,
                                           relocations=RELOCATIONS.TEST,
                                           quantiles=QUANTILES.TEST),
               regexp='delta must be a vector of length 2')
})

test_that("Function works if delta is of length 2 but is an unnamed vector", {
  expect_no_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                           nreps=NREPS.TEST,
                                           I0=I0.TEST, A0=A0.TEST,
                                           delta=c(0.02, 0.01),
                                           breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                           breeding.control=BREEDING.CONTROL.TEST,
                                           lsz.dist=LITTER.SIZE.DIST.TEST,
                                           transfer.rate=TRANSFER.RATE.TEST,
                                           issues=ISSUES.TEST,
                                           relocations=RELOCATIONS.TEST,
                                           quantiles=QUANTILES.TEST))
})

#------------------------------------------------------------------------------
# breeding.pairs.planned:

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in breeding.pairs.planned argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                          nreps=NREPS.TEST,
                                          I0=I0.TEST, A0=A0.TEST,
                                          delta=DELTA.TEST,
                                          breeding.pairs.planned=EDGE.CASES[[i]],
                                          breeding.control=BREEDING.CONTROL.TEST,
                                          lsz.dist=LITTER.SIZE.DIST.TEST,
                                          transfer.rate=TRANSFER.RATE.TEST,
                                          issues=ISSUES.TEST,
                                          relocations=RELOCATIONS.TEST,
                                          quantiles=QUANTILES.TEST))
  })
})

test_that("function works for breeding.pairs.planned=0", {
  expect_no_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                           nreps=NREPS.TEST,
                                           I0=I0.TEST, A0=A0.TEST,
                                           delta=DELTA.TEST,
                                           breeding.pairs.planned=0,
                                           breeding.control=BREEDING.CONTROL.TEST,
                                           lsz.dist=LITTER.SIZE.DIST.TEST,
                                           transfer.rate=TRANSFER.RATE.TEST,
                                           issues=ISSUES.TEST,
                                           relocations=RELOCATIONS.TEST,
                                           quantiles=QUANTILES.TEST))
})

#------------------------------------------------------------------------------
# breeding.control:

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in breeding.control argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                          nreps=NREPS.TEST,
                                          I0=I0.TEST, A0=A0.TEST,
                                          delta=DELTA.TEST,
                                          breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                          breeding.control=EDGE.CASES[[i]],
                                          lsz.dist=LITTER.SIZE.DIST.TEST,
                                          transfer.rate=TRANSFER.RATE.TEST,
                                          issues=ISSUES.TEST,
                                          relocations=RELOCATIONS.TEST,
                                          quantiles=QUANTILES.TEST))
  })
})

test_that("function works for breeding.control=0", {
  expect_no_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                        nreps=NREPS.TEST,
                                        I0=I0.TEST, A0=A0.TEST,
                                        delta=DELTA.TEST,
                                        breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                        breeding.control=0,
                                        lsz.dist=LITTER.SIZE.DIST.TEST,
                                        transfer.rate=TRANSFER.RATE.TEST,
                                        issues=ISSUES.TEST,
                                        relocations=RELOCATIONS.TEST,
                                        quantiles=QUANTILES.TEST))
})

#------------------------------------------------------------------------------
# breeding.pairs.planned vs. breeding.control:

# - run model for breeding.pairs.planned=0 (with random seed set to 123):
out1=colony_forecast(seed=123, start.date=TEST.START, nmonth=NMONTH.TEST,
                         nreps=NREPS.TEST,
                         I0=I0.TEST, A0=A0.TEST,
                         delta=DELTA.TEST,
                         breeding.pairs.planned=0,
                         breeding.control=BREEDING.CONTROL.TEST,
                         lsz.dist=LITTER.SIZE.DIST.TEST,
                         transfer.rate=TRANSFER.RATE.TEST,
                         issues=ISSUES.TEST,
                         relocations=RELOCATIONS.TEST,
                         quantiles=QUANTILES.TEST)

# - run model for breeding.control=0 (with random seed set to 123):
out2=colony_forecast(seed=123, start.date=TEST.START, nmonth=NMONTH.TEST,
                              nreps=NREPS.TEST,
                              I0=I0.TEST, A0=A0.TEST,
                              delta=DELTA.TEST,
                              breeding.pairs.planned=BREEDING.PAIRS.TEST,
                              breeding.control=0,
                              lsz.dist=LITTER.SIZE.DIST.TEST,
                              transfer.rate=TRANSFER.RATE.TEST,
                              issues=ISSUES.TEST,
                              relocations=RELOCATIONS.TEST,
                              quantiles=QUANTILES.TEST)

# test that out1=out2:
sapply(1:length(out1$Xdf), function(j) {
  # print(NREPS.TEST-j)
  test_that("Same results for infants when breeding.pairs.planned=0 as for breeding.control=0", {
    expect_equal(object=out1$Xdf[[j]]$I, expected=out2$Xdf[[j]]$I)
  })
  test_that("Same results for adults when breeding.pairs.planned=0 as for breeding.control=0", {
    expect_equal(object=out1$Xdf[[j]]$A, expected=out2$Xdf[[j]]$A)
  })
})

#------------------------------------------------------------------------------
# lsz.dist:

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in lsz.dist argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                          nreps=NREPS.TEST,
                                          I0=I0.TEST, A0=A0.TEST,
                                          delta=DELTA.TEST,
                                          breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                          breeding.control=BREEDING.CONTROL.TEST,
                                          lsz.dist=EDGE.CASES[[i]],
                                          transfer.rate=TRANSFER.RATE.TEST,
                                          issues=ISSUES.TEST,
                                          relocations=RELOCATIONS.TEST,
                                          quantiles=QUANTILES.TEST))
  })
})

test_that("Function works when lsz.dist is of length 1", {
  expect_no_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                        nreps=NREPS.TEST,
                                        I0=I0.TEST, A0=A0.TEST,
                                        delta=DELTA.TEST,
                                        breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                        breeding.control=BREEDING.CONTROL.TEST,
                                        lsz.dist=1,
                                        transfer.rate=TRANSFER.RATE.TEST,
                                        issues=ISSUES.TEST,
                                        relocations=RELOCATIONS.TEST,
                                        quantiles=QUANTILES.TEST))
})

#------------------------------------------------------------------------------
# transfer.rate:

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in lsz.dist argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                          nreps=NREPS.TEST,
                                          I0=I0.TEST, A0=A0.TEST,
                                          delta=DELTA.TEST,
                                          breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                          breeding.control=BREEDING.CONTROL.TEST,
                                          lsz.dist=LITTER.SIZE.DIST.TEST,
                                          transfer.rate=EDGE.CASES[[i]],
                                          issues=ISSUES.TEST,
                                          relocations=RELOCATIONS.TEST,
                                          quantiles=QUANTILES.TEST))
  })
})

test_that("Function works for transfer.rate=0", {
  expect_no_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                        nreps=NREPS.TEST,
                                        I0=I0.TEST, A0=A0.TEST,
                                        delta=DELTA.TEST,
                                        breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                        breeding.control=BREEDING.CONTROL.TEST,
                                        lsz.dist=LITTER.SIZE.DIST.TEST,
                                        transfer.rate=0,
                                        issues=ISSUES.TEST,
                                        relocations=RELOCATIONS.TEST,
                                        quantiles=QUANTILES.TEST))
})

test_that("Function failsfor transfer.rate<0", {
  expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                           nreps=NREPS.TEST,
                                           I0=I0.TEST, A0=A0.TEST,
                                           delta=DELTA.TEST,
                                           breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                           breeding.control=BREEDING.CONTROL.TEST,
                                           lsz.dist=LITTER.SIZE.DIST.TEST,
                                           transfer.rate=-0.1,
                                           issues=ISSUES.TEST,
                                           relocations=RELOCATIONS.TEST,
                                           quantiles=QUANTILES.TEST))
})

#------------------------------------------------------------------------------
# run model for zero breeding, zero transfers and zero deaths
# - expect outputs for infants and adults to be constant, i.e. the same as initial values for each month

out3=colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                         nreps=NREPS.TEST,
                         I0=I0.TEST, A0=A0.TEST,
                         delta=c(0,0),
                         breeding.pairs.planned=BREEDING.PAIRS.TEST,
                         breeding.control=0,
                         lsz.dist=LITTER.SIZE.DIST.TEST,
                         transfer.rate=0,
                         issues=ISSUES.TEST,
                         relocations=RELOCATIONS.TEST,
                         quantiles=QUANTILES.TEST)

# reference matrix of initial values of infants and adults, for comparison to model output:
I0.reference=matrix(rep(I0.TEST), nrow=5, ncol=25)
A0.reference=matrix(rep(A0.TEST), nrow=5, ncol=25)

test_that("Constant number of infants when delta, breeding.control and transfer.rate are all zero", {
  # - test that all quantiles are equal to I0 value:
  expect_equal(unname(out3$Iq), expected=I0.reference, ignore_attr = TRUE)
})

test_that("Constant number of adults when delta, breeding.control and transfer.rate are all zero", {
  # - test that all quantiles are equal to A0 value:
  expect_equal(unname(out3$Aq), expected=A0.reference, ignore_attr = TRUE)
})

#------------------------------------------------------------------------------
# issues:

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in issues argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                          nreps=NREPS.TEST,
                                          I0=I0.TEST, A0=A0.TEST,
                                          delta=DELTA.TEST,
                                          breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                          breeding.control=BREEDING.CONTROL.TEST,
                                          lsz.dist=LITTER.SIZE.DIST.TEST,
                                          transfer.rate=TRANSFER.RATE.TEST,
                                          issues=EDGE.CASES[[i]],
                                          relocations=RELOCATIONS.TEST,
                                          quantiles=QUANTILES.TEST))
  })
})

test_that("Function throws error with useful message if length of issues is neither 1 nor nmonth", {
  expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                        nreps=NREPS.TEST,
                                        I0=I0.TEST, A0=A0.TEST,
                                        delta=DELTA.TEST,
                                        breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                        breeding.control=BREEDING.CONTROL.TEST,
                                        lsz.dist=LITTER.SIZE.DIST.TEST,
                                        transfer.rate=TRANSFER.RATE.TEST,
                                        issues=c(0,0,0,0),
                                        relocations=RELOCATIONS.TEST,
                                        quantiles=QUANTILES.TEST),
               regexp='issues must have length 1 or nmonth')
})

#------------------------------------------------------------------------------
# relocations:

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in relocations argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                          nreps=NREPS.TEST,
                                          I0=I0.TEST, A0=A0.TEST,
                                          delta=DELTA.TEST,
                                          breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                          breeding.control=BREEDING.CONTROL.TEST,
                                          lsz.dist=LITTER.SIZE.DIST.TEST,
                                          transfer.rate=TRANSFER.RATE.TEST,
                                          issues=ISSUES.TEST,
                                          relocations=EDGE.CASES[[i]],
                                          quantiles=QUANTILES.TEST))
  })
})

test_that("Function throws error with useful message if length of relocations is neither 1 nor nmonth", {
  expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                        nreps=NREPS.TEST,
                                        I0=I0.TEST, A0=A0.TEST,
                                        delta=DELTA.TEST,
                                        breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                        breeding.control=BREEDING.CONTROL.TEST,
                                        lsz.dist=LITTER.SIZE.DIST.TEST,
                                        transfer.rate=TRANSFER.RATE.TEST,
                                        issues=ISSUES.TEST,
                                        relocations=c(0,0,0,0),
                                        quantiles=QUANTILES.TEST),
               regexp='relocations must have length 1 or nmonth')
})

#------------------------------------------------------------------------------
# quantiles:

sapply(1:length(EDGE.CASES), function(i) {
  test_that("Function throws error and stops for edge cases in quantiles argument", {
    expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                          nreps=NREPS.TEST,
                                          I0=I0.TEST, A0=A0.TEST,
                                          delta=DELTA.TEST,
                                          breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                          breeding.control=BREEDING.CONTROL.TEST,
                                          lsz.dist=LITTER.SIZE.DIST.TEST,
                                          transfer.rate=TRANSFER.RATE.TEST,
                                          issues=ISSUES.TEST,
                                          relocations=RELOCATIONS.TEST,
                                          quantiles=EDGE.CASES[[i]]))
  })
})

test_that("Function throws error and stops for edge cases in quantiles argument", {
  expect_error(colony_forecast(start.date=TEST.START, nmonth=NMONTH.TEST,
                                        nreps=NREPS.TEST,
                                        I0=I0.TEST, A0=A0.TEST,
                                        delta=DELTA.TEST,
                                        breeding.pairs.planned=BREEDING.PAIRS.TEST,
                                        breeding.control=BREEDING.CONTROL.TEST,
                                        lsz.dist=LITTER.SIZE.DIST.TEST,
                                        transfer.rate=TRANSFER.RATE.TEST,
                                        issues=ISSUES.TEST,
                                        relocations=RELOCATIONS.TEST,
                                        quantiles=c(110)))
})


