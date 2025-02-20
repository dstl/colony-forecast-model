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
rm(list=ls()) # clear the workspace

library(testthat) # this R package must be installed to be able to run this script

#------------------------------------------------------------------------------
# USER INPUT: specify the working directory in which the model scripts are saved:
working_dir_path='insert/working/directory/path/here'

setwd(working_dir_path)

#------------------------------------------------------------------------------
# source model scripts:
source('./colony_forecast.R')
source('./helper_functions.R')

#------------------------------------------------------------------------------
# dummy data for distribution of litter sizes, used for testing only:
# - illustrative example for 20 singlets, 60 sets of twins and 20 sets of triplets
LitterSizes_dummy=c(rep(1,20), rep(2,60), rep(3, 20))

#------------------------------------------------------------------------------

# tests for D_i_set:
test_that("empty first argument fails", {
  expect_error(
    D_i_set(breeding.pairs.planned=c(),
            nmonth=1)
    )
})

test_that("empty second argument fails", {
  expect_error(
    D_i_set(breeding.pairs.planned=10,
            nmonth=c())
    )
})

test_that("single breeding pair & nmonth=1 --> output length one", {
  expect_length(
    D_i_set(breeding.pairs.planned=10,
            nmonth=1) ,
    n=1)
})

test_that("single breeding pair & nmonth>1 --> output length nmonth", {
  expect_length(
    D_i_set(breeding.pairs.planned=10,
            nmonth=5) ,
    n=5)
})

test_that("single breeding pair & nmonth>1 --> output equal to rep(input,nmonth)", {
  expect_equal(
    D_i_set(breeding.pairs.planned=10,
            nmonth=3) ,
    expected=c(10,10,10))
})

test_that("multiple breeding pairs & nmonth=1 --> fails", {
  expect_error(
    D_i_set(breeding.pairs.planned=c(10,12),
            nmonth=1)
    )
})

test_that("multiple breeding pairs & nmonth>1 --> output length nmonth", {
  expect_length(
    D_i_set(breeding.pairs.planned=c(10,12,14),
            nmonth=3) ,
    n=3)
})

test_that("multiple breeding pairs & nmonth>1 --> output equal to input", {
  expect_equal(
    D_i_set(breeding.pairs.planned=c(10,12,14),
            nmonth=3) ,
    expected=c(10,12,14))
})


#------------------------------------------------------------------------------
# tests for f_i_set:
test_that("empty first argument fails", {
  expect_error(
    f_i_set(breeding.control=c(),
            nmonth=1)
    )
})

test_that("empty second argument fails", {
  expect_error(
    f_i_set(breeding.control=0.5,
            nmonth=c())
    )
})

test_that("one breeding control & nmonth=1 --> output length one", {
  expect_length(
    f_i_set(breeding.control=0.5,
            nmonth=1) ,
    n=1)
})

test_that("one breeding control & nmonth>1 --> output length nmonth", {
  expect_length(
    f_i_set(breeding.control=0.5,
            nmonth=3) ,
    n=3)
})

test_that("multiple breeding controls & nmonth=1 --> fails", {
  expect_error(
    f_i_set(breeding.control=c(0.5, 0.6),
            nmonth=1)
    )
})

test_that("length(breeding.control)>nmonth --> fails", {
  expect_error(
    f_i_set(breeding.control=c(0.5, 0.6, 0.7),
            nmonth=2)
    )
})

test_that("length(breeding.control)=nmonth>1 --> output equal to input", {
  expect_equal(
    f_i_set(breeding.control=c(0.5, 0.6, 0.7),
            nmonth=3) ,
    expected=c(0.5,0.6,0.7))
})

#------------------------------------------------------------------------------
# tests for t_lm_set:
test_that("empty argument fails", {
  expect_error(
    t_lm_set(D_i_hat=c())
    )
})

test_that("input constant --> zero NA values in output", {
  expect_true(
    length(which(is.na(t_lm_set(D_i_hat=c(3,3,3)))))==0
    )
})

test_that("max(input)-min(input) NA values in output", {
  expect_true(
    is.na(t_lm_set(D_i_hat=c(2,3,3))[3])
    )
})

test_that("max(input)-min(input) NA values in output", {
  expect_true(
    length(which(is.na(t_lm_set(D_i_hat=c(10,20,30)))))==20
  )
})

#------------------------------------------------------------------------------
# tests for dams_adjust:
test_that("expected output calculated", {
  expect_equal(
    round(dams_adjust(i=2, D_i_hat=c(1,2), t_lm=c(2,NA), seed=123), 6),
    expected=c(2, 6.115031)
  )
})

test_that("function fails when i=1", {
  expect_error(
    dams_adjust(i=1, D_i_hat=c(1,2,3), t_lm=c(2,3,4))
  )
})

test_that("function fails if number of dams increases but no NA values in t_lm", {
  expect_error(
    dams_adjust(i=2, D_i_hat=c(2,3), t_lm=c(2,2))
    )
})

test_that("NA values are replaced when number of dams increases (by 1)", {
  expect_true(
    length(which(is.na(dams_adjust(i=2, D_i_hat=c(2,3), t_lm=c(2,NA)))))==0
  )
})

test_that("NA values are replaced when number of dams increases (by more than 1)", {
  expect_true(
    length(which(is.na(dams_adjust(i=2, D_i_hat=c(2,8), t_lm=c(2,2,NA,NA,NA,NA,NA,NA)))))==0
  )
})

test_that("Dams are switched to NA when decreasing from one month to the next", {
  expect_true(
    length(which(is.na(dams_adjust(i=2, D_i_hat=c(3,1), t_lm=c(2,3,4)))))==2
  )
})

#------------------------------------------------------------------------------
# tests for new_births:
test_that("b_i=0 if mu_i is empty", {
  expect_equal(
    new_births(lsz.dist=LitterSizes_dummy, mu_i=integer(), t_lm=c(2,3,4))$b_i,
    expected=0
  )
})

test_that("t_lm is unchanged if mu_i is empty", {
  expect_equal(
    new_births(lsz.dist=LitterSizes_dummy, mu_i=integer(), t_lm=c(2,3,4))$t_lm,
    expected=c(2,3,4)
  )
})

test_that("expected number of births is 6 (given random seed)", {
  expect_equal(
    new_births(lsz.dist=LitterSizes_dummy, mu_i=c(1,2,3), t_lm=c(2,3,4), seed=123)$b_i,
    expected=6
  )
})

test_that("expected values of t_lm (given random seed)", {
  expect_equal(
    round(new_births(lsz.dist=LitterSizes_dummy, mu_i=c(1,2,3), t_lm=c(2,3,4), seed=123)$t_lm, 4),
    expected=c(7.3532, 8.3762, 9.0182)
  )
})

#------------------------------------------------------------------------------
# tests for transfers:
test_that("zero transfers if transfer.rate=0", {
  expect_equal(
    transfers(0, 100),
    expected=0
  )
})

test_that("zero transfers if number of infants is zero", {
  expect_equal(
    transfers(0.02, 0),
    expected=0
  )
})

test_that("zero transfers for zero inputs", {
  expect_equal(
    transfers(0, 0),
    expected=0
  )
})

#------------------------------------------------------------------------------
# tests for infant deaths:
test_that("zero deaths if zero inputs", {
  expect_equal(
    deaths_infant(0, 0, 0, 0),
    expected=0
  )
})

test_that("zero deaths if zero infants and zero births", {
  expect_equal(
    deaths_infant(0.01, 0, 0, 6),
    expected=0
  )
})

test_that("zero deaths if death rate is zero", {
  expect_equal(
    deaths_infant(0, 100, 10, 6),
    expected=0
  )
})

test_that("zero deaths if w_i > (nI_i_1 + b_i)", {
  expect_equal(
    deaths_infant(0.01, 10, 1, 12),
    expected=0
  )
})

#------------------------------------------------------------------------------
# tests for adult deaths:
test_that("zero deaths if zero inputs", {
  expect_equal(
    deaths_adult(0, 0, 0),
    expected=0
  )
})

test_that("zero deaths if zero adults and zero weanings", {
  expect_equal(
    deaths_adult(0.5, 0, 0),
    expected=0
  )
})

test_that("zero deaths if death rate is zero", {
  expect_equal(
    deaths_adult(0, 100, 10),
    expected=0
  )
})
