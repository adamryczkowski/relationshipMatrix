library(relationshipMatrix)

library(testthat)
options(warn=2)
context("Reading in sample matrix")

test_that("Getting the properties right", {
  df<-tibble(indepvar1=1:10, indepvar2=1:10, indepvar3=1:10, ivargrp1_1=1:10, ivargrp1_2=1:10, ivargrp1_3=1:10,
             depvar1=1:10, depvar2=1:10, depvar3=1:10, depvar4=1:10, depvar5=1:10, depvar6=1:10,
             dvargrp1_1=1:10, dvargrp1_2=1:10, dvargrp1_3=1:10,
             dvargrp2_1=1:10, dvargrp2_2=1:10, dvargrp2_3=1:10)
  df_structure<-danesurowe::create_df_from_df_structure(df)
  debugonce(read_sheet)
  m<-read_matrix('tests/testthat/test-00_matrix.xlsx', df_structure)
  expect_length(m, 29)
  expect_equal(nrow(m), 72)
  s<-m[c('depvar', 'indepvar', 'prop1', 'prop2', 'prop3', 'prop4')]
  expect_equal((s %>% filter(depvar=='depvar1' & indepvar=='indepvar1'))$prop4, 'indepvar1 vs depvar1')
  expect_equal((s %>% filter(depvar=='depvar1' & indepvar=='indepvar1'))$prop2, 'global prop2 value')
  mref<-readRDS('tests/testthat/test-00_reference.rds')
  expect_equal(m, mref)
#  saveRDS(m, 'tests/testthat/test-00_reference.rds')
})

