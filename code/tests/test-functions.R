library(testthat)
scaled_data=read.csv("../../data/generated_data/scaled_data.csv")[,-1]
source("../../code/scripts/functions.R")


context("Choosing Response Function") 
test_that("Making sure output of choosing response is of right dimensions", {
  expect_equal(ncol(choosing_response(scaled_data, "C150_4")),ncol(scaled_data)-6) 
  })


context("Test that lasso regression works")
test_that("lasso_select works",{
  expect_equal(nrow(lasso_select(choosing_response(scaled_data,"C150_4"), "C150_4", 5)), 5)
})  

context("Test that bayesian information criterion works")
test_that("bic_select works",{
  expect_equal(nrow(bic_select(choosing_response(scaled_data,"C150_4"), "C150_4", 5)), 5)
  
})  

context("Test that forward selection with pvalue works")
test_that("forward_p works",{
  expect_equal(nrow(forward_p(choosing_response(scaled_data,"C150_4"), 5)), 5)
})  

