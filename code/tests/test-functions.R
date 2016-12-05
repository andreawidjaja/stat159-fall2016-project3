library(testthat)

source("../../code/functions.R")

context("Test that choosing response works")
test_that("choosing_response works",{
  
  x <-
  expect_equal(choosing_response(x), )
  
})  


context("Test that lasso regression works")
test_that("lasso_select works",{
  y<-
  expect_equal(lasso_select(y), )
  
})  

context("Test that bayesian information criterion works")
test_that("bic_select works",{
  z<-
  expect_equal(bic_select(z), )
  
})  

context("Test that forward selection with pvalue works")
test_that("forward_p works",{
  a<-
  expect_equal(forward_p(a), )
  
})  