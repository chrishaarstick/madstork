
# Portfolio Class Tests ---------------------------------------------------

library(madstork)
library(tidyverse)

context("Portfolio Class")

test_that("portfolio helper function creates object of class Portfolio", {
  expect_equal(class(Portfolio("new_port")), "Portfolio")
})


test_that("portfolio validator function works as expected", {
  expect_error(Portfolio("new_port", cash=-100))
  expect_error(Portfolio(name = 10))
  expect_error(Portfolio(name = "new_port", cash = "lots"))
})
