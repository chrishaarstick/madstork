
# Portfolio Class Tests ---------------------------------------------------

library(madstork)
library(tidyverse)

context("Portfolio Class")

test_that("portfolio helper function creates object of class portfolio", {
  expect_equal(class(portfolio("new_port")), "portfolio")
})


test_that("portfolio validator function works as expected", {
  expect_error(portfolio("new_port", cash=-100))
  expect_error(portfolio(name = 10))
  expect_error(portfolio(name = "new_port", cash = "lots"))
})

test_that("portfolio getter functions work as expected", {
  expect_equal(portfolio("new_port", cash=100) %>% get_cash(.), 100)
})
