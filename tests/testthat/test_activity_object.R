

# Activity Class Tests ----------------------------------------------------


library(madstork)
library(tidyverse)

context("Activity Class")


w1 <- withdraw(Sys.Date(), 100, "monthly withdraw")
d1 <- deposit(Sys.Date(), 100, "monthly investment deposit")


test_that("activity helper functions creates object of class activity", {
  expect_equal(class(w1), "activity")
  expect_equal(class(d1), "activity")
})


test_that("activity validator function works as expected", {
  expect_error(withdraw("new-date", 100))
  expect_error(withdraw(Sys.Date(),-100))
  expect_error(withdraw(Sys.Date(), 100, desc = 100))
  expect_error(deposit("new-date", 100))
  expect_error(deposit(Sys.Date(),-100))
  expect_error(deposit(Sys.Date(), 100, desc = 100))
})

