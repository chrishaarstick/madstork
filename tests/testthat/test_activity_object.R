

# Activity Class Tests ----------------------------------------------------


library(madstork)
library(tidyverse)
library(testthat)

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


test_that("activity make functions work as expected", {
  p1 <- portfolio("new_port") %>%
    make_deposit(date = Sys.Date(), amount = 100)
  p2 <- p1 %>% make_withdraw(date = Sys.Date(), amount = 50)

  expect_equal(p1$cash, 100)
  expect_equal(p2$cash, 50)
  expect_equal(p1 %>% get_activity(),
               bind_cols(id = 1, to_tibble(deposit(date = Sys.Date(), amount = 100))))
  expect_error(p1 %>% make_withdraw(date = Sys.Date(), amount = 300))
})
