
# Trades Object Tests -----------------------------------------------------


library(madstork)
library(tidyverse)

context("Trades Class")

b1 <- buy(date = Sys.Date(), symbol = "SPY", quantity = 10, price = 100)

test_that("trade helper functions creates object of class trade",{
  expect_equal(class(b1), "trade")
})

test_that("trade helper functions internals work correctly", {
  expect_equal(b1$amount, 1000)
  expect_equal(b1$type, "buy")
})

test_that("trade make functions work as expected", {
  p1 <- portfolio("new_port", cash=0) %>%
    make_deposit(Sys.Date(), amount = 1000) %>%
    make_buy(Sys.Date(), symbol = "SPY", quantity = 10, price = 100)

  expect_equal(p1$cash, 0)
  expect_equal(nrow(p1$activity), 1)
  expect_equal(nrow(p1$trades), 1)
  expect_equal(p1$activity$date_added, Sys.Date())
  expect_equal(p1$trade$date_added, Sys.Date())
})
