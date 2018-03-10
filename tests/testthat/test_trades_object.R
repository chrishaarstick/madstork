
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

test_that("trade buy function work as expected", {
  p1 <- portfolio("new_port", cash=0) %>%
    make_deposit(Sys.Date(), amount = 2000) %>%
    make_buy(Sys.Date(), symbol = "SPY", quantity = 10, price = 100)

  expect_equal(p1$cash, 2000-10*100-10*.05)
  expect_equal(nrow(p1$activity), 3)
  expect_equal(nrow(p1$trades), 1)
  expect_equal(unique(p1$activity$date_added), Sys.Date())
  expect_equal(p1$trade$date_added, Sys.Date())
  expect_error(p1 %>% make_buy(Sys.Date(), symbol = "SPY", quantity = 10, price = 100))
})



test_that("trade sell function work as expected", {
  p1 <- portfolio("new_port", cash=0) %>%
    make_deposit(amount = 2000) %>%
    make_buy(Sys.Date()-1, symbol = "SPY", quantity = 10, price = 100) %>%
    make_sell(id = 1, quantity = 5, price = 105)

  expect_equal(p1$cash, 2000-10*100-10*.05+5*105-5*.05)
  expect_equal(nrow(p1$activity), 5)
  expect_equal(nrow(p1$trades), 2)
  expect_equal(unique(p1$activity$date_added), Sys.Date())
  expect_equal(unique(p1$trade$date_added), Sys.Date())
  expect_equal(p1$tax_liability, 5 * (105-100) * .3)
  expect_equal(nrow(p1$gains), 1)
  expect_equal(p1$gains$gain, 5 * (105-100))
})
