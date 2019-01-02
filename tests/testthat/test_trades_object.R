
# Trades Object Tests -----------------------------------------------------


library(madstork)
library(tidyverse)
library(lubridate)
library(checkmate)

context("Trades Class")

b1 <- buy(date = Sys.Date(), symbol = "SPY", quantity = 10, price = 100)

test_that("trade helper functions creates object of class trade",{
  expect_equal(class(b1), "trade")
})

test_that("trade helper functions internals work correctly", {
  expect_equal(b1$amount, 1000)
  expect_equal(b1$type, "buy")
})

test_that("trade buy function works as expected", {
  p1 <- portfolio("new_port") %>%
    make_deposit(Sys.Date(), amount = 2000) %>%
    make_buy(Sys.Date(), symbol = "SPY", quantity = 10, price = 100)

  expect_equal(p1$cash, 2000-10*100-10*.05)
  expect_equal(nrow(p1$activity), 3)
  expect_equal(nrow(p1$trades), 1)
  expect_equal(unique(p1$activity$date_added), Sys.Date())
  expect_equal(p1$trade$date_added, Sys.Date())
  expect_error(p1 %>% make_buy(Sys.Date(), symbol = "SPY", quantity = 10, price = 100))
})



test_that("trade sell function works as expected", {
  p1 <- portfolio("new_port") %>%
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




test_that("trade transfer out function works as expected", {
  p1 <- portfolio("new_port") %>%
    make_deposit(amount = 2000) %>%
    make_buy(Sys.Date()-1, symbol = "SPY", quantity = 10, price = 100) %>%
    transfer_out(id = 1)

  expect_equal(nrow(p1$trades), 2)
  expect_equal(nrow(p1$holdings), 0)
  expect_equal(p1$cash, 2000-10*100-10*.05)
})




test_that("trade transfer in function works as expected", {
  p1 <- portfolio("new_port") %>%
    make_deposit(amount = 2000) %>%
    make_buy(Sys.Date()-1, symbol = "SPY", quantity = 10, price = 100) %>%
    transfer_in(Sys.Date()-1, symbol = "TLT", quantity = 10, price = 200)

  expect_equal(nrow(p1$trades), 2)
  expect_equal(nrow(p1$holdings), 2)
  expect_equal(p1$cash, 2000-10*100-10*.05)
})


test_that("gains function as expected from sell", {

  p1 <- portfolio("new_port") %>%
    make_deposit(amount = 2000) %>%
    make_buy(Sys.Date()-years(1), symbol = "SPY", quantity = 10, price = 100)

  p1.1 <- make_sell(p1, 1, date = Sys.Date(), quantity = 10, price = 105)

  expect_data_frame(p1.1$gains, nrows=1)
  expect_equal(p1.1$gains$gain, 10 * (105-100))
  expect_equal(p1.1$gains$id, 1)
})
