
# Portfolio Past Unit Tests -----------------------------------------------

# Packages
library(madstork)
library(tidyverse)
library(lubridate)
library(testthat)
library(checkmate)

context("Portfolio Past Functionality")

p1 <- portfolio(name = "test") %>%
  make_deposit(date = as.Date("2018-01-01"), amount = 10000) %>%
  make_buy(date = as.Date("2018-01-08"), symbol = "SPY", quantity = 10, price = 240) %>%
  make_sell(date = as.Date("2018-07-09"), id = 1, quantity = 5, price = 260, trans_cost = 0)



test_that("get past holdings works as expected", {

  past_holdings <- get_past_holdings(p1)
  expect_data_frame(past_holdings, ncols = 3)
  expect_equal(min(past_holdings$date), as.Date("2018-01-08"))
  expect_equal(last(past_holdings$quantity), 5)
})


test_that("get past holdings market value works as expected", {

  past_holdings_value <- get_past_holdings_market_value(p1)
  expect_data_frame(past_holdings_value , ncols = 5)
  expect_equal(min(past_holdings_value$date), as.Date("2018-01-08"))
  expect_equal(last(past_holdings_value$quantity), 5)
})



test_that("adding past dividends works as expected", {

  p2 <- past_dividend_income(p1)
  expect_data_frame(p2$income)
  expect_equal(p2$income %>%
                 filter(transaction_date < "2018-07-09") %>%
                 pull(quantity) %>%
                 max(),
               10)
  expect_equal(p2$income %>%
                 filter(transaction_date >= "2018-07-09") %>%
                 pull(quantity) %>%
                 max(),
               5)
})


test_that("get past cash works as expected", {

  past_cash <- get_past_cash(p1)
  expect_data_frame(past_cash, ncols = 2)
  expect_equal(min(past_cash$date), as.Date("2018-01-01"))
})


test_that("get past tax liability works as expected", {

  past_tax <- get_past_tax_liability(p1)
  expect_data_frame(past_tax, ncols = 2)
  expect_equal(min(past_tax$date), as.Date("2018-01-08"))
  expect_equal(
    past_tax  %>%
      filter(year(date) == 2018) %>%
      filter(date == max(date)) %>%
      pull(tax_liability),
    (10-5)*(260-240) * .30
  )
})


test_that("past market value works as expected", {

  p2 <- past_market_value(p1)
  expect_equal(min(p2$market_value$date), as.Date("2018-01-01"))
})

