
# Portfolio Past Unit Tests -----------------------------------------------

# Packages
library(madstork)
library(tidyverse)
library(lubridate)
library(testthat)
library(checkmate)

context("Portfolio Past Functionality")

# p1 <- portfolio(name = "test") %>%
#   make_deposit(date = as.Date("2018-01-01"), amount = 10000) %>%
#   make_buy(date = as.Date("2018-01-08"), symbol = "SPY", quantity = 10, price = 240) %>%
#   make_sell(date = as.Date("2018-07-09"), id = 1, quantity = 5, price = 260, trans_cost = 0) %>%
#   update_market_value()

activity <- list(
  deposit(date = as.Date("2018-01-01"), amount = 10000),
  buy(date = as.Date("2018-01-08"), symbol = "SPY", quantity = 10, price = 240),
  sell(date = as.Date("2018-07-09"), symbol = "SPY", quantity = 5, price = 260)
)

test_that("initialize portfolio works as expected", {

  # p2 <- portfolio(name = "init", activity = activity)
  # expect_class(p2, "portfolio")
  # expect_equal(last(p2$market_value$investments_value), p1$market_value$investments_value)
  # expect_gte(nrow(p2$income), 0)
})

