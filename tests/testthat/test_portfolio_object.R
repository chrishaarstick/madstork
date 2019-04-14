
# Portfolio Class Tests ---------------------------------------------------

library(madstork)
library(tidyverse)
library(lubridate)
library(testthat)
library(checkmate)

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

  p1 <- portfolio("new_port") %>%
    make_deposit(amount = 100)

  expect_equal(get_cash(p1), 100)
})


test_that("portfolio setter functions work as expected", {

  p1 <- portfolio("new_port")
  p2 <- set_interest_rate(p1, 0.01)

  expect_equal(p1$interest_rate, 0)
  expect_equal(p2$interest_rate, 0.01)
})


test_that("market value update function work as expected", {

  p1 <- portfolio("new_port") %>%
    make_deposit(amount = 1000)
  p1.1 <- update_market_value(p1)

  expect_equal(p1.1$market_value$net_value, 1000)
})


test_that("dividend update function work as expected", {


  p1 <- portfolio("new_port") %>%
    make_deposit(as.Date("2018-01-01"), amount = 5000) %>%
    make_buy(date = as.Date("2018-01-01"), symbol = "SPY", quantity = 10, price = 100) %>%
    make_buy(date = as.Date("2018-07-01"), symbol = "SPY", quantity = 20, price = 100)


  p1$income <- tibble(
    date_added = as.Date("2018-03-31"),
    transaction_date = as.Date("2018-03-31"),
    type = "dividend",
    symbol = "SPY",
    quantity = 10,
    payment = 2,
    amount = 20,
    desc = "",
    id = 1
  )

  p2 <- update_dividend_income(p1)
  spy_divs <- get_dividends("SPY", start_date = as.Date("2018-01-01"))

  expect_equal(filter(p2$income, type == "dividend") %>%
                 nrow(),
               spy_divs %>%
                 inner_join(select(p1$holdings, id, transaction_date, symbol),
                            by = "symbol") %>%
                 filter(date >= transaction_date) %>%
                 nrow())

  spy_divs2 <- get_dividends("SPY", start_date = max(spy_divs$date) + days(1))
  expect_equal(nrow(spy_divs2), 1)
  expect_equal(spy_divs2$dividend, 0)
})



# Activity Constructor ----------------------------------------------------


activity_list <- list(
  deposit(date = as.Date("2018-12-25"), amount = 5000),
  buy(date = as.Date("2018-12-27"), symbol = "SPY", quantity = 10, price = 100),
  buy(date = as.Date("2018-12-28"), symbol = "TLT", quantity = 10, price = 100),
  sell(date = as.Date("2018-12-28"), symbol = "SPY", quantity = 5, price = 105)
)

p1 <- portfolio("test", activity = activity_list)


test_that("portfolio constructor with activity works as expected", {

  expect_data_frame(p1$holdings, nrows = 2)
  expect_equal(p1$holdings %>%
                 filter(symbol == "SPY") %>%
                 pull(quantity),
               5)
  expect_data_frame(p1$gains, nrows = 1)
  expect_data_frame(p1$holdings_market_value, nrows = 2)
  expect_gt(nrow(p1$market_value), 0)
})


test_that("market value update on existing portfolio works as expected", {

  p1.1 <- update_market_value(p1)
  expect_gt(nrow(p1.1$market_value), nrow(p1$market_value))
})
