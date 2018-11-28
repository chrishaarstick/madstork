
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


test_that("dividend update function work as expected", {


  p1 <- portfolio("new_port", cash=0) %>%
    make_deposit(as.Date("2018-01-01"), amount = 1000)

  p1$holdings <- bind_rows(
    tibble(id = 1,
           date_added = as.Date("2018-01-01"),
           transaction_date = as.Date("2018-01-01"),
           symbol = "SPY",
           quantity = 10,
           price = 100,
           desc = ""),
    tibble(id = 2,
           date_added = as.Date("2018-07-01"),
           transaction_date = as.Date("2018-07-01"),
           symbol = "SPY",
           quantity = 20,
           price = 100,
           desc = "")
  )

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
})
