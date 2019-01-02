
# Income Object Tests -----------------------------------------------------

library(madstork)
library(tidyverse)
library(testthat)

context("Income Class")

d1 <- dividend(date = Sys.Date(), symbol = "SPY", quantity = 10, dividend = 2, amount = 20)
i1 <- interest(date = Sys.Date(), principal = 1000, amount = 10)

test_that("income helper functions creates object of class income",{
  expect_equal(class(d1), "income")
  expect_equal(class(i1), "income")
})

test_that("income helper functions internals work correctly", {
  expect_equal(d1$type, "dividend")
  expect_equal(i1$type, "interest")
  expect_equal(i1$symbol, "BANK")
})

test_that("income execution functions work as expected", {
  p1 <- portfolio("new_port") %>%
    make_deposit(amount = 2000) %>%
    make_buy(symbol = "SPY", quantity = 10, price = 100) %>%
    recieve_dividend(symbol = "SPY", amount = 20) %>%
    recieve_interest(amount = 2)

  expect_equal(p1$cash, 2000-10*100-10*.05+20+2)
  expect_equal(nrow(p1$activity), 5)
  expect_equal(nrow(p1$trades), 1)
  expect_equal(nrow(p1$income), 2)
  expect_equal(unique(get_income(p1)$date_added), Sys.Date())
  expect_equal(get_income(p1) %>%
                 summarise_at(vars(amount), funs(sum)) %>%
                 .[[1]],
               20 + 2)
  expect_equal(get_income(p1) %>% filter(id == 1) %>% .$payment, 20/10)
  expect_equal(get_income(p1) %>% filter(id == 2) %>% .$quantity, 2000-10*100-10*.05+20)
})

