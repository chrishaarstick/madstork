
# Constraints Unit Test ---------------------------------------------------

library(madstork)
library(tidyverse)
library(lubridate)
library(testthat)
library(checkmate)

context("Constriaints Class")


# Create Estimates Class
yrs <- 5
syms <- c("SPY", "QQQ", "TLT", "GLD")
e1 <- estimates(symbols = syms,
                start_date = Sys.Date() - years(yrs),
                end_date = Sys.Date(),
                grain = "year",
                periods = 1) %>%
  add_sample_mu() %>%
  add_sample_sigma()


# Create Portfolio
p1 <- portfolio("new_port", cash=0) %>%
  make_deposit(amount = 10100) %>%
  make_buy(symbol = "SPY", quantity = 20, price = 100) %>%
  make_buy(symbol = "QQQ", quantity = 20, price = 100) %>%
  make_buy(symbol = "TLT", quantity = 30, price = 100) %>%
  make_buy(symbol = "GLD", quantity = 20, price = 100) %>%
  update_market_value()


# Create Constraints
c1 <- constraints(symbols = e1$symbols) %>%
  add_symbol_constraint(symbol = "SPY", max = .2) %>%
  add_symbol_constraint(symbol = "QQQ", max = .3) %>%
  add_symbol_constraint(symbol = "TLT", min = .2, max = .5) %>%
  add_symbol_constraint(symbol = "GLD", max = .2)

c2 <- constraints(symbols = e1$symbols) %>%
  add_symbol_constraint(min = .1, max = .5) %>%
  add_group_constraint(symbols = c("SPY", "QQQ"), max = .5) %>%
  add_cardinality_constraint(min = 2, max = 4) %>%
  add_min_return(min = .08)

# Check Constaints
chk1 <- check_constraints(c1, p1, e1)
chk2 <- check_constraints(c2, p1, e1)
