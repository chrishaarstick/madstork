
# Optimization Unit Tests -------------------------------------------------

library(madstork)
library(tidyverse)
library(lubridate)
library(testthat)
library(checkmate)

context("Optimization Functionality")


# Create Estimates Class
yrs <- 5
syms <- c("SPY", "QQQ", "TLT", "GLD")
e1 <- estimates(symbols = syms,
                start_date = Sys.Date() - years(yrs),
                end_date = Sys.Date(),
                grain = "year",
                periods = 1) %>%
  add_sample_mu() %>%
  add_sample_sigma() %>%
  add_dividends()


# Create Portfolio
p1 <- portfolio("new_port", cash=0) %>%
  make_deposit(amount = 15000) %>%
  make_buy(symbol = "SPY", quantity = 20, price = 100) %>%
  make_buy(symbol = "QQQ", quantity = 20, price = 100) %>%
  make_buy(symbol = "TLT", quantity = 30, price = 100) %>%
  make_buy(symbol = "GLD", quantity = 20, price = 100) %>%
  update_market_value()


# Create Constraints
c1 <- constraints(symbols = e1$symbols) %>%
  add_symbol_constraint(min = .1, max = .5) %>%
  add_group_constraint(symbols = c("SPY", "QQQ"), max = .5) %>%
  add_cardinality_constraint(min = 2, max = 4) %>%
  add_min_return(min = .08)


# Create Optimization
po1 <- portfolio_optimization(p1, e1, c1, target = "sharpe")

# Optimize
po1_opt <- madstork::optimize(po1, npairs = 4, amount = 1000, lot_size = 1,
                          max_iter = 10, max_runtime = 120, improve_lag = 1, min_improve = .001)


# Create Constraints
c2 <- constraints(symbols = e1$symbols) %>%
  add_symbol_constraint(min = .0, max = .5)

po2 <- portfolio_optimization(p1, e1, c2, target = "sharpe")

# Optimize
po2_opt <- madstork::optimize(po2, npairs = 4, amount = 1000, lot_size = 1,
                              max_iter = 25, max_runtime = 120,
                              improve_lag = 5, min_improve = .001)



