
# Optimization Unit Tests -------------------------------------------------

library(madstork)
library(tidyverse)
library(lubridate)
library(testthat)
library(checkmate)

context("Optimization Functionality")


# Create Estimates Class
yrs <- 10
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
  add_cash_constraint(max = .02) %>%
  add_min_return(min = .08)


# Create Optimization
po1 <- portfolio_optimization(p1, e1, c1, target = "sharpe")

# Optimize
po1_opt <- madstork::optimize(po1, npairs = 4,
                              amount = 1000, lot_size = 1,
                              max_iter = 15, max_runtime = 120,
                              improve_lag = 10, min_improve = .001)


# Create Constraints
c2 <- constraints(symbols = e1$symbols) %>%
  add_symbol_constraint(min = .0, max = .5)

po2 <- portfolio_optimization(p1, e1, c2, target = "return")

# Optimize
po2_opt <- madstork::optimize(po2, npairs = 4, amount = 1000, lot_size = 1,
                              max_iter = 25, max_runtime = 120,
                              improve_lag = 5, min_improve = .001)



# Single Holding - Testing Estimate Symbol setdiff ------------------------

# Create Portfolio
p3 <- portfolio("new_port", cash=0) %>%
  make_deposit(amount = 15000) %>%
  make_buy(symbol = "TLT", quantity = 30, price = 100) %>%
  update_market_value()


# Create Constraints
c3 <- constraints(symbols = e1$symbols) %>%
  add_cash_constraint(min = 0, max = .05) %>%
  add_min_return(min = .12) %>%
  add_min_yield(min = .015) %>%
  add_symbol_constraint(min = .0, max = .5)


# Create Optimization
po3 <- portfolio_optimization(p3, e1, c3, target = "sharpe")

# Optimize
po3_opt <- madstork::optimize(po3, npairs = 4,
                              amount = 1000, lot_size = 1,
                              max_iter = 15, max_runtime = 180,
                              improve_lag = 10, min_improve = .001)


purrr::map_df(po3_opt$portfolios,
              ~get_symbol_estimates_share(pobj = ., eobj = po3_opt$estimates), .id = "iter") %>%
  ggplot(., aes(x=as.numeric(iter), y=portfolio_share, color = symbol, group=symbol)) +
  geom_line() +
  scale_color_madstork() +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()


purrr::map_df(obj$portfolios,
              ~check_constraints(c1, ., e1), .id = "iter") %>%
  ggplot(., aes(x=as.numeric(iter), y=value, group=args)) +
  geom_ribbon(aes(ymin = min, max = ifelse(max == Inf, value, max)), color="grey75", alpha=.25) +
  geom_line(color = "blue") +
  geom_point(size = 2, shape=1, color ="blue") +
  facet_wrap(~type+args, scales = "free") +
  scale_color_madstork() +
  theme_minimal()

