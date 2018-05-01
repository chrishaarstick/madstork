
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
  add_sample_sigma()


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

#
# # Calculate Impact
# impact <- data.frame()
#
# # Buy Trades
# trades <- get_buy_trades(p1, amount = 1000)
# for(i in 1:nrow(trades)) {
#   t1 <- trades[i,]
#   stats <- make_buy(p1, date = t1$date, symbol = as.character(t1$symbol),
#                     quantity = t1$quantity, price = t1$price) %>%
#     update_market_value(refresh = FALSE) %>%
#     get_estimated_port_stats(e1) %>%
#     filter(type == "portfolio")
#   stats <- cbind(t1, stats)[, c("type", "symbol", "quantity", "amount", po1$target)]
#   impact <- rbind(impact, stats)
# }
#
#
# # Sell Trades
# trades <- get_sell_trades(p1, amount = 1000)
# for(i in 1:nrow(trades)) {
#   t1 <- trades[i,]
#   stats <- make_sell(p1, id = t1$id, date = t1$date,
#                      quantity = t1$quantity, price = t1$price) %>%
#     update_market_value(refresh = FALSE) %>%
#     get_estimated_port_stats(e1) %>%
#     filter(type == "portfolio")
#   stats <- cbind(t1, stats)[, c("type", "symbol", "quantity", "amount", po1$target)]
#   impact <- rbind(impact, stats)
# }
#
# if(po1$criteria == "minimize") {
#   arrange_at(impact, po1$target)
# }else {
#   arrange_at(impact, po1$target, .funs = desc)
# }
# impact
#
#


