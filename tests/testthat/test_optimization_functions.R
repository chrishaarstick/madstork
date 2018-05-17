
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
tp <- trade_pairs(po1)
p1_stats <- get_estimated_port_stats(pobj = p1, eobj = e1) %>%
  dplyr::filter(type == "portfolio")

# Iteration
cc1 <- check_constraints(po1$constraints, po1$portfolio, po1$estimates)
n <- 2
amount <- 1000
lot_size <- 1

tp_smpl <- tp %>%
  filter(active) %>%
  sample_n(n, weight = delta)



port_canidates <- tp_smpl %>%
  split(.$id) %>%
  map(~execute_trade_pair(.$buy, .$sell, po1$portfolio, po1$estimates, amount))



evaluate_constraints <- function(new_portfolio, portfolio, constraints, estimates){

  new_cc <- check_constraints(constraints, new_portfolio, estimates)

  # Constraints Evaluation
  if(! all(new_cc$check))  {

    # compare new constraints to intial state
    cc_eval <- check_constraints(constraints, portfolio, estimates) %>%
      select(id, old_value = value) %>%
      dplyr::inner_join(new_cc, by = "id") %>%
      dplyr::mutate(
        improve = check,
        improve = ifelse(!improve &
                           old_value < min & value > old_value, TRUE, improve),
        improve = ifelse(!improve &
                           old_value > max & value < old_value, TRUE, improve)
      )

    if(! all(cc_eval$improve)) {
      constraints_passed <- FALSE
      message("Constraints conditions not met or improved - trade not approved")
    } else {
      constraints_passed <- TRUE
    }

  } else {
    constraints_passed <- TRUE
  }

  constraints_passed
}


port_optimal <- port_canidates %>%
  keep(.,
       port_canidates %>%
         map_lgl(~evaluate_constraints(., po1$portfolio, po1$constraints, po1$estimates))

  ) %>%
  map(~get_estimated_port_stats(., po1$estimates, port_only = TRUE)) %>%
  map_df(po1$target) %>%
  gather(key = "id") %>%
  filter(value == max(value)) %>%
  .$id %>%
  pluck(port_canidates, .)

tp_stats <- data.frame()


# To do - create list of candidate portfolios by evaluating each trade_pair &
# evaluating constraints. Create evaluate constraints wrapper function for logic
# below. Split into create new canidate portfolio by applying trades and then
# evaluating constraints. Once completed, select optimal portfolio and store in
# optimal port slot. Should update trade pairs data.frame



for(i in 1:nrow(tp_smpl)) {

  buy <- get_buy_trades(e1, tp_smpl$buy[i], amount, lot_size = 1)
  p2 <- make_buy(p1, symbol=as.character(buy$symbol), quantity = buy$quantity, price = buy$price)

  if(tp_smpl$sell[i] != "CASH") {
    sell <- get_sell_trades(p1, tp_smpl$sell[i], amount, lot_size = 1)
    p2 <- make_sell(p2, id = sell$id, quantity = sell$quantity, price = sell$price)
  }

  p2 <- update_market_value(p2, refresh = FALSE)

  cc_tp1 <- check_constraints(po1$constraints, p2, po1$estimates)

  # Constraints Evaluation
  if(! all(cc_tp1$check))  {

    # compare new constraints to intial state
    cc_eval <- cc1 %>%
      select(id, old_value = value) %>%
      dplyr::inner_join(cc_tp1, by = "id") %>%
      dplyr::mutate(
        improve = check,
        improve = ifelse(!improve &
                           old_value < min & value > old_value, TRUE, improve),
        improve = ifelse(!improve &
                           old_value > max & value < old_value, TRUE, improve)
      )

    if(! all(cc_eval$improve)) {
      constraints_passed <- FALSE
      message("Constraints conditions not met or improved - trade not approved")
    } else {
      constraints_passed <- TRUE
    }

  } else {
    constraints_passed <- TRUE
  }

  if(constraints_passed) {

    # Check if target improved
    p2_stats <- get_estimated_port_stats(pobj = p2, eobj = e1) %>%
      dplyr::filter(type == "portfolio") %>%
      dplyr::mutate(id = tp_smpl$id[i])
    tp_stats <- rbind(tp_stats, p2_stats)
  }
}


select_optimal_portfolio()


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


