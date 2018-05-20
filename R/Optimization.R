
# Potfolio Optimization Class ---------------------------------------------


portfolio_optimization <- function(portfolio,
                                   estimates,
                                   constraints,
                                   target,
                                   desc = "",
                                   version = 1.0) {
  checkmate::assert_class(portfolio, "portfolio")
  checkmate::assert_class(estimates, "estimates")
  checkmate::assert_class(constraints, "constraints")
  checkmate::assert_choice(target, c("return", "risk", "sharpe", "income"))
  checkmate::assert_character(desc)
  checkmate::assert_number(version)

  criteria <- ifelse(target %in% c("risk"), "minimize", "maximize")
  tp <- trade_pairs(portfolio, estimates, target)
  port_stats <- get_estimated_port_stats(portfolio, estimates, port_only = TRUE) %>%
    dplyr::mutate(iter = 0)

  structure(
    list(
      portfolios = list(portfolio),
      optimal_portfolio = portfolio,
      estimates = estimates,
      constraints = constraints,
      target = target,
      criteria = criteria,
      trade_pairs = tp,
      portfolio_stats = port_stats,
      created_on = Sys.time(),
      user = as.character(Sys.info()["user"]),
      desc = desc,
      version = version
    ),
    class = c("portfolio_optimization")
  )
}



# NMTO --------------------------------------------------------------------


get_sell_trades <- function(pobj,
                            symbols = NULL,
                            amount,
                            lot_size = 1,
                            partial = TRUE) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_character(symbols, null.ok = TRUE)
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)
  checkmate::assert_flag(partial)

  holdings <- get_holdings(pobj) %>%
    dplyr::select(id, symbol, quantity) %>%
    dplyr::inner_join(
      get_holdings_market_value(pobj) %>%
        dplyr::select(symbol, quantity, price, market_value),
      by = c("symbol", "quantity"))

  if(! is.null(symbols)) {
    checkmate::assert_subset(symbols, holdings$symbol)
    holdings <- dplyr::filter(holdings, symbol %in% symbols)
  }

  sells <- data.frame()
  for (i in 1:nrow(holdings)) {
    h1 <- holdings[i,]
    if (amount > h1$market_value) {
      if (partial) {
        s1 <- as.data.frame(sell(
          date = Sys.Date(),
          symbol = h1$symbol,
          quantity = h1$quantity,
          price = h1$price
        ))
        s1$id <- h1$id
        sells <- rbind(sells, s1)
      }
    } else{
      quantity <- amount %/% (h1$price * lot_size) * lot_size
      s1 <- as.data.frame(sell(
        date = Sys.Date(),
        symbol = h1$symbol,
        quantity = quantity,
        price = h1$price
      ))
      s1$id <- h1$id
      sells <- rbind(sells, s1)
    }
  }

  sells
}


get_buy_trades <- function(obj, symbols, amount, lot_size) {
  UseMethod("get_buy_trades")
}


get_buy_trades.estimates <- function(obj,
                                     symbols = NULL,
                                     amount,
                                     lot_size = 1) {
  checkmate::assert_character(symbols, null.ok = TRUE)
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)

  if( is.null(symbols)) {
    symbols <- obj$symbols
  } else {
    checkmate::assert_subset(symbols, obj$symbols)
  }

  buys <- data.frame()
  for(sym in symbols) {
    price <- obj$prices %>%
      dplyr::filter(symbol == sym) %>%
      dplyr::filter(date == max(date)) %>%
      .$price
    quantity <- amount %/% (price * lot_size) * lot_size
    b1 <- buy(date = Sys.Date(), symbol = sym, quantity = quantity, price = price)
    buys <- rbind(buys, as.data.frame(b1))
  }
  buys
}


get_buy_trades.portfolio <- function(obj,
                                     symbols = NULL,
                                     amount,
                                     lot_size = 1) {
  checkmate::assert_character(symbols, null.ok = TRUE)
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)

  holdings <- obj$holdings_market_value
  if( is.null(symbols)) {
    symbols <- holdings$symbol
  } else {
    checkmate::assert_subset(symbols, holdings$symbol)
  }

  buys <- data.frame()
  for(sym in symbols) {
    price <- holdings %>%
      dplyr::filter(symbol == sym) %>%
      .$price
    quantity <- amount %/% (price * lot_size) * lot_size
    b1 <- buy(date = Sys.Date(), symbol = sym, quantity = quantity, price = price)
    buys <- rbind(buys, as.data.frame(b1))
  }
  buys
}


get_buy_trades.character <- function(obj,
                                     symbols = NULL,
                                     amount,
                                     lot_size = 1) {
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)
  buys <- data.frame()
  for(sym in obj) {
    price <- get_current_prices(sym)[["price"]]
    quantity <- amount %/% (price * lot_size) * lot_size
    b1 <- buy(date = Sys.Date(), symbol = sym, quantity = quantity, price = price)
    buys <- rbind(buys, as.data.frame(b1))
  }
  buys
}




trade_pairs <- function(portfolio, estimates, target){
  checkmate::assert_class(portfolio, "portfolio")
  checkmate::assert_class(estimates, "estimates")
  checkmate::assert_choice(target, c("mu", "sd", "sharpe", "yield"))

  est_stats <- get_estimates_stats(estimates) %>%
    dplyr::select_at(c("symbol", target))

  expand.grid(buy = c("CASH", estimates$symbols),
              sell = c("CASH", as.character(portfolio$holdings$symbol))) %>%
    dplyr::filter(buy != sell) %>%
    dplyr::mutate(id = row_number()) %>%
    dplyr::mutate_at(c("buy", "sell"), as.character) %>%
    dplyr::select(id, buy, sell) %>%
    dplyr::left_join(est_stats, by = c("buy" = "symbol")) %>%
    dplyr::left_join(est_stats, by = c("sell" = "symbol")) %>%
    setNames(c("id", "buy", "sell", "buy_target", "sell_target")) %>%
    tidyr::replace_na(list(buy_target = 0, sell_target = 0)) %>%
    dplyr::mutate(delta = buy_target - sell_target,
                  selected = 0,
                  trades   = 0,
                  active = ifelse(delta > 0, TRUE, FALSE)) %>%
    dplyr::arrange(-delta)
}


execute_trade_pair <- function(buy, sell, portfolio, estimates, amount, lot_size = 1, refresh = FALSE) {

  if(sell != "CASH") {
    sell <- get_sell_trades(portfolio, as.character(sell), amount, lot_size)
    p2 <- make_sell(portfolio, id = sell$id, quantity = sell$quantity, price = sell$price)
  }

  buy <- get_buy_trades(estimates, as.character(buy), amount, lot_size)
  p2 <- make_buy(p2, symbol = as.character(buy$symbol), quantity = buy$quantity, price = buy$price)

  update_market_value(p2, refresh)
}


evaluate_constraints <- function(portfolio1, portfolio2, constraints, estimates){

  cc1 <- check_constraints(constraints, portfolio1, estimates)

  # Constraints Evaluation
  if(! all(cc1$check))  {

    # compare new constraints to intial state
    cc_eval <- check_constraints(constraints, portfolio2, estimates) %>%
      select(id, old_value = value) %>%
      dplyr::inner_join(cc1, by = "id") %>%
      dplyr::mutate(
        improve = check,
        improve = ifelse(!improve &
                           old_value < min & value > old_value, TRUE, improve),
        improve = ifelse(!improve &
                           old_value > max & value < old_value, TRUE, improve)
      )

    if(! all(cc_eval$improve)) {
      constraints_passed <- FALSE
      #message("Constraints conditions not met or improved - trade not approved")
    } else {
      constraints_passed <- TRUE
    }

  } else {
    constraints_passed <- TRUE
  }

  constraints_passed
}


select_optimal_portfolio <- function(portfolios, estimates, target, criteria) {
  checkmate::assert_list(portfolios)
  checkmate::assert_class(estimates, "estimates")

  purrr::map_df(
    portfolios,
    get_estimated_port_stats,
    eobj = estimates,
    port_only = TRUE,
    .id = "id"
  ) %>%
    dplyr::top_n(ifelse(criteria == "minimize", -1, 1), !!rlang::sym(target)) %>%
    .$id %>%
    portfolios[[.]]
}




optimize <- function(obj,
                     npairs,
                     amount,
                     lot_size,
                     max_iter = 10,
                     max_runtime = 300,
                     improve_lag = 2,
                     min_improve = .001,
                     plot_iter = TRUE ) {
  checkmate::assert_class(obj, "portfolio_optimization")
  checkmate::assert_number(npairs,
                           lower = 1,
                           upper = nrow(obj$trade_pairs))
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 1)
  checkmate::assert_number(max_iter, lower = 1)
  checkmate::assert_number(improve_lag, lower = 1, upper = max_iter)
  checkmate::assert_number(max_runtime, lower = 10)
  checkmate::assert_number(min_improve, lower = 0)
  checkmate::assert_logical(plot_iter)

  i <- 0
  continue <- TRUE
  t1 <- Sys.time()
  while (continue) {
    i <- i+1

    # Trade pair samples
    tp_actives <- obj$trade_pairs %>%
      dplyr::filter(active)
    tp_nactives <- nrow(tp_actives)
    if (tp_nactives == 0) {
      message("No further active trade pairs. Stopping optimization")
      break
    } else {
      tp_smpl <- tp_actives %>%
        dplyr::sample_n(min(npairs, tp_nactives), weight = delta)
    }

    # Create Canidate Portfolios
    port_canidates <- tp_smpl %>%
      split(.$id) %>%
      purrr::map(
        ~ execute_trade_pair(
          .$buy,
          .$sell,
          obj$optimal_portfolio,
          obj$estimates,
          amount
        )
      )

    # Evaluate Canidates
    port_evals <- port_canidates %>%
      purrr::map_lgl(
        ~ evaluate_constraints(., obj$optimal_portfolio, obj$constraints, obj$estimates)
      )

    # Evaluate Canidates and Select Optimal
    port_eval_list <- c(purrr::keep(port_canidates, port_evals),
                        list(`0` = obj$optimal_portfolio))
    opt_port_id <- port_eval_list %>%
      purrr::map_df(
        .,
        get_estimated_port_stats,
        eobj = obj$estimates,
        port_only = TRUE,
        .id = "id"
      ) %>%
      dplyr::top_n(ifelse(obj$criteria == "minimize",-1, 1),
                   !!rlang::sym(obj$target)) %>%
      .$id %>%
      head(1)
    opt_port <- port_eval_list[[opt_port_id]]

    # Update Trade Pairs
    obj$trade_pairs <-
      data.frame(id = as.numeric(names(port_evals)),
                 active = port_evals) %>%
      dplyr::mutate(n = 1) %>%
      dplyr::right_join(obj$trade_pairs, by = c("id")) %>%
      tidyr::replace_na(list(n = 0)) %>%
      dplyr::mutate(selected = selected + n) %>%
      dplyr::mutate(active = ifelse(is.na(active.x), active.y, active.x)) %>%
      dplyr::mutate(trades = ifelse(id == as.numeric(opt_port_id), trades + 1, trades)) %>%
      dplyr::select(-n,-active.x,-active.y)

    # Update Obj
    obj$optimal_portfolio <- opt_port
    obj$portfolios <- c(obj$portfolios, list(opt_port))
    obj$portfolio_stats <- obj$portfolio_stats %>% rbind(
      get_estimated_port_stats(opt_port, obj$estimates, port_only = TRUE) %>%
        dplyr::mutate(iter = i)
    )

    if(plot_iter) {
      print(ggplot(obj$portfolio_stats, aes_string(x='iter', y=obj$target)) +
        geom_line(size=1.05, color=madstork_pal()(1)) +
        theme_minimal() +
        labs(title = "Madstork Next Best Trade Optimization",
             subtitle = paste("Iteration", i)))
    }

    # Determine Stopping Conditions
    runtime <- as.numeric(difftime(Sys.time(), t1, units = "sec")) < max_runtime

    if(i >= improve_lag) {
      target_improve <- obj$portfolio_stats %>%
        dplyr::mutate_at(obj$target,
                         dplyr::funs(target_improve = ./dplyr::lag(., n=improve_lag))) %>%
        dplyr::filter(iter == max(iter))

      improve <- ifelse(obj$criteria == "minimize",
                        (1 - target_improve$target_improve) >= min_improve,
                        (target_improve$target_improve - 1) >= min_improve )
    }else {
      improve <- TRUE
    }

    iters <- i < max_iter

    continue <- all(c(runtime, improve, iters))
  }

  obj
}
