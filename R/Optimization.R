
# Potfolio Optimization Class ---------------------------------------------


portfolio_optimization <- function(portfolio,
                                   estimates,
                                   constraints,
                                   prices = NULL,
                                   target,
                                   desc = "",
                                   version = 1.0) {
  checkmate::assert_class(portfolio, "portfolio")
  checkmate::assert_class(estimates, "estimates")
  checkmate::assert_class(constraints, "constraints")
  checkmate::assert_data_frame(prices, null.ok = TRUE)
  checkmate::assert_choice(target, c("mu", "sd", "yield", "return", "risk", "sharpe", "income"))
  checkmate::assert_character(desc)
  checkmate::assert_number(version)

  # Check symbols
  symbols <- estimates$symbols
  checkmate::assert_subset(unique(portfolio$holdings$symbol), symbols)

  if(is.null(prices)) {
    prices <- get_current_prices(symbols = symbols)
  }
  checkmate::assert_subset(c("symbol", "price", "dividend"), colnames(prices))

  criteria <- ifelse(target %in% c("sd"), "minimize", "maximize")
  tp <- trade_pairs(portfolio, estimates, target)
  port_values <- get_estimated_port_values(portfolio, estimates) %>%
    dplyr::mutate(iter = 0)

  structure(
    list(
      portfolios = list(portfolio),
      optimal_portfolio = portfolio,
      estimates = estimates,
      constraints = constraints,
      prices = prices,
      target = target,
      criteria = criteria,
      trade_pairs = tp,
      portfolio_values = port_values,
      created_on = Sys.time(),
      user = as.character(Sys.info()["user"]),
      desc = desc,
      version = version
    ),
    class = c("portfolio_optimization")
  )
}



# NBTO --------------------------------------------------------------------


get_sell_trades <- function(pobj,
                            symbol,
                            amount,
                            lot_size = 1,
                            partial = TRUE) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)
  checkmate::assert_flag(partial)
  sym <- symbol

  holdings <- get_holdings_market_value(pobj) %>%
    dplyr::filter(symbol == sym) %>%
    dplyr::arrange(-unrealized_gain) %>%
    dplyr::select(id, symbol, quantity, price, market_value)

  checkmate::assert_subset(symbol, holdings$symbol)

  sells <- data.frame()
  continue <- TRUE
  i <- 0
  while (continue) {
    i <- i + 1
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
    amount <- amount - h1$quantity * h1$price
    continue <- all(i < nrow(holdings), amount > 0)
  }

  sells
}


get_buy_trades <- function(obj, symbols, amount, lot_size) {
  UseMethod("get_buy_trades")
}


get_buy_trades.data.frame <- function(obj,
                                      symbols = NULL,
                                      amount,
                                      lot_size = 1) {
  checkmate::assert_character(symbols, null.ok = TRUE)
  checkmate::assert_subset(c("symbol","price"), colnames(obj))
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)

  if( is.null(symbols)) {
    symbols <- obj$symbols
  } else {
    checkmate::assert_subset(symbols, obj$symbol)
  }

  buys <- data.frame()
  for(sym in symbols) {
    price <- obj %>%
      dplyr::filter(symbol == sym) %>%
      .$price
    quantity <- amount %/% (price * lot_size) * lot_size
    b1 <- buy(date = Sys.Date(), symbol = sym, quantity = quantity, price = price)
    buys <- rbind(buys, as.data.frame(b1))
  }
  buys
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
  checkmate::assert_choice(target, c("mu", "sd", "sharpe", "yield", "return", "risk", "income"))

  est_stats <- get_estimates_stats(estimates) %>%
    dplyr::select_at(c("symbol", target))

  holdings <- portfolio$holdings
  port_syms <- if(nrow(holdings) > 0) as.character(holdings$symbol) else NULL

  expand.grid(buy = c("CASH", estimates$symbols),
              sell = c("CASH", port_syms)) %>%
    dplyr::mutate_all(as.character) %>%
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
                  active = TRUE) %>%
    dplyr::arrange(-delta)
}



execute_trade_pair <- function(buy,
                               sell,
                               portfolio,
                               prices,
                               amount,
                               lot_size = 1) {
  port <- portfolio
  if (sell != "CASH") {
    sells <- get_sell_trades(port, as.character(sell), amount, lot_size)
    for (i in 1:nrow(sells)) {
      sell <- sells[i, ]
      port <- make_sell(
        port,
        id = sell$id,
        quantity = sell$quantity,
        price = sell$price
      )
    }
    port <- update_market_value(port, prices)
  }

  if(buy != "CASH") {
    amount <- min(amount, port$cash)
    buy <- get_buy_trades(prices, as.character(buy), amount, lot_size)
    port <- make_buy(
      port,
      symbol = as.character(buy$symbol),
      quantity = buy$quantity,
      price = buy$price
    )

    port <- update_market_value(port, prices)
  }

  port
}


evaluate_constraints <- function(pobj1, pobj2, cobj, eobj){

  cc1 <- check_constraints(cobj, pobj1, eobj)

  # Constraints Evaluation
  if(! all(cc1$check))  {

    # compare new constraints to intial state
    cc_eval <- check_constraints(cobj, pobj2, eobj) %>%
      select(id, old_value = value) %>%
      dplyr::inner_join(cc1, by = "id") %>%
      dplyr::mutate_at(c("value", "old_value"), funs(round(., 4))) %>%
      dplyr::mutate(
        improve = check,
        improve = ifelse(!improve &
                           old_value < min & value >= old_value, TRUE, improve),
        improve = ifelse(!improve &
                           old_value > max & value <= old_value, TRUE, improve)
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


#' Compare Constriant Checks for two portfolios
#'
#' Function used to evaluate new portfolio to see if constraints improved upon
compare_constraints <- function(pobj1, pobj2, cobj, eobj){
  checkmate::assert_class(pobj1, "portfolio")
  checkmate::assert_class(pobj2, "portfolio")
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_class(eobj, "estimates")

  # compare new constraints to intial state
  check_constraints(cobj, pobj2, eobj) %>%
    select(id, old_value = value) %>%
    dplyr::inner_join(check_constraints(cobj, pobj1, eobj), by = "id") %>%
    dplyr::mutate_at(c("value", "old_value"), funs(round(., 4))) %>%
    dplyr::mutate(
      improve = check,
      improve = ifelse(!improve &
                         old_value < min & value >= old_value, TRUE, improve),
      improve = ifelse(!improve &
                         old_value > max & value <= old_value, TRUE, improve)
    )
}


select_optimal_portfolio <- function(portfolios, estimates, target, criteria) {
  checkmate::assert_list(portfolios)
  checkmate::assert_class(estimates, "estimates")

  purrr::map_df(
    portfolios,
    get_estimated_port_values,
    eobj = estimates,
    port_only = TRUE,
    .id = "id"
  ) %>%
    dplyr::top_n(ifelse(criteria == "minimize", -1, 1), !!rlang::sym(target)) %>%
    .$id %>%
    portfolios[[.]]
}


#' Meet Symbol Constraints
#'
#' Updates portfolio to meet symbol constraints
meet_symbol_constraints <- function(pobj, cobj, eobj, amount, lot_size = 1){
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_character(purrr::map_chr(cobj$constraints, "type"), min.len = 1)

  # failed symbol constaints
  sym_fc <- check_constraints(cobj, pobj, eobj) %>%
    dplyr::filter(type == "symbol" & (! check))
  nsym_fc <- nrow(sym_fc)
  port <- pobj

  if(nsym_fc > 0) {
    prices <- eobj$prices %>%
      dplyr::filter(date == max(date)) %>%
      split(.symbols) %>%
      purrr::map("price")

    for(i in 1:nsym_fc) {
      min_diff <- sym_fc$value[i] - sym_fc$min[i]
      port_value <- get_market_value(port) %>%
        dplyr::filter(last_updated == max(last_updated)) %>%
        dplyr::pull(net_value)
      .sym <- as.character(sym_fc$args[i])
      if(min_diff < 0) {
        # Buy
        .amount <- abs(min_diff) * port_value
        .amount <- ceiling(.amount/prices[[.sym]]) * prices[[.sym]]
        port <- execute_trade_pair(buy = .sym,
                                   sell = "CASH",
                                   portfolio = port,
                                   prices = obj$prices,
                                   amount = amount,
                                   lot_size = lot_size)
      } else {
        # Sell
        .amount <- (sym_fc$value[i] - sym_fc$max[i]) * port_value
        .amount <- ceiling(.amount/prices[[.sym]]) * prices[[.sym]]
        port <- execute_trade_pair(buy = "CASH",
                                   sell = .sym,
                                   portfolio = port,
                                   prices = obj$prices,
                                   amount = amount,
                                   lot_size = lot_size)
      }
    }
  }

  port
}


meet_group_constraints <- function(pobj,
                                   cobj,
                                   eobj,
                                   tp,
                                   target,
                                   amount,
                                   lot_size = 1,
                                   max_iter = 10,
                                   plot_iter = FALSE) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_character(purrr::map_chr(cobj$constraints, "type"), min.len = 1)
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)
  checkmate::assert_logical(plot_iter)

  # failed group constaints
  grp_fc <- check_constraints(cobj, pobj, eobj) %>%
    dplyr::filter(type == "group" & (!check))
  ngrp_fc <- nrow(grp_fc)
  port <- pobj

  if (ngrp_fc > 0) {
    prices <- eobj$prices %>%
      dplyr::filter(date == max(date)) %>%
      split(.symbols) %>%
      purrr::map("price")
    grp_fc <-
      dplyr::mutate(grp_fc, action = ifelse(value < min, "buy", "sell"))

    for (i in 1:ngrp_fc) {
      val_diff <- ifelse(
        grp_fc$action[i] == "sell",
        grp_fc$value[i] - grp_fc$max[i],
        grp_fc$min[i] - grp_fc$value[i]
      )
      port_value <- get_market_value(port) %>%
        dplyr::filter(last_updated == max(last_updated)) %>%
        dplyr::pull(net_value)
      .syms <- strsplit(as.character(grp_fc$args[i]), ",")[[1]]
      .tp <- if (grp_fc$action[i] == "sell") {
        tp %>% dplyr::filter(sell %in% .syms & (! buy %in% .syms) )
      } else {
        tp %>% dplyr::filter(buy %in% .syms & (! sell %in% .syms))
      }
      continue <- TRUE
      while(continue) {
       port <- nbto(port, cobj, eobj, .tp, target)
      }
    }
  }
}



#' Initial Next Best Trade Optimization sub function
nbto <- function(pobj,
                 cobj,
                 eobj,
                 prices,
                 trade_pairs,
                 target,
                 minimize,
                 amount,
                 lot_size) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(cobj, "constraints")
  checkmate::assert_class(eobj, "estimates")


  # Create Canidate Portfolios
  port_canidates <- trade_pairs %>%
    split(.$id) %>%
    purrr::map(~ execute_trade_pair(.$buy,
                                    .$sell,
                                    pobj,
                                    prices,
                                    amount,
                                    lot_size))

  # Check Canidates Constraints
  port_evals <- port_canidates %>%
    purrr::map( ~ check_constraints(cobj, ., eobj)) %>%
    purrr::map_lgl( ~ all(.$check))

  # Only select canidates that meet constraints
  port_eval_list <- purrr::keep(port_canidates, port_evals)

  # Select optimal portfolio
  if (length(port_eval_list) > 0) {

    if(target %in% c("mu", "sd", "sharpe", "yield")) {
      opt_ports <- port_eval_list %>%
        purrr::map_df(.,
                      get_estimated_port_stats,
                      eobj = eobj,
                      port_only = TRUE,
                      .id = "id")
    } else {
      opt_ports <- port_eval_list %>%
        purrr::map_df(.,
                      get_estimated_port_values,
                      eobj = eobj,
                      .id = "id")
    }

    opt_port_id <- opt_ports %>%
      dplyr::top_n(ifelse(minimize, -1, 1),
                   !!rlang::sym(target)) %>%
      .$id %>%
      head(1)

    port_eval_list[opt_port_id][[1]]
  } else {
    pobj
  }
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

  # Set up
  i <- 0
  continue <- TRUE
  all_active <- FALSE
  prev_iter <- max(obj$portfolio_values$iter)
  t1 <- Sys.time()

  # Check Symbol Constraints
  sym_cc <- check_constraints(obj$constraints, obj$optimal_portfolio, obj$estimates) %>%
    dplyr::filter(type == "symbol" & (! check))
  if(nrow(sym_cc) > 0) {
    port <- meet_symbol_constraints(obj$optimal_portfolio, obj$constraints, obj$estimates, lot_size)

     # Update Obj
    obj$optimal_portfolio <- port
    obj$portfolios <- c(obj$portfolios, list(port))
    obj$portfolio_values <- obj$portfolio_values %>% rbind(
      get_estimated_port_values(port, obj$estimates) %>%
        dplyr::mutate(iter = i + prev_iter)
    )
    prev_iter <- prev_iter + 1
  }


  while (continue) {
    i <- i+1

    if(all_active) obj$trade_pairs$active <- TRUE

    # Trade pair samples
    tp_actives <- obj$trade_pairs %>%
      dplyr::filter(active)
    tp_nactives <- nrow(tp_actives)
    if (tp_nactives == 0) {
      message("No further active trade pairs. Stopping optimization")
      break
    } else {
      tp_smpl <- tp_actives %>%
        #dplyr::sample_n(min(npairs, tp_nactives), weight = delta)
        dplyr::top_n(min(npairs, tp_nactives), wt = delta)
    }

    # Create Canidate Portfolios
    port_canidates <- tp_smpl %>%
      split(.$id) %>%
      purrr::map(
        ~ execute_trade_pair(
          .$buy,
          .$sell,
          obj$optimal_portfolio,
          obj$prices,
          amount,
          lot_size
        )
      )

    # Check constraints for optimal and canidate portfolios
    opt_check <- check_constraints(obj$constraints, obj$optimal_portfolio, obj$estimates) %>%
      .$check %>%
      all()
    cand_checks <- port_canidates %>%
      purrr::map(
        ~ check_constraints(obj$constraints, ., obj$estimates)) %>%
      purrr::map_lgl(~all(.$check))
    active_trades <- cand_checks

    # If optimal meets checks only use canidates that also meet checks
    if (opt_check) {
      port_eval_list <- c(purrr::keep(port_canidates, cand_checks),
                          list(`0` = obj$optimal_portfolio))
    } else {
      # Evaluate Canidates to find canidates that improve on constraints
      port_evals <- port_canidates %>%
        purrr::map_lgl(~ evaluate_constraints(., obj$optimal_portfolio, obj$constraints, obj$estimates))
      active_trades <- port_evals

      # Only select canidates that improve and remove optimal
      port_eval_list <- purrr::keep(port_canidates, port_evals)
    }


    if (length(port_eval_list) > 0) {
      # Select optimal portfolio
      opt_port_id <- port_eval_list %>%
        purrr::map_df(.,
                      get_estimated_port_values,
                      eobj = obj$estimates,
                      # port_only = TRUE,
                      .id = "id") %>%
        dplyr::top_n(ifelse(obj$criteria == "minimize",-1, 1),!!rlang::sym(obj$target)) %>%
        .$id %>%
        head(1)
      opt_port <- port_eval_list[[opt_port_id]]
    } else {
      opt_port_id <- 0
      opt_port <- obj$optimal_portfolio
    }

    # Update Trade Pairs
    if(as.numeric(opt_port_id) == 0) {
      obj$trade_pairs <- obj$trade_pairs %>%
        mutate(selected = ifelse(id %in% as.numeric(names(active_trades)), selected + 1, selected))
    } else {
      obj$trade_pairs <- obj$trade_pairs %>%
        left_join(
          data.frame(id = as.numeric(names(active_trades)),
                     active = active_trades) %>%
            dplyr::mutate(s = 1, t = ifelse(id == as.numeric(opt_port_id), 1, 0)),
          by = "id"
        ) %>%
        tidyr::replace_na(list(s = 0, t = 0)) %>%
        dplyr::mutate(selected = selected + s,
                      trades = trades + t) %>%
        dplyr::mutate(active = ifelse(is.na(active.x), active.y, active.x)) %>%
        dplyr::mutate(active = ifelse(buy == sell[t==1] & buy != "CASH", FALSE, active)) %>%
        dplyr::mutate(active = ifelse(sell == buy[t==1] & sell != "CASH", FALSE, active)) %>%
        dplyr::select(-s,-t, -active.x, -active.y)
    }

    # Update Obj
    obj$optimal_portfolio <- opt_port
    obj$portfolios <- c(obj$portfolios, list(opt_port))
    obj$portfolio_values <- obj$portfolio_values %>% rbind(
      get_estimated_port_values(opt_port, obj$estimates) %>%
        dplyr::mutate(iter = i + prev_iter)
    )
    # if (!opt_check & !is.na(cand_checks[opt_port_id])) {
    #   if (cand_checks[opt_port_id]) {
    #    # all_active <- TRUE
    #     all_active <- FALSE
    #   } else {
    #     all_active <- FALSE
    #   }
    # } else {
    #   all_active <- FALSE
    # }

    if(plot_iter) {
      print(ggplot(obj$portfolio_values, aes_string(x='iter', y=obj$target)) +
              geom_line(size=1.05, color=madstork_pal()(1)) +
              theme_minimal() +
              labs(title = "Madstork Next Best Trade Optimization",
                   subtitle = paste("Iteration", i + prev_iter)))
    }

    # Determine Stopping Conditions
    obj$runtime <- as.numeric(difftime(Sys.time(), t1, units = "sec"))
    runtime_lgl <- obj$runtime < max_runtime

    if(i >= improve_lag) {
      target_improve <- obj$portfolio_values %>%
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

    continue <- all(c(runtime_lgl, improve, iters))
  }

  # Get Consoldated trades
  new_trades <- dplyr::anti_join(obj$optimal_portfolio %>% get_trades(),
                                 obj$portfolios[[1]] %>% get_trades(),
                                 by="id")
  new_sells <- new_trades %>%
    dplyr::filter(type == "sell")
  new_buys <- new_trades %>%
    dplyr::filter(type == "buy") %>%
    dplyr::group_by(date_added, transaction_date, type, symbol, price, desc) %>%
    dplyr::summarise_at("quantity", sum) %>%
    dplyr::mutate(amount = price * quantity)
  final_port <- obj$portfolios[[1]]
  for (.id in new_sells$id) {
    sell <- dplyr::filter(new_sells, id == .id) %>%
      do(get_sell_trades(obj$portfolios[[1]], as.character(.$symbol), .$amount, lot_size))

    final_port <- final_port %>%
      make_sell(
        id = sell$id,
        quantity = sell$quantity,
        price = sell$price,
        desc = as.character(sell$desc)
      )
  }
  for (sym in new_buys$symbol) {
    buy <- dplyr::filter(new_buys, symbol == as.character(sym))
    final_port <- final_port %>%
      make_buy(
        symbol = as.character(sym),
        quantity = buy$quantity,
        price = buy$price,
        desc = as.character(buy$desc)
      )
  }
  obj$optimal_portfolio <- update_market_value(final_port, obj$prices)

  obj
}



# Optimization Report Functions -------------------------------------------


po_symbol_share_chart <- function(obj) {

  purrr::map_df(obj$portfolios,
                ~get_symbol_estimates_share(pobj = ., eobj = obj$estimates), .id = "iter") %>%
    ggplot(., aes(x=as.numeric(iter), y=portfolio_share, color = symbol, group=symbol)) +
    geom_line() +
    scale_color_madstork() +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    labs(title = "Next Best Trade Optimization",
         subtitle = "Portfolio Symbol Share Path",
         caption = "MadStork ")
}


po_constraints_charts <- function(obj) {

  purrr::map_df(obj$portfolios,
                ~check_constraints(obj$constraints, ., obj$estimates), .id = "iter") %>%
    ggplot(., aes(x=as.numeric(iter), y=value, group=args)) +
    geom_ribbon(aes(ymin = min, max = ifelse(max == Inf, value, max)), color="grey75", alpha=.25) +
    geom_line(color = "blue") +
    geom_point(size = 2, shape=1, color ="blue") +
    facet_wrap(~type+args, scales = "free") +
    scale_color_madstork() +
    theme_minimal()
}
