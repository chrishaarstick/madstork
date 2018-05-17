
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
  checkmate::assert_choice(target, c("mu", "sd", "sharpe", "yield"))
  checkmate::assert_character(desc)
  checkmate::assert_number(version)

  criteria <- ifelse(target %in% c("sd"), "minimize", "maximize")

  structure(
    list(
      portfolio = portfolio,
      estimates = estimates,
      constraints = constraints,
      target = target,
      criteria = criteria,
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
          quantity = quantity,
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


trade_pairs <- function(obj){
  checkmate::assert_class(obj, "portfolio_optimization")

  est_stats <- get_estimates_stats(obj$estimates) %>%
    dplyr::select_at(c("symbol", obj$target))

  expand.grid(buy = c("CASH", obj$estimates$symbols),
              sell = c("CASH", as.character(obj$portfolio$holdings$symbol))) %>%
    dplyr::filter(buy != sell) %>%
    dplyr::mutate(id = row_number()) %>%
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

  buy <- get_buy_trades(estimates, as.character(buy), amount, lot_size)
  p2 <- make_buy(portfolio, symbol=as.character(buy$symbol), quantity = buy$quantity, price = buy$price)

  if(sell != "CASH") {
    sell <- get_sell_trades(portfolio, as.character(sell), amount, lot_size)
    p2 <- make_sell(p2, id = sell$id, quantity = sell$quantity, price = sell$price)
  }

  update_market_value(p2, refresh)
}


select_optimal_portfolio <- function(portfolios, estimates, target, criteria) {
  checkmate::assert_list(portfolios)
  checkmate::assert_class(estimates, "estimates")

  port <- purrr::map_dfr(portfolios, get_estimated_port_stats, eobj = estimates, .id = "id") %>%
    dplyr::filter(type == "portfolio") %>%
    dplyr::top_n(ifelse(criteria == "minimize", -1, 1), !! rlang::sym(target)) %>%
    .$id %>%
    as.numeric
  portfolios[[port]]
}
