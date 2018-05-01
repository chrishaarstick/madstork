
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
                            amount,
                            lot_size = 1,
                            partial = TRUE) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)
  checkmate::assert_flag(partial)

  holdings <- get_holdings(pobj) %>%
    dplyr::select(id, symbol, quantity) %>%
    dplyr::inner_join(
      get_holdings_market_value(pobj) %>%
        dplyr::select(symbol, quantity, price, market_value),
      by = c("symbol", "quantity"))

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


get_buy_trades <- function(obj, amount, lot_size) {
  UseMethod("get_buy_trades")
}


get_buy_trades.estimates <- function(obj,
                                     amount,
                                     lot_size = 1) {
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)

  symbols <- obj$symbols
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
                                     amount,
                                     lot_size = 1) {
  checkmate::assert_number(amount, lower = 0)
  checkmate::assert_number(lot_size, lower = 0)

  holdings <- p1$holdings_market_value
  symbols <- holdings$symbol
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
