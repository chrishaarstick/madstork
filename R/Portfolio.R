


# Portfolio Class ---------------------------------------------------------


#' Portfolio Object Constructor function
#'
#' Function creates a new S3 portfolio object
#'
#' @param name name of portfolio. requires string input
#' @param cash cash balance. requires numeric input
#'
#' @return portfolio object
new_portfolio <- function(name,
                          cash) {
  stopifnot(is.character(name))
  stopifnot(is.numeric(cash))

  structure(
    list(
      name = name,
      cash = cash,
      tax_liability = 0,
      holdings = data.frame(),
      activity = data.frame(),
      trades = data.frame(),
      income = data.frame(),
      gains = data.frame(),
      market_value = data.frame(),
      holdings_market_value = data.frame()
    ),
    class = "portfolio"
  )
}



#' Portfolio Object Validation function
#'
#' Validator function to check for valid inputs
#'
#' @param x portfolio object
#'
#' @return valid portfolio object
validate_portfolio <- function(x) {
  if (x$cash < 0) {
    stop("Loans not allowed - initial cash balance should be >= 0",
         .call = F)
  }
  x
}



#' Portfolio creation helper function
#'
#' Function to create Portfolio object. Calls internal new_portfolio and
#' validate_portfolio functions
#'
#' @param name name of portfolio. requires string input
#' @param cash cash balance. requires numeric input. defaults to 0
#'
#' @return returns a Porftolio object
#' @export
#'
#' @examples
#' portfolio("new_port", cash=0)
portfolio <- function(name,
                      cash = 0) {
  validate_portfolio(new_portfolio(name, cash))
}



#' Get Portfolio's Cash
#'
#' getter function to return Portfolio's cash balance
#'
#' @param pobj portfolio object
#'
#' @return numeric cash value
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port", cash = 100) %>%
#' get_cash(.)
get_cash <- function(pobj) {
  stopifnot(class(pobj) == "portfolio")
  pobj$cash
}



#' Get Portfolio Activity
#'
#' getter function to return Portfolio's activity
#'
#' activity relates to any portfolio cash transactions
#'
#' @param pobj portfolio object
#'
#' @return data.frame of activity history
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port") %>%
#' make_deposit(amount = 100) %>%
#' get_activity(.)
get_activity <- function(pobj) {
  stopifnot(class(pobj) == "portfolio")
  a <- pobj$activity
  if (nrow(a) == 0) {
    a
  } else{
    a %>%
      dplyr::select(id, date_added, transaction_date, type, amount, desc)
  }
}


#' Get Portfolio Trades
#'
#' getter function to return Portfolio's trades
#'
#' trades are portfolio investment security transactions
#'
#' @param pobj portfolio object
#'
#' @return data.frame of trade history
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port", cash = 100) %>%
#' get_trades(.)
get_trades <- function(pobj) {
  stopifnot(class(pobj) == "portfolio")

  t <- pobj$trades
  if (nrow(t) == 0) {
    t
  } else {
    t %>% dplyr::select(id,
                        date_added,
                        transaction_date,
                        type,
                        symbol,
                        quantity,
                        price,
                        amount,
                        desc)
  }
}


#' Get Portfolio Holdings
#'
#' getter function to return Portfolio's current holdings
#'
#' holdings are currently held investment securities such as a stock or etf
#'
#' @param pobj portfolio object
#'
#' @return holdings data.frame
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port", cash = 100) %>%
#' get_holdings(.)
get_holdings <- function(pobj) {
  stopifnot(class(pobj) == "portfolio")
  h <- pobj$holdings
  if (nrow(h) == 0) {
    h
  } else{
    h %>%
      dplyr::select(id,
                    date_added,
                    transaction_date,
                    symbol,
                    quantity,
                    price,
                    desc)
  }
}


#' Get Holding helper function
#'
#' @param pobj portfolio object
#' @param .id id of holding to extract
#'
#' @return holding with id == .id
#' @export
get_holding <- function(pobj, .id) {
  stopifnot(class(pobj) == "portfolio")
  stopifnot(is.numeric(.id))
  h <- pobj$holdings
  if (nrow(h) == 0) {
    NULL
  } else{
    h %>%
      dplyr::filter_at('id',  any_vars(. == .id)) %>%
      dplyr::select(id,
                    date_added,
                    transaction_date,
                    symbol,
                    quantity,
                    price,
                    desc)
  }
}


#' Get Portfolio Realized Gains
#'
#' getter function to return Portfolio's realized gains
#'
#' realized gains are gains or losses as a result of investment activity.
#' Realized when sold
#'
#' @param pobj portfolio object
#'
#' @return Portfolio's realized gains
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port", cash = 100) %>%
#' get_gains(.)
get_gains <- function(pobj) {
  stopifnot(class(pobj) == "portfolio")
  g <- pobj$gains
  if (nrow(g) == 0) {
    g
  } else {
    g %>% dplyr::select(
      id,
      symbol,
      quantity,
      purchase_date,
      purchase_price,
      sale_date,
      sale_price,
      gain,
      type,
      tax_rate,
      tax_liability
    )
  }
}



#' Get Portfolio Tax Liability
#'
#' @param pobj portfolio object
#'
#' @return Porfolio's current tax liability
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port", cash = 100) %>%
#' get_tax_liability(.)
get_tax_liability <- function(pobj) {
  stopifnot(class(pobj) == "portfolio")
  pobj$tax_liability
}


#' Get Portfolio Investment Income
#'
#' Returns realized income only. Does not estimate future income payments
#'
#' @param pobj portfolio object
#'
#' @return Portfolio's past income payments from investments
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port", cash = 100) %>%
#' get_income(.)
get_income <- function(pobj) {
  stopifnot(class(pobj) == "portfolio")
  i <- pobj$income
  if (nrow(i) == 0) {
    i
  } else{
    i %>%
      dplyr::select(id,
                    date_added,
                    transaction_date,
                    symbol,
                    quantity,
                    payment,
                    amount,
                    desc)
  }
}




#' Get Porfolio Holding's Market Value
#'
#' Gets current market price and annual dividends for portfolio holdings.
#' Calculates market value, income and yield
#'
#' @param pobj
#'
#' @return data.frame with portfolio's holdings market value
#' @export
#'
#' @examples
#' \dontrun{
#' p1 <- portfolio("new_port", cash=0) %>%
#'  make_deposit(amount = 5000) %>%
#'  make_buy(Sys.Date()-365, symbol = "SPY", quantity = 10, price = 200) %>%
#'  make_buy(Sys.Date()-365, symbol = "TLT", quantity = 25, price = 100)
#' get_holdings_market_value(p1)
#' }
#'
get_holdings_market_value <- function(pobj) {
  stopifnot(class(pobj) == "portfolio")
  holdings <- get_holdings(pobj)
  symbols <- holdings$symbol
  prices <- get_current_prices(symbols)
  dividends <- get_annual_dividends(symbols)

  holdings %>%
    dplyr::select(symbol, quantity, price, date_added) %>%
    dplyr::rename(cost = price) %>%
    dplyr::inner_join(prices %>%
                        select(symbol, price, last_updated)) %>%
    dplyr::inner_join(dividends %>%
                        select(symbol, annual_dividend) %>%
                        rename(dividend = annual_dividend)) %>%
    dplyr::group_by(last_updated,
                    symbol,
                    price,
                    dividend) %>%
    dplyr::summarise(quantity = sum(quantity),
                     cost = weighted.mean(cost, quantity)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      market_value = quantity * price,
      cost_basis = quantity * cost,
      unrealized_gain = quantity * (price - cost),
      annual_income = quantity * dividend,
      yield = dividend / price
    ) %>%
    dplyr::select(
      last_updated,
      symbol,
      quantity,
      price,
      market_value,
      cost_basis,
      unrealized_gain,
      dividend,
      annual_income,
      yield
    ) %>%
    as.data.frame()
}


#' Update Porfolio Market Value
#'
#' Function to update porfolio and holding's market value
#'
#' Function appends portfolio's market value record for historical analysis and
#' overwrites the holdings market value
#'
#' @param pobj portfolio object
#'
#' @return Updated Porfolio object with new market value
#' @export
#'
#' @examples
#' \dontrun{
#' p1 <- portfolio("new_port", cash=0) %>%
#'  make_deposit(amount = 5000) %>%
#'  make_buy(Sys.Date()-365, symbol = "SPY", quantity = 10, price = 200) %>%
#'  make_buy(Sys.Date()-365, symbol = "TLT", quantity = 25, price = 100)
#' update_holdings_market_value(p1)
#' }
update_market_value <- function(pobj){
  stopifnot(class(pobj) == "portfolio")

  holdings <- get_holdings(pobj)
  holdings_market_value <- get_holdings_market_value(pobj)

  current_market_value <- data.frame(
    last_updated = Sys.time(),
    cash = as.numeric(get_cash(pobj)),
    investments_value = sum(holdings_market_value$market_value),
    investments_annual_income = sum(holdings_market_value$annual_income),
    loans = as.numeric(0),
    tax_liability = as.numeric(get_tax_liability(pobj))) %>%
    dplyr::mutate(net_value = cash + investments_value - loans - tax_liability)

  pobj$holdings_market_value <- holdings_market_value
  pobj$market_value <- rbind(pobj$market_value, current_market_value)

  pobj
}
