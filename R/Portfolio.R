


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
  checkmate::assert_class(pobj, "portfolio")
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
  checkmate::assert_class(pobj, "portfolio")
  a <- pobj$activity
  if (nrow(a) == 0) {
    a
  } else{
    a %>%
      dplyr::select_at(c(
        "id",
        "date_added",
        "transaction_date",
        "type",
        "amount",
        "desc"
      ))
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
  checkmate::assert_class(pobj, "portfolio")

  t <- pobj$trades
  if (nrow(t) == 0) {
    t
  } else {
    t %>% dplyr::select_at(
      c(
        "id",
        "date_added",
        "transaction_date",
        "type",
        "symbol",
        "quantity",
        "price",
        "amount",
        'desc'
      )
    )
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
  checkmate::assert_class(pobj, "portfolio")
  h <- pobj$holdings
  if (nrow(h) == 0) {
    h
  } else{
    h %>%
      dplyr::mutate_at("symbol", as.character) %>%
      dplyr::select_at(c(
        "id",
        "date_added",
        "transaction_date",
        "symbol",
        "quantity",
        "price",
        "desc"
      ))
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
  checkmate::assert_class(pobj, "portfolio")
  stopifnot(is.numeric(.id))
  h <- pobj$holdings
  if (nrow(h) == 0) {
    NULL
  } else{
    h %>%
      dplyr::filter_at('id',  any_vars(. == .id)) %>%
      dplyr::select_at(c(
        "id",
        "date_added",
        "transaction_date",
        "symbol",
        "quantity",
        "price",
        "desc"
      ))
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
  checkmate::assert_class(pobj, "portfolio")
  g <- pobj$gains
  if (nrow(g) == 0) {
    g
  } else {
    g %>% dplyr::select_at(
      c(
        "id",
        "symbol",
        "quantity",
        "purchase_date",
        "purchase_price",
        "sale_date",
        "sale_price",
        "gain",
        "type",
        "tax_rate",
        'tax_liability'
      )
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
  checkmate::assert_class(pobj, "portfolio")
  pobj$tax_liability
}


#' Settle Portfolio Tax Liability
#'
#' Function to settle the tax liabilty. Option to make cash withdraw and add to
#' portfolio activity
#'
#' @param pobj portfolio object
#' @param date date of transaction. default is current date
#' @param amount amount of tax settlement
#' @param withdraw logical option to make a cash withdraw from portfolio
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#'library(tidyverse)
#'portfolio("new_port", cash=0) %>%
#'  make_deposit(amount = 2000) %>%
#'  make_buy(Sys.Date()-1, symbol = "SPY", quantity = 10, price = 100) %>%
#'  make_sell(id = 1, quantity = 5, price = 105) %>%
#'  settle_tax_liability(amount = 7.5, withdraw = TRUE)
settle_tax_liability <- function(pobj, date = Sys.Date(), amount, withdraw = FALSE){
  checkmate::assert_class(pobj, "portfolio")

  if(withdraw){
    pobj <- make_withdraw(pobj, date, amount, desc = "Tax Payment")
  }
  pobj$tax_liability <- pobj$tax_liability - amount


  pobj
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
  checkmate::assert_class(pobj, "portfolio")
  i <- pobj$income
  if (nrow(i) == 0) {
    i
  } else{
    i %>%
      dplyr::select_at(
        c(
          "id",
          "date_added",
          "transaction_date",
          "symbol",
          "quantity",
          "payment",
          "amount",
          "desc"
        )
      )
  }
}




#' Updated Porfolio Holding's Market Value
#'
#' Updateds current market price and annual dividends for portfolio holdings.
#' Calculates market value, income and yield
#'
#' @param pobj portfolio object
#' @param eobj estimates object. Default is NULL. If provided, prices and
#'   dividends updated from Estimates object
#' @param refresh logical option to refresh holdings price and dividend data.
#'   default it TRUE. If TRUE, will override estimates if estimates provided
#'
#' @return data.frame with portfolio's holdings market value
#' @export
#' @importFrom stats weighted.mean
#'
#' @examples
#' \dontrun{
#' p1 <- portfolio("new_port", cash=0) %>%
#'  make_deposit(amount = 5000) %>%
#'  make_buy(Sys.Date()-365, symbol = "SPY", quantity = 10, price = 200) %>%
#'  make_buy(Sys.Date()-365, symbol = "TLT", quantity = 25, price = 100)
#'  update_holdings_market_value(p1)
#' }
#'
update_holdings_market_value <- function(pobj, eobj = NULL, refresh = TRUE) {
  checkmate::assert_class(pobj, "portfolio")
  if(! is.null(eobj) ) checkmate::assert_class(eobj, "estimates")
  checkmate::assert_flag(refresh)
  holdings <- get_holdings(pobj)
  symbols <- unique(holdings$symbol)

  if (refresh) {
    prices <- get_current_prices(symbols)
    dividends <- get_annual_dividends(symbols) %>%
      dplyr::select(symbol, dividend = annual_dividend)
  } else{

    if( is.null(eobj) ) {
      holdings_mv <- get_holdings_market_value(pobj)
      prices <- holdings_mv %>%
        dplyr::distinct(symbol, price, last_updated)
      dividends <- holdings_mv %>%
        dplyr::distinct(symbol, dividend)
    } else {
      prices <- eobj$prices %>%
        dplyr::group_by(symbol) %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::select(symbol, price, last_updated = date)
      dividends <- eobj$dividends
    }
  }

  holdings %>%
    dplyr::select(id, symbol, quantity, price, date_added) %>%
    dplyr::rename(cost = price) %>%
    dplyr::inner_join(prices, by = "symbol") %>%
    dplyr::inner_join(dividends, by = "symbol") %>%
    dplyr::mutate(
      market_value = quantity * price,
      cost_basis = quantity * cost,
      unrealized_gain = quantity * (price - cost),
      annual_income = quantity * dividend,
      yield = dividend / price,
      investments_share = market_value/sum(market_value),
      portfolio_share = market_value/(sum(market_value)+get_cash(pobj))
    ) %>%
    dplyr::select_at(c(
      "id",
      "last_updated",
      "symbol",
      "quantity",
      "price",
      "market_value",
      "cost_basis",
      "unrealized_gain",
      "dividend",
      "annual_income",
      'yield',
      "investments_share",
      "portfolio_share"
    ))
}

#' Update Porfolio Market Value
#'
#' Function to update porfolio and holding's market value
#'
#' Function appends portfolio's market value record for historical analysis and
#' overwrites the holdings market value
#'
#' @inheritParams update_holdings_market_value
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
#'  update_market_value(p1)
#' }
update_market_value <- function(pobj, eobj = NULL,refresh = TRUE) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_flag(refresh)

  holdings <- get_holdings(pobj)
  holdings_market_value <- update_holdings_market_value(pobj, eobj, refresh)

  current_market_value <- data.frame(
    last_updated = Sys.time(),
    cash = as.numeric(get_cash(pobj)),
    investments_value = sum(holdings_market_value$market_value),
    investments_annual_income = sum(holdings_market_value$annual_income),
    loans = as.numeric(0),
    tax_liability = as.numeric(get_tax_liability(pobj))
  ) %>%
    dplyr::mutate(net_value = cash + investments_value - loans - tax_liability)

  pobj$holdings_market_value <- holdings_market_value
  pobj$market_value <-
    rbind(pobj$market_value, current_market_value)

  pobj
}



#' Get Portfolio Market Value
#'
#' @param pobj portfolio object
#'
#' @return data.frame with portfolio's market value
#' @export
get_market_value <- function(pobj){
  checkmate::assert_class(pobj, "portfolio")

  pobj$market_value
}


#' Get Portfolio's Holdings Market Value
#'
#' @param pobj portfolio object
#'
#' @return data.frame with portfolio holdings market value
#' @export
get_holdings_market_value <- function(pobj){
  checkmate::assert_class(pobj, "portfolio")

  pobj$holdings_market_value
}


#' Get Share of Total Portfolio By Symbol
#'
#' Aggregates holdings portfolio share to symbol
#'
#' @param pobj portfolio object
#'
#' @return data.frame with portfolio share by symbol
#' @export
get_symbol_portfolio_share <- function(pobj) {
  checkmate::assert_class(pobj, "portfolio")

  pobj$holdings_market_value %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise_at("portfolio_share", sum)
}



#'@export
#'@rdname print
print.portfolio <- function(pobj){
  checkmate::assert_class(pobj, "portfolio")

  cat("Portfolio", pobj$name, "\n")
  cat("---------------------------", "\n")

  if(nrow(pobj$market_value)>0){
    mv <- dplyr::filter(pobj$market_value, last_updated == max(last_updated))
    cat("Market Value as of:", as.character(mv$last_updated), "\n")
    cat("* Net Value   ", scales::dollar(mv$net_value), "\n")
    cat("* Investments ", scales::dollar(mv$investments_value), "\n")
    cat("* Cash        ", scales::dollar(mv$cash), "\n")
    cat("* Annual Income", scales::dollar(mv$investments_annual_income),"\n\n")
  }

  if(nrow(pobj$holdings_market_value) > 0){
    cat("Top 5 Holdings by Market Value:", "\n")
    pobj$holdings_market_value %>%
      dplyr::top_n(5, market_value) %>%
      dplyr::arrange(-market_value) %>%
      dplyr::select(symbol, market_value, cost_basis, unrealized_gain) %>%
      print()
    cat("\n")
  }

  cat("Recent Activity:", '\n')
  get_activity(pobj) %>%
    dplyr::top_n(5, id) %>%
    dplyr::arrange(-id) %>%
    print()
}


#' Save Portfolio Function
#'
#' Function saves the portfolio to file. RData file extension required.
#'
#' @param pobj portfolio object
#' @param path file path to save portfolio
#' @param overwrite logical object to overwrite existing portfolio if exists.
#'   default to TRUE
#'
#' @export
save_portfolio <- function(pobj, path, overwrite = TRUE){
  checkmate::assert_class(pobj, "portfolio")
  stopifnot(tools::file_ext(path) == "RData")

  if(overwrite){
    save(pobj, file = path)
  }else if (! file.exists(path)){
    save(pobj, file = path)
  }else{
    message("portfolio already exists - portfolio will not be saved")
  }
}


#' Load Existing Portfolio
#'
#' Function to load into memory saved portfolio object in .RData file
#'
#' @param path valid file path to porfolio object. should be .RData file type
#'
#' @return portfolio object
#' @export
load_portfolio <- function(path){
  stopifnot(file.exists(path))
  port_env <- new.env()
  load(path, envir = port_env)
  stopifnot("pobj" %in% ls(port_env))
  stopifnot(class(port_env$pobj) == "portfolio")
  return(port_env$pobj)
}
