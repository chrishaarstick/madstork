


# Portfolio Class ---------------------------------------------------------


#' Portfolio Object Constructor function
#'
#' Function creates a new S3 portfolio object. If activity list provided,
#' activity is processed, the historical market value is calculated and the
#' updated portfolio is returned
#'
#' @param name name of portfolio. requires string input
#' @param activity optional list of historical portfolio activity
#'
#' @export
#' @return portfolio object
portfolio <- function(name, activity = NULL) {

  checkmate::assert_character(name)
  checkmate::assert_list(activity, null.ok = TRUE)

  # Create empty structures
  empty_holdings <- empty_holdings_df()
  empty_activity <- empty_activity_df()
  empty_trades <- empty_trades_df()
  empty_income <- empty_income_df()
  empty_gains <- empty_gains_df()
  empty_market_value <- empty_market_value_df()
  empty_holdings_mv <- empty_holdings_market_value_df()

  pobj <- structure(
    list(
      name = name,
      cash = 0,
      tax_liability = 0,
      date_created = Sys.Date(),
      date_initialized = Sys.Date(),
      interest_rate = 0,
      holdings = empty_holdings,
      activity = empty_activity,
      trades = empty_trades,
      income = empty_income,
      gains = empty_gains,
      market_value = empty_market_value,
      holdings_market_value = empty_holdings_mv
    ),
    class = "portfolio"
  )

  if(! is.null(activity)) {

    for(i in seq_along(activity)) {
      pobj <- process(activity[[i]], pobj)
    }

    pobj <- past_market_value(pobj)
    pobj$holdings_market_value <- update_holdings_market_value(pobj)
  }

  pobj
}





# Getters -----------------------------------------------------------------



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
#' portfolio("new_port") %>%
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
#' @return tibble of activity history
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port") %>%
#' make_deposit(amount = 100) %>%
#' get_activity(.)
get_activity <- function(pobj) {
  checkmate::assert_class(pobj, "portfolio")
  activity <- pobj$activity
  if (nrow(activity) == 0) {
    activity
  } else{
    activity %>%
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
#' @return tibble of trade history
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port") %>%
#' get_trades(.)
get_trades <- function(pobj) {
  checkmate::assert_class(pobj, "portfolio")

  trades <- pobj$trades
  if (nrow(trades) == 0) {
    trades
  } else {
    trades %>%
      dplyr::select_at(
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
#' @return holdings tibble
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port") %>%
#' get_holdings(.)
get_holdings <- function(pobj) {
  checkmate::assert_class(pobj, "portfolio")
  holdings <- pobj$holdings
  if (nrow(holdings) == 0) {
    holdings
  } else{
    holdings %>%
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
  holdings <- pobj$holdings
  if (nrow(holdings) == 0) {
    NULL
  } else{
    holdings %>%
      dplyr::filter_at('id', dplyr::any_vars(. == .id)) %>%
      dplyr::select_at(
        c(
          "id",
          "date_added",
          "transaction_date",
          "symbol",
          "quantity",
          "price",
          "desc"
        )
      )
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
#' portfolio("new_port") %>%
#' get_gains(.)
get_gains <- function(pobj) {
  checkmate::assert_class(pobj, "portfolio")
  gains <- pobj$gains
  if (nrow(gains) == 0) {
    gains
  } else {
    gains %>%
      dplyr::select_at(
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
#' portfolio("new_port") %>%
#' get_income(.)
get_income <- function(pobj) {
  checkmate::assert_class(pobj, "portfolio")
  income <- pobj$income
  if (nrow(income) == 0) {
    income
  } else{
    income %>%
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


#' Get Portfolio Tax Liability
#'
#' @param pobj portfolio object
#'
#' @return Porfolio's current tax liability
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port") %>%
#' get_tax_liability(.)
get_tax_liability <- function(pobj) {
  checkmate::assert_class(pobj, "portfolio")
  pobj$tax_liability
}


#' Get Portfolio Market Value
#'
#' @param pobj portfolio object
#'
#' @return tibble with portfolio's market value
#' @export
get_market_value <- function(pobj){
  checkmate::assert_class(pobj, "portfolio")
  pobj$market_value
}


#' Get Portfolio's Holdings Market Value
#'
#' @param pobj portfolio object
#'
#' @return tibble with portfolio holdings market value
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
#' @return tibble with portfolio share by symbol
#' @export
get_symbol_portfolio_share <- function(pobj) {
  checkmate::assert_class(pobj, "portfolio")

  pobj$holdings_market_value %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise_at("portfolio_share", sum)
}



# Functions ---------------------------------------------------------------


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
#'portfolio("new_port") %>%
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


#' Get Portfolio Return
#'
#' Function extracts portfolio market values and calculates returns for given
#' time horizon. Function calculates the net investment value which is return on
#' invests only (net cash change)
#'
#' @param pobj portfolio object
#' @param start_date starting date for return horizon. Date class required
#' @param end_date end date for return horizon. Date class required
#'
#' @return data.frame with cash, investments, net investment and net value
#'   return statistic
#' @export
get_portfolio_returns <- function(pobj, start_date, end_date = Sys.Date()) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_subset(class(start_date), c("Date", "POSIXlt", "POSIXt"))
  checkmate::assert_subset(class(end_date), c("Date", "POSIXlt", "POSIXt"))

  get_market_value(pobj) %>%
    dplyr::filter(date >= start_date, date <= end_date) %>%
    dplyr::group_by(date) %>%
    dplyr::filter(last_updated == max(last_updated)) %>%
    dplyr::ungroup() %>%
    #dplyr::mutate(net_investment_value = investments_value + cash - c(0, diff(cash))) %>%
    dplyr::summarise_at(c("cash", "investments_value", "net_value"),
                        funs((last(.) - first(.))/first(.)))
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


# Internal helper function for creating empty holdings df
empty_holdings_df <- function() {
  tibble::tibble(
    id = integer(),
    date_added = character(),
    transaction_date = character(),
    symbol = character(),
    quantity = numeric(),
    price = numeric(),
    desc = character()
  )
}


# Internal Helper function to create an empty market-value df
empty_market_value_df <- function() {
  tibble::tibble(
    last_updated = as.character(),
    date = as.character(),
    cash = numeric(),
    investments_value = numeric(),
    loans = numeric(),
    tax_liability = numeric(),
    net_value = numeric()
  )
}

# Internal helper function to create an empty holdings market value df
empty_holdings_market_value_df <- function() {
  tibble::tibble(
    id = integer(),
    last_updated = as.character(),
    symbol = character(),
    quantity = numeric(),
    price = numeric(),
    market_value = numeric(),
    cost_basis = numeric(),
    unrealized_gain = numeric(),
    dividend = numeric(),
    annual_income = numeric(),
    yield = numeric(),
    investments_share = numeric(),
    portfolio_share = numeric()
  )
}



# Methods -----------------------------------------------------------------


#'@export
print.portfolio <- function(x, ...){
  checkmate::assert_class(x, "portfolio")

  cat("Portfolio", x$name, "\n")
  cat("---------------------------", "\n")

  if(nrow(x$market_value)>0){
    mv <- dplyr::filter(x$market_value, date == max(date))
    cat("Market Value as of:", as.character(mv$last_updated), "\n")
    cat("* Net Value   ", scales::dollar(mv$net_value), "\n")
    cat("* Investments ", scales::dollar(mv$investments_value), "\n")
    cat("* Cash        ", scales::dollar(mv$cash), "\n")
  }

  if(nrow(x$holdings_market_value) > 0){
    cat("Top 5 Holdings by Market Value:", "\n")
    x$holdings_market_value %>%
      dplyr::top_n(5, market_value) %>%
      dplyr::arrange(-market_value) %>%
      dplyr::select(symbol, market_value, cost_basis, unrealized_gain) %>%
      print()
    cat("\n")
  }

  cat("Recent Activity:", '\n')
  get_activity(x) %>%
    dplyr::top_n(5, id) %>%
    dplyr::arrange(-id) %>%
    print()
}





# Generics ----------------------------------------------------------------


#' Process Portfolio Activity
#'
#' Generic function to process madstork activity, income, and trade class
#' objects on a portfolio
#'
#' @param obj activity object to process
#' @param pobj portfolio object
#' @param ... additional arguments to pass to the portfolio object
#'
#' @return updated portfolio object with activity
#' @export
process <- function(obj, pobj, ...) {
  UseMethod("process")
}
