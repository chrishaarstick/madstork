


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

  structure(list(
    name = name,
    cash = cash,
    holdings = data.frame(),
    activity = data.frame(),
    trades = data.frame(),
    realized_gains = data.frame()
  ),
  class = "portfolio")
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
#' portfolio("new_port", cash = 100) %>%
#' get_activity(.)
get_activity <- function(pobj) {
  stopifnot(class(pobj) == "portfolio")
  pobj$activity
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
  pobj$trades
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
  pobj$holdings
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
#' @return realized_gains data.frame
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port", cash = 100) %>%
#' get_realized_gains(.)
get_realized_gains <- function(pobj) {
  stopifnot(class(pobj) == "portfolio")
  pobj$realized_gains
}

