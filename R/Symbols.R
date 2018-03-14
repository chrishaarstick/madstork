



#' Get Security Price and Volume Data
#'
#' Wrapper function to get historical security prices for 1 or more symbols.
#' Function uses getSymbol function from quantmod package. Returns Open, Close,
#' High, Low, Adj Close, and Volume
#'
#' @param symbols vector of stock symbol characters. can be 1 or more
#' @param start_date starting date for historical prices. default is
#'   '01-01-1990'
#' @param end_date ending date for historical prices. default is current date
#' @param error_handling option to handle errors within foreach loop. options
#'   are 'pass', 'remove', or 'stop'
#'
#' @return data.frame with symbol historical prices. schema is date, symbol,
#'   open, close, high, low, adj_close, volume
#' @export
#'
#' @importFrom quantmod Op Cl Hi Lo Ad Vo getSymbols
#' @importFrom foreach foreach %do%
#' @importFrom zoo index
#' @examples
#' library(tidyverse)
#' library(quantmod)
#' symbols <- c("spy", "tlt")
#' sym_data <- symbols %>% get_ochlav(.)
get_ochlav <- function(symbols,
                       start_date = "1990-01-01",
                       end_date = Sys.Date(),
                       error_handling = "pass",
                       warning = FALSE) {
  foreach(
    sym = toupper(symbols),
    .combine = "rbind",
    .errorhandling = error_handling
  ) %do% {
    # Get Daily Data
    getSymbols(
      sym,
      from = start_date,
      to = end_date,
      warning4.0 = warning,
      yahoo.warning = warning
    )
    symbol <- gsub("[[:punct:]]", "", sym)

    # Combine into Daily Data
    data.frame(
      date = as.Date(index(get(symbol))),
      symbol = as.character(symbol),
      open = as.numeric(Op(get(symbol))),
      close = as.numeric(Cl(get(symbol))),
      high = as.numeric(Hi(get(symbol))),
      low = as.numeric(Lo(get(symbol))),
      adj_close = as.numeric(Ad(get(symbol))),
      volume = as.numeric(Vo(get(symbol)))
    )
  }
}


#' Get Historical Security Prices
#'
#' Wrapper function around get_ochlav function that returns historical securtity prices
#'
#' Prices are set to adjusted close
#'
#' @inheritParams get_ochlav
#'
#' @return single data.frame with symbol price history row bounded in long format
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(quantmod)
#' symbols <- c("spy", "tlt")
#' prices <- symbols %>% get_prices(.)
get_prices <- function(symbols,
                       start_date = "1990-01-01",
                       end_date = Sys.Date(),
                       error_handling = "pass",
                       warning = FALSE) {
  get_ochlav(symbols, start_date, end_date, error_handling, warning) %>%
    select(date, symbol, adj_close) %>%
    rename(price = adj_close)
}




#' Get Current Security Prices
#'
#' Wrapper function around get_prices function that returns current prices. Adds
#' timestamp of date pulled
#'
#' @inheritParams get_ochlav
#'
#' @return data.frame with 1 record per symbol with current adjusted close price
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(quantmod)
#' symbols <- c("spy", "tlt")
#' prices <- symbols %>% get_current_prices(.)
get_current_prices <- function(symbols,
                               error_handling = "pass",
                               warning = FALSE) {
  get_prices(
    symbols,
    start_date = Sys.Date(),
    end_date = Sys.Date(),
    error_handling = error_handling,
    warning = warning
  ) %>%
    mutate(as_of = Sys.time())
}
