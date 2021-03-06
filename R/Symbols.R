


#' Get Securities Price and Volume Data
#'
#' Wrapper function to get historical security prices for 1 or more symbols.
#' Function uses getSymbol function from quantmod package. Returns Open, Close,
#' High, Low, Adj Close, and Volume
#'
#' @param symbols vector of stock symbol characters. can be 1 or more
#' @param start_date starting date for historical prices. default is
#'   '1990-01-01'. Y-m-d format required
#' @param end_date ending date for historical prices. default is current date.
#'   Y-m-d format required
#' @param error_handling option to handle errors within foreach loop. options
#'   are 'pass', 'remove', or 'stop'
#' @param warning logical argument to print getSymbol warnings to console
#'
#' @return tibble with symbol historical prices. schema is date, symbol,
#'   open, close, high, low, adj_close, volume
#' @export
#'
#' @importFrom quantmod Op Cl Hi Lo Ad Vo getSymbols
#' @importFrom foreach foreach %do%
#' @importFrom zoo index
#' @examples
#' \donttest{
#' library(tidyverse)
#' library(quantmod)
#' symbols <- c("spy", "tlt")
#' sym_data <- symbols %>% get_ochlav(.)
#' }
get_ochlav <- function(symbols,
                       start_date = "1990-01-01",
                       end_date = Sys.Date(),
                       error_handling = "pass",
                       warning = FALSE) {
  checkmate::assert_character(symbols)
  checkmate::assert_date(as.Date(start_date))
  checkmate::assert_date(as.Date(end_date))
  checkmate::assert_flag(warning)
  checkmate::assert_choice(error_handling, c("pass", "remove", "stop"))

  foreach(
    sym = toupper(symbols),
    .combine = "rbind",
    .errorhandling = error_handling
  ) %do% {
    symbol <- quantmod::getSymbols(
      sym,
      from = start_date,
      to = end_date,
      warning4.0 = warning,
      yahoo.warning = warning,
      auto.assign = FALSE
    ) %>%
      to_tibble()

    symbol$date <- rownames(symbol)

    symbol %>%
      dplyr::rename_all(dplyr::funs(tolower(gsub(
        paste0(sym, "."), "", .
      )))) %>%
      dplyr::rename(adj_close = adjusted) %>%
      dplyr::mutate(date = as.Date(date), symbol = as.character(sym)) %>%
      dplyr::select(date, symbol, open, close, high, low, adj_close, volume)
  } %>%
    na.omit()
}


#' Get Historical Security Prices
#'
#' Wrapper function around get_ochlav function that returns historical securtity prices
#'
#' Prices are set to adjusted close
#'
#' @inheritParams get_ochlav
#'
#' @return single tibble with symbol price history row bounded in long format
#' @export
#'
#' @examples
#' \donttest{
#' library(tidyverse)
#' library(quantmod)
#' symbols <- c("spy", "tlt")
#' prices <- symbols %>% get_prices(.)
#' }
get_prices <- function(symbols,
                       start_date = "1990-01-01",
                       end_date = Sys.Date(),
                       error_handling = "pass",
                       warning = FALSE) {
  get_ochlav(symbols, start_date, end_date, error_handling, warning) %>%
    dplyr::select(date, symbol, adj_close) %>%
    dplyr::rename(price = adj_close)
}




#' Get Current Security Prices
#'
#' Wrapper function around get_prices function that returns current prices. Adds
#' timestamp of date pulled
#'
#' @inheritParams get_ochlav
#' @param dividends logical option to add current annual dividend amount to
#'   output
#'
#' @return tibble with 1 record per symbol with current adjusted close price
#' @export
#'
#' @examples
#' \donttest{
#' library(tidyverse)
#' library(quantmod)
#' symbols <- c("spy", "tlt")
#' prices <- symbols %>% get_current_prices(.)
#' }
get_current_prices <- function(symbols,
                               dividends = FALSE,
                               error_handling = "pass",
                               warning = FALSE) {
  checkmate::assert_flag(dividends)

  prices <- get_prices(
    symbols,
    start_date = Sys.Date() - 5,
    end_date = Sys.Date(),
    error_handling = error_handling,
    warning = warning
  ) %>%
    dplyr::group_by(symbol) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(last_updated = Sys.time()) %>%
    dplyr::select_at(c("last_updated", "symbol", "price"))

  if(dividends) {
    prices %>%
      dplyr::inner_join(get_annual_dividends(symbols) %>%
                          dplyr::select(symbol, dividend = annual_dividend),
                        by = "symbol")
  }else {
    prices
  }
}



#' Get Security Historical Dividends
#'
#' Wrapper function around quantmod::getDividend function
#'
#' @param symbols vector of stock symbol characters. can be 1 or more
#' @param start_date starting date for historical dividends. default is
#'   '1990-01-01'. Y-m-d format required
#' @param end_date ending date for historical dividends. default is current date.
#'   Y-m-d format required
#'
#' @return tibble with a record for each dividend distrubtion per security
#' @export
#' @importFrom quantmod getDividends
#'
#' @examples
#' \donttest{
#' library(tidyverse)
#' symbols <- c("spy", "tlt")
#' dividends <- symbols %>% get_dividends(., start_date = "2016-01-01")
#' }
get_dividends <- function(symbols,
                          start_date = "1990-01-01",
                          end_date = Sys.Date()) {
  purrr::map_dfr(symbols, ~get_dividend(., start_date, end_date))
}


# Internal get dividend helper function
get_dividend <- function(symbol, start_date, end_date) {
  checkmate::assert_character(symbol)
  checkmate::assert_date(start_date)
  checkmate::assert_date(end_date)

  div <- quantmod::getDividends(symbol, from = start_date, to = end_date)
  div_index <- as.Date(zoo::index(div))

  div <- div[div_index >= start_date]
  div_index <- div_index[div_index >= start_date]

  if(nrow(div) == 0){
    tibble::tibble(date = end_date,
                   symbol = symbol,
                   dividend = 0)
  }else {
    tibble::tibble(date = div_index,
                   symbol = as.character(symbol),
                   dividend = as.numeric(div))
  }
}



#' Get Annualized Security Dividends
#'
#' Wrapper function around get_dividends function that returns annualized
#' dividends based on prior years dividend payments. Annual dividend used to
#' calculate security yield
#'
#' Annual dividend is calculated as the sum of prior years payments. Note if
#' either 5 or 13 payments returned, dividend trims the first payment
#'
#' @inheritParams get_dividends
#'
#' @return data.frame with 1 record per symbol with annual dividend, avg
#'   payment, number of payments per year and last payment date
#' @export
#'
#' @examples
#' \donttest{
#' library(tidyverse)
#' library(quantmod)
#' symbols <- c("spy", "tlt")
#' dividends <- symbols %>% get_annual_dividends(., start_date = "2016-01-01")
#' }
get_annual_dividends <- function(symbols) {
  get_dividends(symbols,
                start_date = Sys.Date() - 365,
                end_date = Sys.Date()) %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise(
      count = n(),
      annual_dividend = ifelse(count == 5 |
                                 count == 13, sum(dividend[-1]), sum(dividend)),
      avg_dividend = mean(dividend),
      annual_payments = ifelse(count == 5 |
                                 count == 13, count - 1, count),
      last_payment = max(date)
    ) %>%
    dplyr::mutate(last_updated = Sys.time()) %>%
    dplyr::ungroup() %>%
    dplyr::select_at(
      c(
        "last_updated",
        "symbol",
        "annual_dividend",
        "avg_dividend",
        "annual_payments",
        "last_payment"
      )
    )
}
