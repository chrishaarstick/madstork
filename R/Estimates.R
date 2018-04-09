#' Estimates Class Constructer function
#'
#' Creates an estimate object
#'
#' @param symbols vector of security symbols
#' @param start_date start date for historical prices
#' @param end_date end date for historical prices
#' @param grain time grain of returns. Can be either day, week, month, year
#' @param periods number of time periods
#' @param prices optional input for security prices
#' @param returns optional input for security returns
#' @param mu optional input for mean estimates
#' @param sigma optional input for sigma estimate
#'
#' @return estimates object
#' @export
new_estimates <- function(symbols,
                          start_date,
                          end_date,
                          grain,
                          periods,
                          prices,
                          returns,
                          mu,
                          sigma
){

  checkmate::assert_character(symbols)
  checkmate::assert_date(start_date)
  checkmate::assert_date(end_date)
  checkmate::assert_choice(grain, c("day", "week", "month", "year"))
  checkmate::assert_number(periods, lower = 1)
  checkmate::assert_data_frame(prices, null.ok = TRUE)
  if(! is.null(prices)) checkmate::assert_subset(c("date", "symbol", "price"), colnames(prices))
  checkmate::assert_data_frame(returns, null.ok = TRUE)
  if(! is.null(returns)) checkmate::assert_subset( c("date", "symbol"), colnames(returns))
  checkmate::assert_data_frame(mu, null.ok = TRUE)
  if(! is.null(mu)) checkmate::assert_subset(c("symbol", "mu"), colnames(mu))
  checkmate::assert_matrix(sigma, null.ok = TRUE, any.missing = FALSE,
                           nrow = length(symbols), ncols = length(symbols))

  structure(
    list(
      symbols = symbols,
      sample_start_date = start_date,
      sample_end_date = end_date,
      grain = grain,
      estimate_start_date = end_date + 1,
      estimate_end_date = end_date + match.fun(paste0(grain, "s"))(periods),
      prices = prices,
      returns = returns,
      mu = mu,
      sigma = sigma
    ),
    class = "estimates"
  )
}

#' Estimate Helper
#'
#' Function to create a new estimates object. If prices not supplied, get_prices
#' function called and returns calculated. Alternatively, returns and prices can
#' be supplied.
#' @inheritParams new_estimates
#' @export
estimates <- function(symbols,
                      start_date,
                      end_date,
                      grain,
                      periods = 1,
                      prices = NULL,
                      returns = NULL) {

  if(is.null(prices) & (! is.null(returns))) {
    stop("Returns provided but not prices. Please provide prices")
  }


  if(is.null(prices)) {
    prices <- get_prices(symbols, start_date = start_date, end_date = end_date) %>%
      group_by(symbol, floor = floor_date(date, unit = grain)) %>%
      filter(date == max(date)) %>%
      ungroup() %>%
      select(-floor)
  }else{
    if(! is.null(prices)) checkmate::assert_subset(c("date", "symbol", "price"), colnames(prices))
  }

  if(is.null(returns)) {
    returns <- prices %>%
      group_by(symbol) %>%
      mutate(return = price/lag(price, 1) - 1) %>%
      filter(! is.na(return)) %>%
      select(-price) %>%
      ungroup()
  }

  new_estimates(
    symbols,
    start_date,
    end_date,
    grain,
    periods,
    prices = prices,
    returns = returns,
    mu = NULL,
    sigma = NULL
  )
}


#' Get Returns from Estiamates Object
#'
#' @param eobj estimates object
get_returns <- function(eobj) {
  checkmate::assert_class(eobj, "estimates")
  rets <- eobj$returns
  checkmate::assert_subset(c("date", "symbol", "return"), colnames(rets))
  rets
}


#' Add Sample Mu to Estimates
#'
#' Function adds mu estimates. Allows for configurable function applied to
#' sample returns
#'
#' @param eobj estimates object
#' @param fun summarise function. default is mean
#' @param ... additional arguments to pass to summarising function
#'
#' @return updated estimates object
#' @export
#'
#' @examples
add_sample_mu <- function(eobj, fun = "mean", ...){
  checkmate::assert_class(eobj, "estimates")
  .fun <- match.fun(fun)
  checkmate::assert_function(.fun)

  eobj$mu <- get_returns(eobj) %>%
    group_by(symbol) %>%
    summarise_at("return", funs(.fun), ...)

  eobj
}

