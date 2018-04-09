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
      dplyr::group_by(symbol, floor = floor_date(date, unit = grain)) %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-floor)
  }else{
    if(! is.null(prices)) checkmate::assert_subset(c("date", "symbol", "price"), colnames(prices))
  }

  if(is.null(returns)) {
    returns <- prices %>%
      dplyr::group_by(symbol) %>%
      dplyr::mutate(return = price/lag(price, 1) - 1) %>%
      dplyr::filter(! is.na(return)) %>%
      dplyr::select(-price) %>%
      dplyr::ungroup()
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


#' Add Calculated Sample Mu to Estimates
#'
#' Function adds mu estimates. Allows for configurable function applied to
#' sample returns
#'
#' @param eobj estimates object
#' @param fun summarise function. default is mean
#' @param ... additional arguments to pass to function
#'
#' @return updated estimates object
#' @export
add_sample_mu <- function(eobj, fun = "mean", ...){
  checkmate::assert_class(eobj, "estimates")
  .fun <- match.fun(fun)
  checkmate::assert_function(.fun)

  eobj$mu <- get_returns(eobj) %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise_at("return", funs(.fun), ...)

  eobj
}


#' Add Mu Estimates
#'
#' Function adds pre-calculated mu estimates. Mu estimates should contain
#' estimates for all symbols in estimate object
#'
#' @param eobj estimates object
#' @param mu data.frame with mu estimates. requires symbol and return column
#'   names
#'
#' @return upated estimates object
#' @export
add_mu <- function(eobj, mu) {
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_data_frame(mu)
  checkmate::assert_subset(c("symbol", "return"), colnames(mu))
  checkmate::assert_set_equal(eobj$symbols, unique(mu$symbol))

  eobj$mu <- mu
  eobj
}



#' Get Mu Estimates from Estiamates Object
#'
#' @param eobj estimates object
get_mu <- function(eobj) {
  checkmate::assert_class(eobj, "estimates")
  eobj$mu
}



#' Add Calculated Sample Sigma to Estimates
#'
#' Function adds mu estimates. Allows for configurable function applied to
#' sample returns
#'
#' @param eobj estimates object
#' @param fun summarise function. default is cov
#' @param ... additional arguments to pass to function
#'
#' @return updated estimates object
#' @export
add_sample_sigma <- function(eobj, fun = "cov", ...){
  checkmate::assert_class(eobj, "estimates")
  .fun <- match.fun(fun)
  checkmate::assert_function(.fun)

  eobj$sigma <- get_returns(eobj) %>%
    tidyr::spread(key = symbol, value = return) %>%
    dplyr::select(-date) %>%
    dplyr::do(as.data.frame(.fun(., ...))) %>%
    as.matrix()

  eobj
}

#' Add Sigma Estimates
#'
#' Function to add pre-calculated sigma matrix to estimates
#'
#' @param eobj estimates object
#' @param sigma sigma estimate matrix
#' @export
add_sigma <- function(eobj, sigma) {
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_class(sigma, "matrix")
  checkmate::assert_matrix(sigma,
                           any.missing = FALSE,
                           nrow = length(eobj$symbols),
                           ncols = length(eobj$symbols))
  eobj$sigma <- sigma
}


#' Get Sigma Matrix
#'
#' Returns Sigma Estimates as a Matrix
#' @param eobj estimates object
#' @export
get_sigma <- function(eobj) {
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_matrix(eobj$sigma)

  eobj$sigma[eobj$symbols, eobj$symbols]
}


#' Get Sigma data.frame
#'
#' Returns Sigma Estimates as a long data.frame
#' @param eobj estimates object
#' @export
get_sigma_df <- function(eobj) {
  checkmate::assert_class(eobj, "estimates")
  checkmate::assert_class(eobj$sigma, "matrix")

  eobj$sigma %>%
    as.data.frame() %>%
    dplyr::mutate(symbol1 = rownames(eobj$sigma)) %>%
    tidyr::gather(key = symbol2, value = sigma, -symbol1)
}
