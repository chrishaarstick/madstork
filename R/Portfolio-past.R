
# Portfolio Past Market Value Functionality -------------------------------



#' Calculate Portfolio Past Market Value
#'
#' Function to calculate the complete portfolio historical market value.
#' Calculates the past investment value, cash balance, and tax liability
#' seperately
#'
#' @param pobj portfolio object
#'
#' @return updated portfolio object with all dividends processed and added to
#'   portfolio income
#' @export
past_market_value <- function(pobj) {

  checkmate::assert_class(pobj, "portfolio")

  # Add past dividends
  pobj <- past_dividend_income(pobj)

  # Calculate past holding market value
  past_holdings_value <- get_past_holdings_market_value(pobj)

  # Calculate past investments value
  past_investment_value <- past_holdings_value %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(investments_value = sum(market_value))

  # Calculate past cash
  past_cash <- get_past_cash(pobj)

  # Calculate past tax liability
  past_tax_liability <- get_past_tax_liability(pobj)

  # Calculate past portfolio market value
  pobj$market_value <- past_investment_value %>%
    dplyr::full_join(past_cash, by = "date") %>%
    dplyr::full_join(past_tax_liability, by = "date") %>%
    dplyr::arrange(date) %>%
    tidyr::fill(investments_value, cash, tax_liability, .direction = "down") %>%
    tidyr::replace_na(list(investments_value = 0, tax_liability = 0)) %>%
    dplyr::mutate(last_updated = Sys.time(),
                  loans = 0,
                  net_value = cash + investments_value - tax_liability) %>%
    dplyr::select(last_updated, date, cash, investments_value, loans, tax_liability, net_value)

  pobj$date_initialized <- min(pobj$market_value$date)

  pobj
}



#' Get Portfolio Past Holdings
#'
#' Function to extract the full historical portfolio holdings. Returns expanded
#' date sequence of symbol holdings
#'
#' @param pobj portfolio object
#'
#' @return tibble with historical holdings. arrange by symbol and date with
#'   holding quantity
#' @export
get_past_holdings <- function(pobj) {

  checkmate::assert_class(pobj, "portfolio")

  holdings_dates <- pobj$trades %>%
    dplyr::group_by(symbol) %>%
    tidyr::expand(date = seq(from = min(transaction_date), to = Sys.Date(), by = "day"))  %>%
    dplyr::ungroup()

  pobj$trades %>%
    dplyr::mutate(type = factor(type, levels = c("buy", "sell", "transfer_in", "transfer_out"))) %>%
    tidyr::spread(key = "type", value = "quantity", fill = 0, drop = FALSE) %>%
    dplyr::filter(buy != 0 | sell != 0 | transfer_in != 0 | transfer_out != 0) %>%
    dplyr::rename(date = transaction_date) %>%
    dplyr::full_join(holdings_dates, by = c("date", "symbol")) %>%
    tidyr::replace_na(list(buy = 0, sell = 0, transfer_in = 0, transfer_out = 0)) %>%
    dplyr::group_by(symbol) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate_at(c("buy", "sell", "transfer_in", "transfer_out"), cumsum) %>%
    dplyr::mutate(quantity = buy + transfer_in - sell - transfer_out) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(symbol, date) %>%
    dplyr::filter(quantity != 0) %>%
    dplyr::select(date, symbol, quantity)
}


#' Get Portfolio Past Holdings Market Value
#'
#' Function to extract the full historical portfolio holdings and calculate
#' market value. Returns expanded date sequence of symbol holdings with price and market value
#'
#' @param pobj portfolio object
#'
#' @return tibble with historical holdings. arrange by symbol and date with
#'   holding quantity, price and value
#' @export
get_past_holdings_market_value <- function(pobj) {

  checkmate::assert_class(pobj, "portfolio")

  past_holdings <- get_past_holdings(pobj)

  past_holdings %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise_at("date", funs(min, max)) %>%
    split(.$symbol) %>%
    purrr::map_df(~get_prices(symbols = .x$symbol, start_date = .x$min, end_date = .x$max)) %>%
    dplyr::inner_join(past_holdings, by = c("date", "symbol")) %>%
    dplyr::mutate(market_value = price * quantity)
}


#' Get Portfolio Past Dividends
#'
#' Function to extract the full historical portfolio dividends.
#'
#' @param pobj portfolio object
#'
#' @return tibble with historical dividends. arrange by symbol and date with
#'   holding quantity, dividend and total payment amount
#'
#' @export
get_past_dividends <- function(pobj) {

  checkmate::assert_class(pobj, "portfolio")

  past_holdings <- get_past_holdings(pobj)

  past_holdings %>%
    dplyr::group_by(symbol, quantity) %>%
    dplyr::summarise_at("date", funs(min, max)) %>%
    split(list(.$symbol, .$quantity), drop=TRUE) %>%
    purrr::map_df(~get_dividends(symbols = .x$symbol, start_date = .x$min, end_date = .x$max))  %>%
    dplyr::inner_join(past_holdings, by = c("date", "symbol")) %>%
    dplyr::mutate(amount = dividend * quantity) %>%
    dplyr::arrange(symbol, date)
}



#' Past Dividend Income
#'
#' Function to process all past dividend income from historical holdings.
#' Functions removes any prior dividend income, gets all past dividends and then
#' updates portfolio
#'
#' @param pobj portfolio object
#'
#' @return updated portfolio object with all dividends processed and added to
#'   portfolio income
#' @export
past_dividend_income <- function(pobj) {

  checkmate::assert_class(pobj, "portfolio")

  # remove any prior dividend income & activity
  pobj$income <- dplyr::filter(pobj$income, type != "dividend")
  pobj$activity <- dplyr::filter(pobj$activity, type != "dividend")

  # update income ids
  n <- nrow(pobj$income)
  if(n > 0) pobj$income$id <- 1:n

  # update activity ids
  n <- nrow(pobj$activity)
  if(n > 0) pobj$activity$id <- 1:n

  # get all past dividends
  past_dividends <- get_past_dividends(pobj)

  # recieve dividends
  pobj <- recieve_dividends(pobj, past_dividends)

  pobj
}


#' Get Portfolio Past Cash Balance
#'
#' Function to extract the full historical portfolio cash balance
#'
#' @param pobj portfolio object
#'
#' @return tibble with historical cash balance
#' @export
get_past_cash <- function(pobj) {

  checkmate::assert_class(pobj, "portfolio")

  past_holding_dates <- get_past_holdings(pobj) %>%
    distinct(date)

  pobj$activity %>%
    mutate(change = case_when(type %in% c("deposit", "interest", "dividend") ~ amount,
                              TRUE ~ -amount)) %>%
    select(date = transaction_date, change) %>%
    group_by(date) %>%
    summarise_at('change', sum) %>%
    full_join(past_holding_dates, by = "date") %>%
    replace_na(list(change = 0)) %>%
    arrange(date) %>%
    mutate(cash = cumsum(change)) %>%
    select(date, cash)
}



#' Get Potfolio Past Tax Liability
#'
#' Function to extact and calculate tax liability for past gains. Function
#' resets tax liability for each calander year
#'
#' @param pobj portfolio object
#'
#' @return tibble with historical tax liability amount
#' @export
get_past_tax_liability <- function(pobj) {

  checkmate::assert_class(pobj, "portfolio")

  past_holding_dates <- get_past_holdings(pobj) %>%
    distinct(date)

  pobj$gains %>%
    arrange(sale_date) %>%
    select(date = sale_date, tax_liability) %>%
    group_by(date) %>%
    summarise_at('tax_liability', sum) %>%
    right_join(past_holding_dates, by = "date") %>%
    replace_na(list(tax_liability = 0)) %>%
    group_by(year = year(date)) %>%
    mutate_at('tax_liability', cumsum) %>%
    ungroup() %>%
    select(date, tax_liability)
}
