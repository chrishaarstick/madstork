
# Update Portfolio Functionality ------------------------------------------

#' Updated Porfolio Holding's Market Value
#'
#' Updateds current market price and annual dividends for portfolio holdings.
#' Calculates market value, income and yield
#'
#' @param pobj portfolio object
#' @param prices tibble with symbol prices. requires symbol, and price
#'   columns
#'
#' @return tibble with portfolio's holdings market value
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
update_holdings_market_value <- function(pobj, prices = NULL) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_data_frame(prices, null.ok = TRUE)

  holdings <- get_holdings(pobj)
  symbols <- unique(holdings$symbol)

  if(is.null(prices)) {
    if(length(symbols) > 0) {
      prices <- get_current_prices(symbols = symbols, dividends = TRUE)
    } else {
      prices <- tibble::tibble(last_updated = character(),
                               symbol = character(),
                               price = numeric(),
                               dividend = numeric())
    }
  }
  checkmate::assert_subset(c("symbol", "price", "dividend"), colnames(prices))
  checkmate::assert_subset(symbols, prices$symbol)

  holdings %>%
    dplyr::select(id, symbol, quantity, cost = price, date_added) %>%
    dplyr::inner_join(prices, by = "symbol") %>%
    dplyr::mutate(
      market_value = quantity * price,
      cost_basis = quantity * cost,
      unrealized_gain = quantity * (price - cost),
      annual_income = quantity * dividend,
      yield = dividend / price,
      investments_share = market_value/sum(market_value),
      portfolio_share = market_value/(sum(market_value)+get_cash(pobj))
    ) %>%
    dplyr::select_at(
      c(
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
      )
    )
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
update_market_value <- function(pobj, prices = NULL) {
  checkmate::assert_class(pobj, "portfolio")

  holdings <- get_holdings(pobj)
  holdings_market_value <- update_holdings_market_value(pobj, prices)

  current_market_value <- tibble::tibble(
    last_updated = Sys.time(),
    cash = as.numeric(get_cash(pobj)),
    investments_value = sum(holdings_market_value$market_value),
   # investments_annual_income = sum(holdings_market_value$annual_income),
    loans = as.numeric(0),
    tax_liability = as.numeric(get_tax_liability(pobj))
  ) %>%
    dplyr::mutate(net_value = cash + investments_value - loans - tax_liability)

  pobj$holdings_market_value <- holdings_market_value
  pobj$market_value <- rbind(pobj$market_value, current_market_value)

  pobj
}


#' Get New Portfolio Dividends
#'
#' Function to return all new holding dividends not yet recieved.
#'
#' @param pobj portfolio object
#'
#' @return tibble with new dividends. arrange by symbol and date with
#'   holding quantity, dividend and total payment amount
#'
#' @export
get_new_dividends <- function(pobj) {
  checkmate::assert_class(pobj, "portfolio")

  div_holdings <- pobj$holdings %>%
    dplyr::mutate_at("symbol", as.character) %>%
    dplyr::left_join(
      pobj$income %>%
        to_tibble() %>%
        dplyr::filter(type == "dividend") %>%
        dplyr::group_by(symbol) %>%
        dplyr::summarise(last_income_date = max(transaction_date)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate_at("symbol", as.character),
      by = "symbol"
    ) %>%
    dplyr::mutate(date = dplyr::case_when(is.na(last_income_date) ~ transaction_date,
                                          transaction_date > last_income_date ~ transaction_date,
                                          TRUE ~ last_income_date + 1)) %>%
    dplyr::filter(date <= Sys.Date()) %>%
    dplyr::select(id, date, symbol, quantity)

  div_holdings %>%
    split(.$id) %>%
    purrr::map_dfr(~get_dividends(as.character(.$symbol), .$date), .id = "id") %>%
    dplyr::mutate_at("id", as.numeric) %>%
    dplyr::inner_join(
      dplyr::select(div_holdings, id, quantity),
      by = "id") %>%
    dplyr::mutate(amount = dividend * quantity) %>%
    dplyr::filter(dividend > 0) %>%
    dplyr::arrange(symbol, date) %>%
    dplyr::select(date, symbol, dividend, quantity, amount)
}



#' Update Dividend Income
#'
#' Auto dividend income function. Function searches for last dividend payment
#' for each symbol holding, gets all subsequent dividends and updates the
#' portfolio
#'
#' Function to be used in daily portfolio-update script to remove need for
#' manual dividend updating
#'
#' @param pobj portfolio object
#'
#' @return updated portfolio object
#' @export
update_dividend_income <- function(pobj) {
  checkmate::assert_class(pobj, "portfolio")

  # get new dividends
 new_dividends <- get_new_dividends(pobj)

 # recieve dividends
 pobj <- recieve_dividends(pobj, new_dividends)

 pobj
}

