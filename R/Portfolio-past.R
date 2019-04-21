
#' Initialize Portfolio from Activity List
#'
#' Initialize a portfolio from a list of prior activity. Function process all
#' activity and updates the historical portfolio market value
#'
#' @param pobj portfolio object
#' @param activity list of portfolio activity. Examples trade, income and other activity
#'
#' @return portfolio with activity added and past market value calculated
#' @export
intialize_portfolio <- function(pobj, activity) {

  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(activity, "list")

  trade_activity <- keep(activity, function(x) class(x) == "trade") %>%
    map_df(., to_tibble)

  holdings_dates <- trade_activity %>%
    dplyr::group_by(symbol) %>%
    tidyr::expand(date = seq(from = min(transaction_date), to = Sys.Date(), by = "day"))  %>%
    dplyr::ungroup()

  past_holdings <- trade_activity %>%
    dplyr::mutate(flow = dplyr::case_when(type %in% c("buy", "transfer_in") ~ quantity,
                                          type %in% c("sell", "transfer_out") ~ -quantity,
                                          TRUE ~ 0)) %>%
    dplyr::select(symbol, date = transaction_date, flow) %>%
    dplyr::right_join(holdings_dates, by = c("symbol", "date")) %>%
    tidyr::replace_na(list(flow = 0)) %>%
    dplyr::arrange(date) %>%
    dplyr::group_by(symbol) %>%
    dplyr::mutate(quantity = cumsum(flow)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(symbol, date) %>%
    dplyr::select(date, symbol, quantity)

  past_holdings_market_value <- past_holdings %>%
    dplyr::group_by(symbol) %>%
    dplyr::summarise_at("date", funs(min, max)) %>%
    split(.$symbol) %>%
    purrr::map_df(~get_prices(symbols = .x$symbol, start_date = .x$min, end_date = .x$max)) %>%
    dplyr::inner_join(past_holdings, by = c("date", "symbol")) %>%
    dplyr::mutate(market_value = price * quantity) %>%
    dplyr::group_by(symbol) %>%
    tidyr::fill(price, quantity, market_value, .direction = "down") %>%
    dplyr::ungroup()

  past_holdings_income <- past_holdings %>%
    dplyr::group_by(symbol, quantity) %>%
    dplyr::summarise_at("date", funs(min, max)) %>%
    split(list(.$symbol, .$quantity), drop=TRUE) %>%
    purrr::map_df(~get_dividends(symbols = .x$symbol, start_date = .x$min, end_date = .x$max))  %>%
    dplyr::inner_join(past_holdings, by = c("date", "symbol")) %>%
    dplyr::mutate(amount = dividend * quantity) %>%
    dplyr::arrange(symbol, date)

  past_investment_value <- past_holdings_market_value %>%
    tidyr::complete(crossing(date, symbol)) %>%
    dplyr::group_by(symbol) %>%
    tidyr::fill(price, quantity, market_value, .direction = "down") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(investments_value = sum(market_value, na.rm = TRUE))

  first_activity_date <- map_dfc(activity, "transaction_date") %>% t() %>% min(.) %>% as.Date()
  activity_range <- seq(from=first_activity_date, to=Sys.Date(), by='day')

  for(index in 1:length(activity_range)) {
    day <- as.Date(activity_range[index])
    day_activity <- keep(activity, function(x) x$transaction_date == day)

    if(length(day_activity) > 0) {
      for(i in seq_along(day_activity)) {
        pobj <- process(day_activity[[i]], pobj)
      }
    }

    day_income <- dplyr::filter(past_holdings_income, date == day)
    if(nrow(day_income) > 0){
      pobj <- recieve_dividends(pobj, day_income)
    }

    day_investment_value <- past_investment_value %>%
      dplyr::filter(date == day) %>%
      pull(investments_value)

    portfolio_market_value <- tibble(
      last_updated = Sys.time(),
      date = day,
      cash = get_cash(pobj),
      investments_value = day_investment_value,
      loans = 0,
      tax_liability = get_tax_liability(pobj),
      net_value = cash + investments_value - loans - tax_liability
    )

    if(nrow(pobj$market_value) == 0) {
      pobj$market_value <- portfolio_market_value
    } else {
      pobj$market_value <- bind_rows(pobj$market_value, portfolio_market_value)
    }

    pobj
  }

  pobj$holdings_market_value <- update_holdings_market_value(pobj)
  pobj
}
