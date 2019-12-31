
# Trade Class -------------------------------------------------------------

#' Trade Object Constructor function
#'
#' Function to create an object of trade class
#'
#' @param type trade type. string type.
#' @param date date of transaction. Date type
#' @param symbol symbol ticker. character type
#' @param quantity number of shares. numeric type
#' @param price price of shares. numeric type
#' @param amount trade dollar amount. numeric type
#' @param desc trade description. string type
#' @param trans_cost transaction cost (dollars per share)
#'
#' @return object of class activity
new_trade <- function(type,
                      date,
                      symbol,
                      quantity,
                      price,
                      amount,
                      desc,
                      trans_cost) {
  stopifnot(is.character(type))
  stopifnot(lubridate::is.Date(date))
  stopifnot(is.character(symbol))
  stopifnot(is.numeric(quantity))
  stopifnot(is.numeric(price))
  stopifnot(is.numeric(amount))
  stopifnot(is.character(desc))
  stopifnot(is.numeric(trans_cost))

  structure(
    list(
      date_added = Sys.Date(),
      transaction_date = date,
      type = type,
      symbol = symbol,
      quantity = quantity,
      price = price,
      amount = amount,
      desc = desc,
      trans_cost = trans_cost
    ),
    class = "trade"
  )
}


#' Trade Object Validation function
#'
#' Function to validate trade input values
#'
#' @param x object of class trade
#'
#' @return valid trade object
validate_trade <- function(x) {
  if (x$amount < 0) {
    stop("negative amount not allowed",
         .call = F)
  }
  if (x$quantity < 0) {
    stop("negative quantity not allowed",
         .call = F)
  }
  if (x$price < 0) {
    stop("negative price not valid",
         .call = F)
  }
  if (!x$type %in% c("buy", "sell", "transfer_in", "transfer_out")) {
    stop("trade type not supported. only buy, sell, transfer_in and transfer_out types allowed",
         .call = F)
  }
  if (nchar(x$symbol) > 4) {
    message("double check symbol ticker - character length greater than 4")
  }
  if (x$trans_cost < 0) {
    message("negative transaction costs not allowed")
  }
  x
}


#' Function to convert trade to tibble
#'
#' @rdname to_tibble
#' @export
to_tibble.trade <- function(x, ...) {
  tibble::tibble(
    date_added = as.Date(x$date_added),
    transaction_date = as.Date(x$transaction_date),
    type = as.character(x$type),
    symbol = as.character(x$symbol),
    quantity = as.numeric(x$quantity),
    price = as.numeric(x$price),
    amount = as.numeric(x$amount),
    desc = as.character(x$desc),
    trans_cost = as.numeric(x$trans_cost)
  )
}


# Internal Trade Tibble Helper
empty_trades_df <- function() {
  tibble::tibble(
    date_added = as.Date(character()),
    transaction_date = as.Date(character()),
    type = character(),
    symbol = character(),
    quantity = numeric(),
    price = numeric(),
    amount = numeric(),
    desc = character(),
    trans_cost = numeric(),
    id = numeric()
  )
}



# Buys --------------------------------------------------------------------


#' Create Buy Trade Helper function
#'
#' Creates a buy type of trade object
#'
#' @inheritParams new_trade
#'
#' @return buy type trade object
#' @export
#'
#' @examples
#' buy(date = Sys.Date(), symbol = "SPY", quantity = 10, price = 100)
buy <- function(date,
                symbol,
                quantity,
                price,
                desc = "",
                trans_cost = .05) {
  validate_trade(new_trade(
    type = "buy",
    date,
    symbol,
    quantity,
    price,
    amount = price * quantity,
    desc,
    trans_cost
  ))
}


#' Make Buy Trade Execution function
#'
#' Function executions a buy trade
#'
#' subtracts trade amount from cash balance, updates portfolio internal trade
#' history and holdings
#'
#' @param pobj portfolio object
#' @inheritParams buy
#' @importFrom magrittr %>%
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port") %>%
#' make_deposit(Sys.Date(), amount = 2000) %>%
#' make_buy(symbol = "SPY", quantity = 10, price = 100)
make_buy <- function(pobj,
                     date = Sys.Date(),
                     symbol,
                     quantity,
                     price,
                     desc = "",
                     trans_cost = .05) {
  checkmate::assert_class(pobj, "portfolio")

  trade <- buy(date, symbol, quantity, price, desc, trans_cost)

  nid <- ifelse(nrow(pobj$trades) == 0, 1, max(pobj$trades$id) + 1)
  trade_df <- to_tibble(trade) %>%
    dplyr::mutate(id = nid)

  if (pobj$cash < trade$amount) {
    stop("Trade amount more than cash available. Insufficient cash to make buy trade",
         .call = FALSE)
  }


  pobj <- pobj %>%
    make_withdraw(date,
                  amount = trade$amount,
                  desc = paste("trade_id:", trade_df$id)) %>%
    incur_fee(
      date,
      amount = trade$quantity * trans_cost,
      desc = paste("trade_id:", trade_df$id)
    )

  pobj$trades <- dplyr::bind_rows(pobj$trades, trade_df)
  pobj$holdings <- dplyr::bind_rows(
    pobj$holdings,
    trade_df %>%
      dplyr::select(
        id,
        date_added,
        transaction_date,
        symbol,
        quantity,
        price,
        desc
      )
  )

  pobj
}




# Sells -------------------------------------------------------------------



#' Create Sell Trade Helper function
#'
#' Creates a sell type of trade object
#'
#' @inheritParams new_trade
#'
#' @return sell type trade object
#' @export
#'
#' @examples
#' sell(date = Sys.Date(), symbol = "SPY", quantity = 10, price = 100)
sell <- function(date,
                 symbol,
                 quantity,
                 price,
                 desc = "",
                 trans_cost = .05) {
  validate_trade(new_trade(
    type = "sell",
    date,
    symbol,
    quantity,
    price,
    amount = price * quantity,
    desc,
    trans_cost
  ))
}


#' Make Sell Trade Execution function
#'
#' Function executions a sell trade
#'
#' add trade amount to cash balance, updates portfolio internal trade
#' history and holdings
#'
#' @param pobj portfolio object
#' @param id holding id to sell
#' @param trans_cost transaction cost (dollars per share)
#' @inheritParams sell
#' @importFrom magrittr %>%
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#' library(tidyverse)
#'  p1 <- portfolio("new_port") %>%
#'        make_deposit(Sys.Date(), amount = 2000) %>%
#'        make_buy(Sys.Date()-1, symbol = "SPY", quantity = 10, price = 100) %>%
#'        make_sell(id = 1, quantity = 5, price = 105)
make_sell <- function(pobj,
                      id,
                      date = Sys.Date(),
                      quantity,
                      price,
                      desc = "",
                      trans_cost = .05) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_number(id)
  holding <- pobj %>% get_holding(id)
  Id <- id

  if (nrow(holding) == 0) {
    stop("No holdings returned. Check for correct Trade ID",
         .call = FALSE)
  }

  symbol <- as.character(holding$symbol)
  trade <- sell(date, symbol, quantity, price, desc, trans_cost)

  if (trade$quantity > holding$quantity) {
    stop("Trade quantity greater than holding amount. No short trades allowed.",
         .call = FALSE)
  }

  nid <- ifelse(nrow(pobj$trades) == 0, 1, max(pobj$trades$id) + 1)
  trade_df <- to_tibble(trade) %>%
    dplyr::mutate(id = nid)

  new_holding <- holding
  new_holding$quantity <- new_holding$quantity - trade$quantity
  if(new_holding$quantity == 0) new_holding <- NULL

  gain <- gains(trade, holding) %>%
    add_tax_liability() %>%
    dplyr::mutate(id = nrow(pobj$gains) + 1)

  pobj <- pobj %>%
    make_deposit(date,
                 amount = trade$amount,
                 desc = paste("trade_id:", trade_df$id)) %>%
    incur_fee(
      date,
      amount = trade$quantity * trans_cost,
      desc = paste("trade_id:", trade_df$id)
    )

  pobj$tax_liability <- pobj$tax_liability + gain$tax_liability
  pobj$trades <- dplyr::bind_rows(pobj$trades, trade_df)
  pobj$holdings <- dplyr::bind_rows(dplyr::filter(pobj$holdings, id != Id),
                         new_holding) %>%
    dplyr::arrange(id)
  pobj$gains <- dplyr::bind_rows(pobj$gains, gain)

  pobj
}



#' Get Sell Trades
#'
#' Returns all possible sell trades from a portfolio object
#'
#' Checks current holdings and generates sell tickets based on symbol, trade
#' amount and lot_size
#'
#' @param pobj portfolio object
#' @param symbols vector of holding symbols to filter sell tickets by
#' @param amount trade amount
#' @param partial logical option to allow for partial trade tickets with an
#'   quantity less than the quantity argument. default is TRUE
#'
#' @return data.frame with possible sell trades
#' @export
get_sell_ids <- function(pobj,
                         date,
                         symbol,
                         quantity) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_character(symbol)
  checkmate::assert_number(quantity, lower = 0)

  .quantity <- quantity
  .symbol <- symbol

  holdings <- get_holdings(pobj) %>%
    dplyr::filter(symbol == .symbol) %>%
    dplyr::mutate(lt_gain = dplyr::case_when(
      transaction_date <= date - lubridate::years(1) ~ 1,
      TRUE ~ 0)) %>%
    dplyr::arrange(-lt_gain, price) %>%
    dplyr::select(id, symbol, quantity)

  sells <- tibble::tibble(
    id = numeric(),
    symbol = character(),
    quantity = numeric())

  for(i in 1:nrow(holdings)) {

    if(.quantity > 0) {
      sell <- holdings %>%
        dplyr::slice(i) %>%
        dplyr::select(id, symbol, quantity) %>%
        dplyr::mutate(quantity = dplyr::case_when(.quantity <= quantity ~ .quantity,
                                                  TRUE ~ quantity))
      sells <- rbind(sells, sell)
      .quantity <- .quantity - sell$quantity
    }
  }

  sells
}




# Transfers ---------------------------------------------------------------



#' Create Transfer Trade Helper
#'
#' Create a valid transfer object of class Trade
#'
#' @inheritParams new_trade
#' @return valid trade object
#' @export
#'
#' @examples
#' library(tidyverse)
#' transfer(date = Sys.Date(), symbol = "SPY", quantity = 10, price = 100, type="out")
transfer <- function(date,
                     symbol,
                     quantity,
                     price,
                     type = "",
                     desc = "") {
  validate_trade(
    new_trade(
      type = paste("transfer", type, sep="_"),
      date,
      symbol,
      quantity,
      price,
      amount = price * quantity,
      desc,
      trans_cost = 0
    )
  )
}



#' Transfer Out Holdings function
#'
#' Function removes holding from portfolio without a cash transaction
#'
#' @inheritParams make_sell
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#' library(tidyverse)
#'  p1 <- portfolio("new_port") %>%
#'        make_deposit(Sys.Date(), amount = 2000) %>%
#'        make_buy(Sys.Date()-1, symbol = "SPY", quantity = 10, price = 100) %>%
#'        transfer_out(id = 1, quantity = 10)
transfer_out <- function(pobj,
                         id,
                         date = Sys.Date(),
                         quantity,
                         desc = "") {
  stopifnot(class(pobj) == "portfolio")
  stopifnot(class(id) == "numeric")
  holding <- get_holding(pobj, id)
  Id <- id

  if (nrow(holding) == 0) {
    stop("No holdings returned. Check for correct Trade ID",
         .call = FALSE)
  }

  symbol <- as.character(holding$symbol)
  trade <- transfer(date, symbol, quantity, holding$price, type = "out", desc)
  trade_df <- to_tibble(trade) %>%
    dplyr::mutate(id = max(pobj$trades$id, 0) + 1)
  pobj$trades <- dplyr::bind_rows(pobj$trades, trade_df)

  if(quantity == holding$quantity) {
    pobj$holdings <- pobj$holdings %>%
      dplyr::filter(id != Id) %>%
      dplyr::arrange(id)
  } else {
    pobj$holdings[pobj$holdings$id == Id, quantity] <- holding$quantity - quantity
  }

  pobj
}



#' Transfer In Holdings function
#'
#' Function adds holding from portfolio without a cash transaction
#'
#' @inheritParams make_buy
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#' library(tidyverse)
#'  p1 <- portfolio("new_port") %>%
#'        make_deposit(Sys.Date(), amount = 2000) %>%
#'        transfer_in(Sys.Date()-1, symbol = "SPY", quantity = 10, price = 100)
transfer_in <- function(pobj,
                        date = Sys.Date(),
                        symbol,
                        quantity,
                        price,
                        desc = "") {
  stopifnot(class(pobj) == "portfolio")
  trade <-transfer(date, symbol, quantity, price, type = "in", desc)
  trade_df <- to_tibble(trade) %>%
    dplyr::mutate(id = max(pobj$trades$id, 0) + 1)

  pobj$trades <- dplyr::bind_rows(pobj$trades, trade_df)
  pobj$holdings <- dplyr::bind_rows(
    pobj$holdings,
    trade_df %>%
      dplyr::select(
        id,
        date_added,
        transaction_date,
        symbol,
        quantity,
        price,
        desc
      )
  )
  pobj
}


# Methods -----------------------------------------------------------------


#' @param trans_cost transaction costs per share. default is 5cents ~ 0.05
#'
#' @rdname process
#' @export
process.trade <- function(obj, pobj, trans_cost = 0.05, ...){
  checkmate::assert_class(pobj, "portfolio")

  if(obj$type == "buy") {

    pobj <- make_buy(pobj,
                     date = obj$transaction_date,
                     symbol = obj$symbol,
                     quantity = obj$quantity,
                     price = obj$price,
                     desc = obj$desc,
                     trans_cost = trans_cost)

  } else if (obj$type == "sell") {

    sells <- get_sell_ids(pobj = pobj,
                          date = obj$transaction_date,
                          symbol = obj$symbol,
                          quantity = obj$quantity)

    for (.id in sells$id) {

      s1 <- filter(sells, id == .id)
      pobj <- make_sell(pobj,
                        id = s1$id,
                        date = obj$transaction_date,
                        quantity = s1$quantity,
                        price = obj$price,
                        desc = obj$desc,
                        trans_cost = trans_cost)
    }

  } else if (obj$type == "transfer_in") {

    pobj <- transfer_in(pobj,
                        date = obj$transaction_date,
                        symbol = obj$symbol,
                        quantity = obj$quantity,
                        price = obj$price,
                        desc = obj$desc)

  } else if (obj$type == "transfer_out") {

    pobj <- transfer_in(pobj,
                        date = obj$transaction_date,
                        symbol = obj$symbol,
                        quantity = obj$quantity,
                        price = obj$price,
                        desc = obj$desc)
  }

  pobj
}
