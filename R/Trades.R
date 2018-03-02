








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
#'
#' @return object of class activity
new_trade <- function(type,
                      date,
                      symbol,
                      quantity,
                      price,
                      amount,
                      desc) {
  stopifnot(is.character(type))
  stopifnot(lubridate::is.Date(date))
  stopifnot(is.character(symbol))
  stopifnot(is.numeric(quantity))
  stopifnot(is.numeric(price))
  stopifnot(is.numeric(amount))
  stopifnot(is.character(desc))

  structure(
    list(
      date_added = Sys.Date(),
      transaction_date = date,
      type = type,
      symbol = symbol,
      quantity = quantity,
      price = price,
      amount = amount,
      desc = desc
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
  if (!x$type %in% c("buy", "sell")) {
    stop("trade type not supported. only buy or sell types allowed",
         .call = F)
  }
  if (nchar(x$symbol) > 4) {
    message("double check symbol ticker - character length greater than 4")
  }
  x
}


#'@export
as.data.frame.trade <- function(x) {
  data.frame(
    date_added = x$date_added,
    transaction_date = x$transaction_date,
    type = x$type,
    symbol = x$symbol,
    quantity = x$quantity,
    price = x$price,
    amount = x$amount,
    desc = x$desc
  )
}


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
                desc = "") {
  validate_trade(new_trade(
    type = "buy",
    date,
    symbol,
    quantity,
    price,
    amount = price * quantity,
    desc
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
#' portfolio("new_port", cash = 1000) %>%
#' make_buy(date = Sys.Date(), symbol = "SPY", quantity = 10, price = 100)
make_buy <- function(pobj,
                     date,
                     symbol,
                     quantity,
                     price,
                     desc = "") {
  stopifnot(class(pobj) == "portfolio")
  trade <- buy(date, symbol, quantity, price, desc)
  trade_df <- as.data.frame(trade) %>%
    dplyr::mutate(id = max(pobj$trades$id, 0) + 1)

  if (pobj$cash < trade$amount) {
    stop("Trade amount more than cash available. Insufficient cash to make buy trade",
         .call = FALSE)
  }

  pobj$cash <- pobj$cash - trade$amount
  pobj$trades <- rbind(pobj$trades, trade_df)
  pobj$holdings <- rbind(
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
                 desc = "") {
  validate_trade(new_trade(
    type = "sell",
    date,
    symbol,
    quantity,
    price,
    amount = price * quantity,
    desc
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
#' @param id trade id of holding to sell
#' @inheritParams sell
#' @importFrom magrittr %>%
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#' library(tidyverse)
#'  p1 <- portfolio("new_port", cash=0) %>%
#'        make_deposit(Sys.Date(), amount = 1000) %>%
#'        make_buy(Sys.Date()-1, symbol = "SPY", quantity = 10, price = 100) %>%
#'        make_sell(id = 1, date = Sys.Date(), symbol = "SPY", quantity = 5, price = 105)

make_sell <- function(pobj,
                      id,
                      date,
                      symbol,
                      quantity,
                      price,
                      desc = "") {
  stopifnot(class(pobj) == "portfolio")
  stopifnot(class(id) == "numeric")
  trade <- sell(date, symbol, quantity, price, desc)
  holding <- pobj %>% get_holding(id)

  if (trade$quantity > holding$quantity) {
    stop("Trade quantity greater than holding amount. No short trades allowed.",
         .call = FALSE)
  }

  trade_df <- as.data.frame(trade) %>%
    dplyr::mutate(id = max(pobj$trades$id, 0) + 1)

  new_holding <- holding
  new_holding$quantity <- new_holding$quantity - trade$quantity
  gain <- gains(trade, holding) %>%
    add_tax_liability() %>%
    dplyr::mutate(id = max(pobj$gains$id, 0) + 1)

  pobj$cash <- pobj$cash + trade$amount
  pobj$tax_liability <- pobj$tax_liability + gain$tax_liability
  pobj$trades <- rbind(pobj$trades, trade_df)
  pobj$holdings <- rbind(pobj$holdings %>%
                           dplyr::filter(id != id), new_holding) %>%
    dplyr::arrange(id)
  pobj$gains <- rbind(pobj$gains, gain)

  pobj
}
