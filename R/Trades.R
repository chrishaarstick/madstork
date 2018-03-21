
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
  if (!x$type %in% c("buy", "sell", "transfer_in", "transfer_out")) {
    stop("trade type not supported. only buy, sell, transfer_in and transfer_out types allowed",
         .call = F)
  }
  if (nchar(x$symbol) > 4) {
    message("double check symbol ticker - character length greater than 4")
  }
  x
}


# Internal function to convert trade to data.frame
as.data.frame.trade <- function(x) {
  data.frame(
    date_added = as.Date(x$date_added),
    transaction_date = as.Date(x$transaction_date),
    type = as.character(x$type),
    symbol = as.character(x$symbol),
    quantity = as.numeric(x$quantity),
    price = as.numeric(x$price),
    amount = as.numeric(x$amount),
    desc = as.character(x$desc)
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
#' @param trans_cost transaction cost (dollars per share)
#' @importFrom magrittr %>%
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port", cash = 2000) %>%
#' make_buy(symbol = "SPY", quantity = 10, price = 100)
make_buy <- function(pobj,
                     date = Sys.Date(),
                     symbol,
                     quantity,
                     price,
                     desc = "",
                     trans_cost = .05) {
  stopifnot(class(pobj) == "portfolio")
  trade <- buy(date, symbol, quantity, price, desc)
  trade_df <- as.data.frame(trade) %>%
    dplyr::mutate(id = max(pobj$trades$id, 0) + 1)

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
#' @param trans_cost transaction cost (dollars per share)
#' @inheritParams sell
#' @importFrom magrittr %>%
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#' library(tidyverse)
#'  p1 <- portfolio("new_port", cash=0) %>%
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
  stopifnot(class(pobj) == "portfolio")
  stopifnot(class(id) == "numeric")
  holding <- pobj %>% get_holding(id)
  Id <- id

  if (nrow(holding) == 0) {
    stop("No holdings returned. Check for correct Trade ID",
         .call = FALSE)
  }

  symbol <- as.character(holding$symbol)
  trade <- sell(date, symbol, quantity, price, desc)

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
  pobj$trades <- rbind(pobj$trades, trade_df)
  pobj$holdings <- rbind(pobj$holdings %>%
                           dplyr::filter(id != Id),
                         new_holding) %>%
    dplyr::arrange(id)
  pobj$gains <- rbind(pobj$gains, gain)

  pobj
}


#' Create Transfer Trade Helper
#'
#' Create a valid transfer object of class Trade
#'
#' @inheritParams new_trade
#' @return valid trade object
#' @export
#'
#' @examples
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
      desc
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
#'  p1 <- portfolio("new_port", cash=0) %>%
#'        make_deposit(Sys.Date(), amount = 2000) %>%
#'        make_buy(Sys.Date()-1, symbol = "SPY", quantity = 10, price = 100) %>%
#'        transfer_out(id = 1)
transfer_out <- function(pobj,
                         id,
                         date = Sys.Date(),
                         desc = "") {
  stopifnot(class(pobj) == "portfolio")
  stopifnot(class(id) == "numeric")
  holding <- pobj %>% get_holding(id)
  Id <- id

  if (nrow(holding) == 0) {
    stop("No holdings returned. Check for correct Trade ID",
         .call = FALSE)
  }
  symbol <- as.character(holding$symbol)
  trade <- transfer(date, symbol, holding$quantity, holding$price, type = "out", desc)
  trade_df <- as.data.frame(trade) %>%
    dplyr::mutate(id = max(pobj$trades$id, 0) + 1)

  pobj$trades <- rbind(pobj$trades, trade_df)
  pobj$holdings <- pobj$holdings %>%
    dplyr::filter(id != Id) %>%
    dplyr::arrange(id)

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
#'  p1 <- portfolio("new_port", cash=0) %>%
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
  trade_df <- as.data.frame(trade) %>%
    dplyr::mutate(id = max(pobj$trades$id, 0) + 1)

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

