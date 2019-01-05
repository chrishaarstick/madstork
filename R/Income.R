
# Income Class ------------------------------------------------------------


#' Income Object Constructor function
#'
#' Function to create an object of class income
#'
#' @param type trade type. string type.
#' @param date date of income payment. Date type
#' @param symbol symbol ticker. character type
#' @param quantity number of shares. numeric type
#' @param payment payment per share. Dividend for equity and interest for bond.
#'   numeric type
#' @param amount income dollar amount. numeric type
#' @param desc income description. string type
#'
#' @return object of class income
new_income <- function(type,
                       date,
                       symbol,
                       quantity,
                       payment,
                       amount,
                       desc) {
  stopifnot(is.character(type))
  stopifnot(lubridate::is.Date(date))
  stopifnot(is.character(symbol))
  stopifnot(is.numeric(quantity))
  stopifnot(is.numeric(payment))
  stopifnot(is.numeric(amount))
  stopifnot(is.character(desc))

  structure(
    list(
      date_added = Sys.Date(),
      transaction_date = date,
      type = type,
      symbol = symbol,
      quantity = quantity,
      payment = payment,
      amount = amount,
      desc = desc
    ),
    class = "income"
  )
}


#' Income Object Validation function
#'
#' Function to validate income input values
#'
#' @param x object of class income
#'
#' @return valid income object
validate_income <- function(x) {
  if (x$payment < 0) {
    stop("negative payment not allowed",
         .call = F)
  }
  if (x$amount < 0) {
    stop("negative amount not allowed",
         .call = F)
  }
  if (!x$type %in% c("dividend", "interest")) {
    stop("income type not supported. only dividend and interest types allowed",
         .call = F)
  }
  if (nchar(x$symbol) > 4) {
    message("double check symbol ticker - character length greater than 4")
  }
  x
}


#' Function to convert income to tibble
#'
#' @rdname to_tibble
#' @export
to_tibble.income <- function(x, ...) {
  tibble::tibble(
    date_added = x$date_added,
    transaction_date = x$transaction_date,
    type = x$type,
    symbol = x$symbol,
    quantity = x$quantity,
    payment = x$payment,
    amount = x$amount,
    desc = x$desc
  )
}


# Internal helper function to create empty income df
empty_income_df <- function() {
  tibble::tibble(
    date_added = character(),
    transaction_date = character(),
    type = character(),
    symbol = character(),
    quantity = numeric(),
    payment = numeric(),
    amount = numeric(),
    desc = character(),
    id = numeric()
  )
}


# Internal Function to execute an income object
make_income <- function(pobj, income){
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_class(income, "income")

  income_df <- to_tibble(income)
  nid <- ifelse(nrow(pobj$income) == 0, 1, max(pobj$income$id) + 1)
  income_df <- dplyr::mutate(income_df, id = nid)

  activity <- income_df %>%
    dplyr::mutate(desc = paste("income_id:", income_df$id)) %>%
    dplyr::mutate(id = max(pobj$activity$id, 0) + 1) %>%
    dplyr::select(date_added, transaction_date, type, amount, desc, id)

  pobj$cash <- pobj$cash + income$amount
  pobj$activity <- rbind(pobj$activity, activity)
  pobj$income <- rbind(pobj$income, income_df)
  pobj
}


#' Create Dividend Helper function
#'
#' Function to create a dividend income type
#'
#' @param date date of dividend. Date type
#' @param symbol symbol ticker. character type
#' @param quantity number of shares. numeric type
#' @param dividend dividend per share. numeric type
#' @param amount dollar amount. numeric type
#' @param desc optional description of dividend. string type
#'
#' @return valid income object
#' @export
#'
#' @examples
#' dividend(Sys.Date(), symbol = "SPY", quantity = 10, dividend = 2, amount = 20)
dividend <- function(date,
                     symbol,
                     quantity,
                     dividend,
                     amount,
                     desc = "") {
  validate_income(
    new_income(
      type = "dividend",
      date = date,
      symbol = symbol,
      quantity = quantity,
      payment = dividend,
      amount = amount,
      desc = desc
    )
  )
}


#' Recieve Dividend payment
#'
#' Function updates cash balance and adds record to activity and income records
#'
#' @param pobj portfolio object
#' @inheritParams dividend
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port") %>%
#' make_deposit(amount = 2000) %>%
#' make_buy(symbol = "SPY", quantity = 10, price = 100) %>%
#' recieve_dividend(symbol = "SPY", quantity = 10, dividend = 2)
recieve_dividend <- function(pobj,
                             date = Sys.Date(),
                             symbol,
                             quantity = NULL,
                             dividend = NULL,
                             amount = NULL,
                             desc = "") {

  checkmate::assert_class(pobj, "portfolio")

  if(is.null(amount) & (is.null(dividend) | is.null(quantity))){
    stop("Please supply either valid amount or valid dividend and quantities",
         .call=FALSE)
  }
  if (is.null(amount)) {
    amount <- dividend * quantity
  }
  if (is.null(quantity)) {
    sym <- symbol
    quantity <- get_holdings(pobj) %>%
      dplyr::filter(symbol == sym) %>%
      dplyr::summarise_at(vars(quantity), funs(sum)) %>%
      pull(quantity)
  }
  if (is.null(dividend)) {
    dividend <- amount / quantity
  }
#
#   if(! symbol %in% get_holdings(pobj)$symbol){
#     stop("Dividend symbol not in current holdings",
#          .call=FALSE)
#   }

  income <- dividend(date, symbol, quantity, dividend, amount, desc)
  make_income(pobj, income)
}


# Internal function to process bulk dividends
# dividends input should be return of either get_past_dividends or get_new_dividends
recieve_dividends <- function(pobj, dividends) {

  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_data_frame(dividends, ncol = 5)
  checkmate::assert_set_equal(colnames(dividends),
                              c("date", "symbol", "dividend", "quantity", "amount"))

  n <- nrow(dividends)
  if(n > 0) {

    for(rn in 1:n) {

      div <- dplyr::slice(dividends, rn)
      pobj <- recieve_dividend(pobj,
                               date     = div$date,
                               symbol   = div$symbol,
                               quantity = div$quantity,
                               dividend = div$dividend,
                               amount   = div$amount)
    }
  }

  pobj
}


#' Create Interest Helper function
#'
#' Function to create an interest income type
#'
#' @param date date of interest payment. Date type
#' @param principal principal amount . numeric type
#' @param amount dollar amount of interest payment. numeric type
#' @param desc optional description of interest. string type
#'
#' @return valid income object
#' @export
#'
#' @examples
#' interest(Sys.Date(), 1000, 1, "monthly interest")
interest <- function(date, principal = 1, amount, desc = "") {
  validate_income(
    new_income(
      type = "interest",
      date = date,
      symbol = "BANK",
      quantity = principal,
      payment = amount/principal,
      amount = amount,
      desc = desc
    )
  )
}



#' Recieve Interest payment
#'
#' Function updates cash balance and adds record to activity and income records
#'
#' @param pobj portfolio object
#' @inheritParams interest
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port") %>%
#' make_deposit(amount = 2000) %>%
#' make_buy(symbol = "SPY", quantity = 10, price = 100) %>%
#' recieve_interest(amount = 5)
recieve_interest <- function(pobj,
                             date = Sys.Date(),
                             amount,
                             desc = "") {

  checkmate::assert_class(pobj, "portfolio")
  income <- interest(date, get_cash(pobj), amount, desc)
  make_income(pobj, income)
}


#' Set Portfolio Interest Rate
#'
#' Set the rate of interest portfolio earns on cash position
#'
#' @param pobj portfolio object
#' @param rate interest rate. ex: 0.05 = 5 percent
#'
#' @return updated portfolio object
#' @export
set_interest_rate <- function(pobj, rate) {
  checkmate::assert_class(pobj, "portfolio")
  checkmate::assert_numeric(rate)

  pobj$interest_rate <- rate
  pobj
}


#' @rdname process
#' @export
process.income <- function(obj, pobj, ...) {

  checkmate::assert_class(pobj, "portfolio")

  if(obj$type == "interest") {

    pobj <- recieve_interest(pobj,
                             date = obj$transaction_date,
                             amount = obj$amount,
                             desc = obj$desc)

  } else if(obj$type == "dividend") {

    pobj <- recieve_dividend(pobj,
                             date = obj$transaction_date,
                             symbol = obj$symbol,
                             quantity = obj$quantity,
                             dividend = obj$payment,
                             amount = obj$amount,
                             desc = obj$desc)
  }

  pobj
}
