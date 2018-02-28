
# Activity Class ----------------------------------------------------------


#' Activity Object Constructor function
#'
#' Function to create an object of class activity
#'
#' @param activity activity type. string type.
#' @param date date of activity. Date type
#' @param amount activity dollar amount. numeric type
#' @param desc activity description. string type
#'
#' @return object of class activity
new_activity <- function(activity,
                         date,
                         amount,
                         desc) {
  stopifnot(is.character(activity))
  stopifnot(lubridate::is.Date(date))
  stopifnot(is.character(desc))
  stopifnot(is.numeric(amount))

  structure(
    list(
      date_added = Sys.Date(),
      activity_date = date,
      activity = activity,
      amount = amount,
      desc = desc
    ),
    class = "activity"
  )
}


#' Activity Object Validation function
#'
#' Function to validate activity input values
#'
#' @param x object of class activity
#'
#' @return valid activity object
validate_activity <- function(x) {
  if (x$amount < 0) {
    stop("negative amount not allowed",
         .call = F)
  }
  if (!x$activity %in% c("deposit", "withdraw", "fee")) {
    stop("activity type not supported. only deposit, withdraw, fee types allowed",
         .call = F)
  }
  x
}


#'@export
as.data.frame.activity <- function(x) {
  data.frame(
    date_added = x$date_added,
    activity_date = x$activity_date,
    activity = x$activity,
    amount = x$amount,
    desc = x$desc
  )
}


#' Create Deposit Helper function
#'
#' Function to create a deposit activity type
#'
#' @param date date of deposit. Date type
#' @param amount amount of deposit. numeric type
#' @param desc optional description of deposit. string type
#'
#' @return valid activity object
#' @export
#'
#' @examples
#' deposit(Sys.Date(), 100, "monthly investment deposit")
deposit <- function(date, amount, desc = "") {
  validate_activity(new_activity(
    activity = "deposit",
    date = date,
    amount = amount,
    desc = desc
  ))
}




#' Make Deposit function
#'
#' add deposit to portfolio
#'
#' adds to cash balance and appends the deposit to the
#' portfolio activity
#'
#' @param pobj portfolio object
#' @param deposit deposit instance of activity object
#'
#' @return portfolio object with updated deposit
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port", cash = 100) %>%
#' make_deposit(deposit(date=Sys.Date(), amount = 100))
make_deposit <- function(pobj, deposit) {
  stopifnot(class(pobj) == "portfolio")
  stopifnot(class(deposit) == "activity")

  pobj$cash <- pobj$cash + deposit$amount
  pobj$activity <- rbind(pobj$activity, as.data.frame(deposit))
  pobj
}


#' Create Withdraw Helper function
#'
#' Function to create a withdraw activity type
#'
#' @param date date of deposit. Date type
#' @param amount amount of deposit. numeric type
#' @param desc optional description of deposit. string type
#'
#' @return valid activity object
#' @export
#'
#' @examples
#' withdraw(Sys.Date(), 100, "monthly withdraw")
withdraw <- function(date, amount, desc = "") {
  validate_activity(new_activity(
    activity = "withdraw",
    date = date,
    amount = amount,
    desc = desc
  ))
}


#' Make Withdraw function
#'
#' make withdraw from a portfolio
#'
#' subtracts withdraw amount from cash balance and adds record to actvity
#'
#' @param pobj
#' @param withdraw
#'
#' @return
#' @export
#'
#' @examples
#' portfolio("new_port", cash = 100) %>%
#' make_withdraw(withdraw(date=Sys.Date(), amount = 50)
make_withdraw <- function(pobj, withdraw){
  stopifnot(class(pobj) == "portfolio")
  stopifnot(class(withdraw) == "activity")

  if( withdraw$amount > pobj$cash){
    stop("Withdraw amount greater than cash available",
         .call=FALSE)
  }
  pobj$cash <- pobj$cash - withdraw$amount
  pobj$activity <- rbind(pobj$activity, as.data.frame(withdraw))
  pobj
}
