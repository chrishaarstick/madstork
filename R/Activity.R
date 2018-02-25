




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
