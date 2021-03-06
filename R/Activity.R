

# Activity Class ----------------------------------------------------------


#' Activity Object Constructor function
#'
#' Function to create an object of class activity
#'
#' @param type activity type. string type.
#' @param date date of activity. Date type
#' @param amount activity dollar amount. numeric type
#' @param desc activity description. string type
#'
#' @return object of class activity
new_activity <- function(type,
                         date,
                         amount,
                         desc) {
  stopifnot(is.character(type))
  stopifnot(lubridate::is.Date(date))
  stopifnot(is.character(desc))
  stopifnot(is.numeric(amount))

  structure(
    list(
      date_added = Sys.Date(),
      transaction_date = date,
      type = type,
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
  if (!x$type %in% c("deposit", "withdraw", "fee")) {
    stop("activity type not supported. only deposit, withdraw, fee types allowed",
         .call = F)
  }
  x
}


#' Function to convert activity to tibble
#'
#' @rdname to_tibble
#' @export
to_tibble.activity <- function(x, ...) {
  tibble::tibble(
    date_added = x$date_added,
    transaction_date = x$transaction_date,
    type = x$type,
    amount = x$amount,
    desc = x$desc
  )
}


# Internal Helper function to create empty activity df
empty_activity_df <- function() {
  tibble::tibble(
    date_added = as.Date(character()),
    transaction_date = as.Date(character()),
    type = character(),
    amount = numeric(),
    desc = character(),
    id = numeric()
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
    type = "deposit",
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
#' @inheritParams deposit
#'
#' @return portfolio object with updated deposit
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port") %>%
#' make_deposit(date=Sys.Date(), amount = 100)
make_deposit <- function(pobj, date = Sys.Date(), amount, desc = "") {
  checkmate::assert_class(pobj, "portfolio")

  action <- deposit(date, amount, desc)
  action_df <- to_tibble(action)

  nid <- ifelse(nrow(pobj$activity) == 0, 1, max(pobj$activity$id) + 1)
  action_df <- dplyr::mutate(action_df, id = nid)

  pobj$cash <- pobj$cash + action$amount
  pobj$activity <- rbind(pobj$activity, action_df)
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
    type = "withdraw",
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
#' @param pobj portfolio object
#' @inheritParams withdraw
#' @importFrom magrittr %>%
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port") %>%
#' make_deposit(date=Sys.Date(), amount = 100) %>%
#' make_withdraw(date=Sys.Date(), amount = 50)
make_withdraw <- function(pobj, date = Sys.Date(), amount, desc = "") {
  stopifnot(class(pobj) == "portfolio")

  action <- withdraw(date, amount, desc)
  action_df <- to_tibble(action) %>%
    dplyr::mutate(id = max(pobj$activity$id, 0)+1)

  if (action$amount > pobj$cash) {
    stop("Withdraw amount greater than cash available",
         .call = FALSE)
  }

  pobj$cash <- pobj$cash - action$amount
  pobj$activity <- rbind(pobj$activity, action_df)
  pobj
}


#' Create Fee Activity Type Helper
#'
#' Create a valid fee object of class Activity
#'
#' @inheritParams new_activity
#' @return valid activity object
#' @export
#'
#' @examples
#' fee(Sys.Date(), 5, "transaction cost")
fee <- function(date, amount, desc = ""){
  validate_activity(new_activity(
    type = "fee",
    date = date,
    amount = amount,
    desc = desc
  ))
}


#' Incur Fee function
#'
#' incurr fee on a portfolio. fees could be transaction or management fees
#'
#' subtracts fee amount from cash balance and adds record to actvity
#'
#' @param pobj portfolio object
#' @inheritParams fee
#' @importFrom magrittr %>%
#'
#' @return updated portfolio object
#' @export
#'
#' @examples
#' library(tidyverse)
#' portfolio("new_port") %>%
#' make_deposit(amount = 100) %>%
#' incur_fee(amount = 50)
incur_fee <- function(pobj, date = Sys.Date(), amount, desc=""){
  stopifnot(class(pobj) == "portfolio")

  action <- fee(date, amount, desc)
  action_df <- to_tibble(action) %>%
    dplyr::mutate(id = max(pobj$activity$id, 0)+1)

  if (action$amount > pobj$cash) {
    stop("Fee greater than cash available",
         .call = FALSE)
  }

  pobj$cash <- pobj$cash - action$amount
  pobj$activity <- rbind(pobj$activity, action_df)
  pobj
}


#' @rdname process
#' @export
process.activity <- function(obj, pobj, ...) {

  checkmate::assert_class(pobj, "portfolio")

  activity_fun <- dplyr::case_when(
    obj$type == "deposit" ~ "make_deposit",
    obj$type == "withdraw" ~ "make_withdraw",
    obj$type == "fee" ~ "incur_fee",
    TRUE ~ "identity"
  ) %>%
    match.fun()

  activity_args <- list(pobj = pobj,
                        date = obj$transaction_date,
                        amount = obj$amount,
                        desc = obj$desc)

  pobj <- do.call("activity_fun", activity_args)

  pobj
}
