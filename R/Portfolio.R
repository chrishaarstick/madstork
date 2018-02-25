

#' Portfolio Object Constructor
#'
#' Function creates a new S3 Portfolio object
#'
#' @param name name of portfolio. requires string input
#' @param cash cash balance. requires numeric input. defaults to 0
#' @param activity activity history. requires a data.frame input. defaults to
#'   null
#'
#' @return Portfolio object
#' @export
#'
#' @examples
#' new_portfolio("new_port", cash=0, activity = data.frame())
new_portfolio <- function(name,
                          cash,
                          activity) {
  stopifnot(is.character(name))
  stopifnot(is.numeric(cash))
  stopifnot(is.data.frame(activity))

  structure(list(
    name = name,
    cash = cash,
    activity = activity
  ),
  class = "Portfolio")
}



#' Portfolio validation function
#'
#' Validator function to check for valid inputs
#'
#' @param x Portfolio object
#'
#' @return
#' @export
#'
#' @examples
#' validate_portfolio(new_portfolio("new_port", cash=0, activity = data.frame()))
validate_portfolio <- function(x) {
  if (x$cash < 0) {
    stop("Loans not allowed - initial cash balance should be >= 0",
         .call = F)
  }
  x
}


# Portfolio S3 helper function
Portfolio <- function(name,
                      cash = 0,
                      activity = data.frame()) {
  validate_portfolio(new_portfolio(name, cash, activity))
}
