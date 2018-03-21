
# Realized Gains Class ----------------------------------------------------



#' Realized Gains Constructor function
#'
#' Creates new object of class gains
#'
#' @param symbol symbol ticker. character type
#' @param quantity number of shares. numeric type
#' @param purchase_date date of purchase. Date type
#' @param purchase_price cost basis. numeric type
#' @param sale_date date of sale. Date type
#' @param sale_price sale price
#'
#' @return obj of class gains
#' @export
new_gains <- function(symbol,
                      quantity,
                      purchase_date,
                      purchase_price,
                      sale_date,
                      sale_price,
                      gain,
                      type) {
  stopifnot(is.character(symbol))
  stopifnot(is.numeric(quantity))
  stopifnot(lubridate::is.Date(purchase_date))
  stopifnot(is.numeric(purchase_price))
  stopifnot(lubridate::is.Date(sale_date))
  stopifnot(is.numeric(gain))
  stopifnot(is.character(type))

  structure(
    list(
      symbol = symbol,
      quantity = quantity,
      purchase_date = purchase_date,
      purchase_price = purchase_price,
      sale_date = sale_date,
      sale_price = sale_price,
      gain = gain,
      type = type
    ),
    class = "gains"
  )
}


#' Gains Object Validation function
#'
#' Function to validate gains input values
#'
#' @param x object of class gains
#'
#' @return valid gains object
validate_gains <- function(x) {
  if (x$quantity < 0) {
    stop("negative quantity not allowed",
         .call = F)
  }
  if (x$purchase_price < 0) {
    stop("negative price not valid",
         .call = F)
  }
  if (x$purchase_date > x$sale_date) {
    stop("sale date prior to purchase date",
         .call = F)
  }
  if (nchar(x$symbol) > 4) {
    message("double check symbol ticker - character length greater than 4")
  }
  if (!x$type %in% c("st", "lt")) {
    stop("gains type not st or lt (short term or long term)",
         .call = F)
  }

  x
}


#' Create Gains Helper Fuction
#'
#' Function create a realized gains object of class gains
#'
#' Used in make_sale function
#'
#' @param sale sell type trade object
#' @param holding holding record
#' @param gain_threshold min number of days for a holding to be taxed at long
#'   term rate
#'
#' @importFrom magrittr %>%
gains <- function(sale, holding, gain_threshold = 365) {
  stopifnot(class(sale) == "trade")
  stopifnot(sale$symbol == holding$symbol)

  new_gains(
    symbol = sale$symbol,
    quantity = sale$quantity,
    purchase_date = holding$transaction_date,
    purchase_price = holding$price,
    sale_date = sale$transaction_date,
    sale_price = sale$price,
    gain = sale$quantity * (sale$price - holding$price),
    type = ifelse(
      sale$transaction_date - holding$transaction_date > gain_threshold,
      "lt",
      "st"
    )
  ) %>% validate_gains()
}


# Internal function to convert gains to data.frame
as.data.frame.gains <- function(x) {
  data.frame(
    symbol = as.character(x$symbol),
    quantity = as.numeric(x$quantity),
    purchase_date = as.Date(x$purchase_date),
    purchase_price = as.numeric(x$purchase_price),
    sale_date = as.Date(x$sale_date),
    sale_price = as.numeric(x$sale_price),
    gain = as.numeric(x$gain),
    type = as.character(x$type)
  )
}


#' Add Tax Liability function
#'
#' Appends the tax impact of a realized gain
#'
#' @param gain object of gains class
#' @param st_taxrate short term tax rate on gains for holings less than year
#' @param lt_taxrate long term tax rate on
#' @importFrom magrittr %>%
#'
#' @return updated gains with tax impact
#' @export
add_tax_liability <- function(gain,
                              st_taxrate = 0.30,
                              lt_taxrate = 0.15) {
  stopifnot(class(gain) == "gains")

  gain %>%
    as.data.frame() %>%
    mutate(
      tax_rate = ifelse(type == "st", st_taxrate, lt_taxrate),
      tax_liability = tax_rate * gain
    )
}
