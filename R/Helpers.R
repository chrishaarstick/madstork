
#' Capitalize function
#'
#' Function that capitalizes all words.
#'
#' @param x vector of character strings
#' @param first_only first word only
#'
#' @return vector of capitalized character strings
#' @export
#'
#' @examples
#'
capitalize <- function(x, first_only = FALSE) {
  s <- sapply(x, function(x) strsplit(x, " ")[[1]])
  if(first_only){
    s[1] <- paste(toupper(substring(s[1], 1,1)), substring(s[1], 2),  sep="", collapse=" ")
    paste(s, sep="", collapse = " ")
  }else{
    paste(toupper(substring(s, 1,1)), substring(s, 2),  sep="", collapse=" ")
  }
}


#' Function to convert object to tibble
#'
#' @param x trade object
#' @param ... additional arguments. not currently implemented
#'
#' @export
to_tibble <- function(x, ...) {
  UseMethod("to_tibble")
}


#' Default to tibble behavior
#'
#' @rdname to_tibble
#' @export
to_tibble.default <- function(x, ...) {
  tibble::as_tibble(x)
}
