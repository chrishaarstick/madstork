
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
