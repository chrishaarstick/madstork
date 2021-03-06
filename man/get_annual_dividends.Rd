% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Symbols.R
\name{get_annual_dividends}
\alias{get_annual_dividends}
\title{Get Annualized Security Dividends}
\usage{
get_annual_dividends(symbols)
}
\arguments{
\item{symbols}{vector of stock symbol characters. can be 1 or more}
}
\value{
data.frame with 1 record per symbol with annual dividend, avg
  payment, number of payments per year and last payment date
}
\description{
Wrapper function around get_dividends function that returns annualized
dividends based on prior years dividend payments. Annual dividend used to
calculate security yield
}
\details{
Annual dividend is calculated as the sum of prior years payments. Note if
either 5 or 13 payments returned, dividend trims the first payment
}
\examples{
\donttest{
library(tidyverse)
library(quantmod)
symbols <- c("spy", "tlt")
dividends <- symbols \%>\% get_annual_dividends(., start_date = "2016-01-01")
}
}
