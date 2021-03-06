% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Symbols.R
\name{get_ochlav}
\alias{get_ochlav}
\title{Get Securities Price and Volume Data}
\usage{
get_ochlav(symbols, start_date = "1990-01-01", end_date = Sys.Date(),
  error_handling = "pass", warning = FALSE)
}
\arguments{
\item{symbols}{vector of stock symbol characters. can be 1 or more}

\item{start_date}{starting date for historical prices. default is
'1990-01-01'. Y-m-d format required}

\item{end_date}{ending date for historical prices. default is current date.
Y-m-d format required}

\item{error_handling}{option to handle errors within foreach loop. options
are 'pass', 'remove', or 'stop'}

\item{warning}{logical argument to print getSymbol warnings to console}
}
\value{
tibble with symbol historical prices. schema is date, symbol,
  open, close, high, low, adj_close, volume
}
\description{
Wrapper function to get historical security prices for 1 or more symbols.
Function uses getSymbol function from quantmod package. Returns Open, Close,
High, Low, Adj Close, and Volume
}
\examples{
\donttest{
library(tidyverse)
library(quantmod)
symbols <- c("spy", "tlt")
sym_data <- symbols \%>\% get_ochlav(.)
}
}
