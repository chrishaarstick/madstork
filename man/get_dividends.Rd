% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Symbols.R
\name{get_dividends}
\alias{get_dividends}
\title{Get Security Historical Dividends}
\usage{
get_dividends(symbols, start_date = "1990-01-01",
  end_date = Sys.Date())
}
\arguments{
\item{symbols}{vector of stock symbol characters. can be 1 or more}

\item{start_date}{starting date for historical dividends. default is
'1990-01-01'. Y-m-d format required}

\item{end_date}{ending date for historical dividends. default is current date.
Y-m-d format required}
}
\value{
tibble with a record for each dividend distrubtion per security
}
\description{
Wrapper function around quantmod::getDividend function
}
\examples{
\donttest{
library(tidyverse)
symbols <- c("spy", "tlt")
dividends <- symbols \%>\% get_dividends(., start_date = "2016-01-01")
}
}
