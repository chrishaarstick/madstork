% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Portfolio-update.R
\name{get_new_dividends}
\alias{get_new_dividends}
\title{Get New Portfolio Dividends}
\usage{
get_new_dividends(pobj)
}
\arguments{
\item{pobj}{portfolio object}
}
\value{
tibble with new dividends. arrange by symbol and date with
  holding quantity, dividend and total payment amount
}
\description{
Function to return all new holding dividends not yet recieved.
}
