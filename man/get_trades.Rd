% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Portfolio.R
\name{get_trades}
\alias{get_trades}
\title{Get Portfolio Trades}
\usage{
get_trades(pobj)
}
\arguments{
\item{pobj}{portfolio object}
}
\value{
tibble of trade history
}
\description{
getter function to return Portfolio's trades
}
\details{
trades are portfolio investment security transactions
}
\examples{
library(tidyverse)
portfolio("new_port") \%>\%
get_trades(.)
}
