% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Trades.R
\name{sell}
\alias{sell}
\title{Create Sell Trade Helper function}
\usage{
sell(date, symbol, quantity, price, desc = "", trans_cost = 0.05)
}
\arguments{
\item{date}{date of transaction. Date type}

\item{symbol}{symbol ticker. character type}

\item{quantity}{number of shares. numeric type}

\item{price}{price of shares. numeric type}

\item{desc}{trade description. string type}

\item{trans_cost}{transaction cost (dollars per share)}
}
\value{
sell type trade object
}
\description{
Creates a sell type of trade object
}
\examples{
sell(date = Sys.Date(), symbol = "SPY", quantity = 10, price = 100)
}
