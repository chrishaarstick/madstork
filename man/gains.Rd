% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Gains.R
\name{gains}
\alias{gains}
\title{Create Gains Helper Fuction}
\usage{
gains(sale, holding, gain_threshold = 365)
}
\arguments{
\item{sale}{sell type trade object}

\item{holding}{holding record}

\item{gain_threshold}{min number of days for a holding to be taxed at long
term rate}
}
\description{
Function create a realized gains object of class gains
}
\details{
Used in make_sale function
}
