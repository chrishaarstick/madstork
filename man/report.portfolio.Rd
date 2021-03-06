% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Reports.R
\name{report.portfolio}
\alias{report.portfolio}
\title{Portfolio Performance Report}
\usage{
\method{report}{portfolio}(portfolio,
  output_file = "performance-report.html", output_dir = getwd(),
  pandoc_dir = "C:/Program Files/RStudio/bin/pandoc", ...)
}
\arguments{
\item{portfolio}{portfolio object}

\item{output_file}{name of output file. Needs a html extension}

\item{output_dir}{directory to write output file to}

\item{pandoc_dir}{pandoc directory path}

\item{...}{additional parameters to pass to rmarkdown::render function}
}
\description{
Function to create a Porftolio Performance Report for input Portfolio.
}
\details{
Uses portfolio_report.Rmd template in the madstork package
}
