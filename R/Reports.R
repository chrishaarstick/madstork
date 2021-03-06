

#' Generic Report Function
#'
#' @param portfolio portfolio object
#' @param output_file name of output file. Needs a html extension
#' @param output_dir directory to write output file to
#' @param pandoc_dir pandoc directory path
#' @param ... additional parameters to pass to rmarkdown::render function
#'
#' @export
report <- function(...) {
  UseMethod("report")
}

#' Portfolio Performance Report
#'
#' Function to create a Porftolio Performance Report for input Portfolio.
#'
#' Uses portfolio_report.Rmd template in the madstork package
#'
#' @inheritParams report
#'
#' @export
report.portfolio <- function(portfolio,
                             output_file = "performance-report.html",
                             output_dir = getwd(),
                             pandoc_dir = "C:/Program Files/RStudio/bin/pandoc",
                             ...) {

  checkmate::assert_directory(output_dir)
  checkmate::assert_character(output_file, pattern = ".html")
  template <- system.file("Rmd", "portfolio_report.Rmd", package = "madstork")
  checkmate::assert_file_exists(template)

  Sys.setenv(RSTUDIO_PANDO = pandoc_dir)
  rmarkdown::render(
    input = template,
    intermediates_dir = output_dir,
    params = list(port = portfolio),
    output_file = output_file,
    output_dir = output_dir
  )
}



#' MadStork Risk Report Function
#'
#' Creates a Risk Report on Portfolio provided.
#'
#' Uses risk_report.Rmd template in MadStork package
#'
#' @inheritParams report
#'
#' @export
risk_report <- function(portfolio,
                        output_file = "risk-report.html",
                        output_dir = getwd(),
                        pandoc_dir = "C:/Program Files/RStudio/bin/pandoc",
                        ...) {

  checkmate::assert_directory(output_dir)
  checkmate::assert_character(output_file, pattern = ".html")
  template <- system.file("Rmd", "risk_report.Rmd", package = "madstork")
  checkmate::assert_file_exists(template)

  Sys.setenv(RSTUDIO_PANDO = pandoc_dir)
  rmarkdown::render(
    input = template,
    intermediates_dir = output_dir,
    params = list(port = portfolio),
    output_file = output_file,
    output_dir = output_dir
  )
}
