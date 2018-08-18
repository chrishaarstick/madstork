
#' Generic Report Function
#'
#' @param ... additional parameters to pass to rmarkdown::render function
#' @export
report <- function(...){
  UseMethod("report")
}

#' Portfolio Performance Report
#'
#' Function to create a Porftolio Performance Report for input Portfolio.
#'
#' Uses portfolio_report.Rmd template in the madstork package
#'
#' @param portfolio portfolio object
#' @param output_file name of output file. Needs a html extension
#' @param output_dir directory to file folder to
#' @export
report.portfolio <- function(portfolio, output_file, output_dir, ...){
  checkmate::assert_directory(output_dir)
  checkmate::assert_file(output_file, extension = ".html")
  template <- system.file("Rmd", "portfolio_report.Rmd", package = "madstork")
  checkmate::assert_file_exists(template)
  rmarkdown::render(input = template,
                    intermediates_dir = output_dir,
                    params = list(port = portfolio),
                    output_file = output_file,
                    output_dir = output_dir,
                    ...)
}
