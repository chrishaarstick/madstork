#!/usr/bin/env Rscript
#


# MadStork Portfolio Update Script ----------------------------------------
# Script to be run programmatically from command line
# loads configurable portfolio object, updates, creates report and saves back to file


# Docopt Command Line Arguments -------------------------------------------

doc <- "Usage: portfolio_update.R [options] [-h]

-p --port PORT  path of Portfolio object to load and update  [default: NA]
-d --dir DIR directory to save report in [default: `./`]
"

# Load required packages
for(p in c('docopt', 'madstork', 'tidyverse', 'quantmod', 'rmarkdown')){
  if(p %in% rownames(installed.packages())){
    suppressMessages(library(p, character.only = T))
  }else{
    stop(paste(p, "package not installed \n"))
  }
}

opt <- docopt::docopt(doc)

# Execute logic -----------------------------------------------------------

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
Sys.setenv("RSTUDIO_PANDOC" = "C:/Program Files/RStudio/bin/pandoc")

# Load Portfolio
port <- load_portfolio(opt$port)

# Update Market Value
port <- update_market_value(port)

# Create Report
warning(Sys.getenv("RSTUDIO_PANDOC"), "\n")

if (pandoc_available())
 warning("pandoc", as.character(pandoc_version()), "is available!\n")

if (pandoc_available("1.12.3"))
 warning("required version of pandoc is available!\n")

report(port, output_file = "performance-report.html", output_dir = opt$dir)

# Save Updated Port
save_portfolio(port, opt$port, overwrite = TRUE)

