#!/usr/bin/env Rscript
#


# MadStork Portfolio Update Script ----------------------------------------
# Script to be run programmatically from command line
# loads configurable portfolio object, updates, creates report and saves back to file


# Docopt Command Line Arguments -------------------------------------------

doc <- "Usage: portfolio_update.R [options] [-h]

-p --port PORT  path of Portfolio object to load and update  [default: NA]
-rdir --reportdir REPORTDIR directory to save report in [default: `./`]
"

# Load required packages
for(p in c('docopt', 'madstork', 'tidyverse', 'quantmod')){
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

# Load Portfolio
port <- load_portfolio(opt$port)

# Update Market Value
port <- update_market_value(port)

# Create Report
report(port, "performance-report.html", opt$rdir)

# Save Updated Port
save_portfolio(port, opt$port, overwrite = TRUE)

