
# Create test samples -----------------------------------------------------

# Desc - script to create helper madstork objects used in testing

# R Packages
devtools::load_all(".")
library(tidyverse)
library(lubridate)
library(testthat)
library(checkmate)

# Test Estimates ----------------------------------------------------------


# Estimates params
yrs <- 10
.symbols <- c("SPY", "QQQ", "TLT", "GLD")
.grain <- "year"
.periods <- 1
test_estimates <- estimates(symbols = .symbols,
                            start_date = Sys.Date() - years(yrs),
                            end_date = Sys.Date(),
                            grain = "year",
                            periods = 1) %>%
  add_sample_mu() %>%
  add_sample_sigma() %>%
  add_dividends()



# Prices ------------------------------------------------------------------

test_prices <- get_current_prices(.symbols, dividends = TRUE)
p <- test_prices %>% split(.$symbol) %>% map("price")



# Test Portfolio ----------------------------------------------------------
#
# test_port <- portfolio("test_port", cash=0) %>%
#   make_deposit(amount = 100000) %>%
#   make_buy(symbol = "SPY", quantity = 100, price = p$SPY) %>%
#   make_buy(symbol = "QQQ", quantity = 100, price = p$QQQ) %>%
#   make_buy(symbol = "TLT", quantity = 100, price = p$TLT) %>%
#   make_buy(symbol = "GLD", quantity = 100, price = p$GLD) %>%
#   update_market_value(test_prices)



# Save to data folder -----------------------------------------------------

save(test_estimates, file = 'data/test-estimates.rdata', compress = 'xz')
save(test_prices, file = 'data/test-prices.rdata', compress = 'xz')
#save(test_port, file = 'data/test-portfolio.rdata', compress = 'xz')

