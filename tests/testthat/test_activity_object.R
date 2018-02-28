

# Activity Class Tests ----------------------------------------------------


library(madstork)
library(tidyverse)

context("Activity Class")


w1 <- withdraw(Sys.Date(), 100, "monthly withdraw")
d1 <- deposit(Sys.Date(), 100, "monthly investment deposit")


test_that("activity helper functions creates object of class activity", {
  expect_equal(class(w1), "activity")
  expect_equal(class(d1), "activity")
})


test_that("activity validator function works as expected", {
  expect_error(withdraw("new-date", 100))
  expect_error(withdraw(Sys.Date(),-100))
  expect_error(withdraw(Sys.Date(), 100, desc = 100))
  expect_error(deposit("new-date", 100))
  expect_error(deposit(Sys.Date(),-100))
  expect_error(deposit(Sys.Date(), 100, desc = 100))
})


test_that("activity make functions work as expected", {
  p1 <- portfolio("new_port", cash = 100) %>%
    make_deposit(date = Sys.Date(), amount = 100)
  p2 <- p1 %>% make_withdraw(date = Sys.Date(), amount = 150)

  expect_equal(p1$cash, 200)
  expect_equal(p2$cash, 50)
  expect_equal(p1 %>% get_activity(), as.data.frame(deposit(date = Sys.Date(), amount = 100)))
  expect_error(p1 %>% make_withdraw(date = Sys.Date(), amount = 300))
})
