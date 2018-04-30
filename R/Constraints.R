



# Constraints Class -------------------------------------------------------


constraints <- function(symbols,
                        constraints = NULL) {
  checkmate::assert_character(symbols)
  checkmate::assert_list(constraints, null.ok = TRUE)

  if (is.null(constraints)) {
    constraints <- list()
  }

  structure(list(symbols = symbols,
                 constraints = constraints),
            class = "constraints")
}


add_constraint <- function(constraints, constraint) {
  checkmate::assert_class(constraints, "constraints")
  checkmate::assert_class(constraint, "constraint")

  id <- length(constraints$constraints) + 1
  constraint$id <- id
  constraints$constraints[[id]] <- constraint
  constraints
}


get_constraints <- function(constraints,
                            type = NULL,
                            id = NULL) {
  checkmate::assert_class(constraints, "constraints")
  checkmate::assert_subset(type,
                           c("symbol", "cardinality", "group", "performance"),
                           empty.ok = TRUE)
  checkmate::assert_number(id, lower = 1, null.ok = TRUE)

  if (is.null(type)) {
    constraints <- constraints$constraints
  } else {
    constraints <-
      constraints$constraints[sapply(constraints$constraints, function(x)
        x$type) == type]
  }

  if (!is.null(id)) {
    constraints <-
      constraints[sapply(constraints, function(x)
        x$id) == id]
  }

  constraints
}



check_constraints <- function(constraints, portfolio, estimates) {
  checkmate::assert_class(constraints, "constraints")
  checkmate::assert_class(portfolio, "portfolio")
  checkmate::assert_class(estimates, "estimates")

  holdings <- get_holdings_market_value(portfolio)
  stats <-
    suppressWarnings(get_estimated_port_stats(portfolio, estimates)) %>%
    dplyr::filter(type == "portfolio")

  checks <- lapply(
    constraints$constraints,
    check_constraint,
    holdings = holdings,
    stats = stats
  )
  suppressWarnings(dplyr::bind_rows(checks, .id = "id"))
}



# Constraint Class --------------------------------------------------------


constraint <- function(type,
                       args,
                       min,
                       max) {
  checkmate::assert_choice(type, c("symbol", "cardinality", "group", "performance"))
  checkmate::assert_character(args, null.ok = TRUE)
  checkmate::assert_number(min)
  checkmate::assert_number(max)

  structure(list(
    type = type,
    args = args,
    min  = min,
    max  = max
  ),
  class = "constraint")
}


check_constraint <-
  function(constraint,
           holdings = NULL,
           stats = NULL) {
    UseMethod("check_constraint")
  }



# Symbol Constraints ------------------------------------------------------


symbol_constraint <- function(symbols,
                              min,
                              max) {
  checkmate::assert_character(symbols, null.ok = TRUE)
  checkmate::assert_number(min, lower = 0.0, upper = 1.0)
  checkmate::assert_number(max, lower = 0.0, upper = 1.0)

  structure(
    constraint(type = "symbol", args = symbols, min, max),
    class = c("symbol_constraint", "constraint")
  )
}


add_symbol_constraint <- function(constraints,
                                  symbol = NULL,
                                  min = 0.0,
                                  max = 1.0) {
  checkmate::assert_class(constraints, "constraints")
  checkmate::assert_subset(symbol, constraints$symbols, empty.ok = TRUE)

  if (is.null(symbol)) {
    symbols <- constraints$symbols
  } else {
    symbols <- symbol
  }

  for (sym in symbols) {
    c1 <- symbol_constraint(sym, min, max)
    constraints <- add_constraint(constraints, c1)
  }

  constraints
}


print.symbol_constraint <- function(constraint) {
  cat(
    "Symbol Constraint:",
    paste0(
      constraint$args,
      " min share = ",
      constraint$min,
      ", max share = ",
      constraint$max
    )
  )
}


check_constraint.symbol_constraint <-
  function(constraint, holdings, ...) {
    checkmate::assert_subset(c("symbol", "portfolio_share"), colnames(holdings))
    share <-
      holdings[holdings$symbol == constraint$args, "portfolio_share"]
    check <-
      ifelse(share < constraint$min |
               share > constraint$max, FALSE, TRUE)
    data.frame(
      type = constraint$type,
      args = constraint$args,
      min = constraint$min,
      max = constraint$max,
      value = share,
      check = check
    )
  }


# Cardinality Constraints -------------------------------------------------


cardinality_constraint <- function(min,
                                   max) {
  checkmate::assert_number(min, lower = 0.0)
  checkmate::assert_number(max, lower = 0.0)

  structure(
    constraint(type = "cardinality", args = NULL, min, max),
    class = c("cardinality_constraint", "constraint")
  )
}


add_cardinality_constraint <- function(constraints,
                                       min = 0,
                                       max = NULL) {
  checkmate::assert_class(constraints, "constraints")
  n <- length(constraints$symbols)
  max <- ifelse(is.null(max), n, max)
  checkmate::assert_number(min, lower = 0, upper = n)
  checkmate::assert_number(max, lower = 1, upper = n)

  c1 <- cardinality_constraint(min, max)
  add_constraint(constraints, c1)
}


print.cardinality_constraint <- function(constraint) {
  cat(
    "Cardinality Constraint:",
    paste0(
      "min symbols = ",
      constraint$min,
      ", max symbols = ",
      constraint$max
    )
  )
}


check_constraint.cardinality_constraint <-
  function(constraint, holdings, ...) {
    checkmate::assert_subset(c("symbol"), colnames(holdings))
    n <- length(unique(holdings$symbol))
    check <-
      ifelse(n < constraint$min | n > constraint$max, FALSE, TRUE)
    data.frame(
      type = constraint$type,
      args = "",
      min = constraint$min,
      max = constraint$max,
      value = n,
      check = check
    )
  }


# Group Constraints -------------------------------------------------------


group_constraint <- function(symbols,
                             min,
                             max) {
  checkmate::assert_character(symbols)
  checkmate::assert_number(min, lower = 0.0, upper = 1.0)
  checkmate::assert_number(max, lower = 0.0, upper = 1.0)

  structure(
    constraint(type = "group", args = symbols, min, max),
    class = c("group_constraint", "constraint")
  )
}


add_group_constraint <- function(constraints,
                                 symbols = NULL,
                                 min = 0.0,
                                 max = 1.0) {
  checkmate::assert_class(constraints, "constraints")
  checkmate::assert_subset(symbols, constraints$symbols)

  c1 <- group_constraint(symbols, min, max)
  add_constraint(constraints, c1)
}


check_constraint.group_constraint <-
  function(constraint, holdings, ...) {
    checkmate::assert_subset(c("symbol", "portfolio_share"), colnames(holdings))
    share <- holdings %>%
      dplyr::filter(symbol %in% constraint$args) %>%
      dplyr::summarise_at("portfolio_share", sum) %>%
      .$portfolio_share
    check <-
      ifelse(share < constraint$min |
               share > constraint$max, FALSE, TRUE)
    data.frame(
      type = constraint$type,
      args = paste(constraint$args, collapse = ","),
      min = constraint$min,
      max = constraint$max,
      value = share,
      check = check
    )
  }


# Performance Constraints -------------------------------------------------


performance_constraint <- function(statistic,
                                   min,
                                   max) {
  checkmate::assert_character(statistic)
  checkmate::assert_number(min)
  checkmate::assert_number(max)

  structure(
    constraint(type = "performance", args = statistic, min, max),
    class = c("performance_constraint", "constraint")
  )
}


add_min_return <- function(constraints,
                           min = NULL) {
  checkmate::assert_class(constraints, "constraints")

  c1 <- performance_constraint("mu", min, max = Inf)
  add_constraint(constraints, c1)
}


add_max_risk <- function(constraints,
                         max = NULL) {
  checkmate::assert_class(constraints, "constraints")

  c1 <- performance_constraint("sd", min = 0, max = max)
  add_constraint(constraints, c1)
}



add_min_yield <- function(constraints,
                          min = NULL) {
  checkmate::assert_class(constraints, "constraints")

  c1 <- performance_constraint("yield", min = min, max = Inf)
  add_constraint(constraints, c1)
}



check_constraint.performance_constraint <- function(constraint, stats, ...) {
    checkmate::assert_subset(c("mu", "sd", "sharpe", "yield"), colnames(stats))
    checkmate::assert_choice(stats$type, "portfolio")
    stat <- stats[[constraint$args]]
    check <- ifelse(stat < constraint$min |
               stat > constraint$max, FALSE, TRUE)
    data.frame(
      type = constraint$type,
      args = constraint$args,
      min = constraint$min,
      max = constraint$max,
      value = stat,
      check = check
    )
  }
