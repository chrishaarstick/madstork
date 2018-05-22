



# Constraints Class -------------------------------------------------------


#' Constraints Constructer
#'
#' Creates an object of class constraints
#'
#' @param symbols vector of symbols to apply constraints to
#' @param constraints list of contraints
#'
#' @return constraints object
#' @export
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


#' Add Constraint to Constraints Object
#'
#' Appends constraint object to constraints list of contraints. Adds id field to
#' constraint object
#'
#' @param constraints constraints object
#' @param constraint constraint object
#'
#' @return updated constraints object
#' @export
add_constraint <- function(constraints, constraint) {
  checkmate::assert_class(constraints, "constraints")
  checkmate::assert_class(constraint, "constraint")

  id <- length(constraints$constraints) + 1
  constraint$id <- id
  constraints$constraints[[id]] <- constraint
  constraints
}


#' Get Constraints
#'
#' Getter function to return constraints object contraints. Subsetable by type
#' or id
#'
#' @param constraints constraints object
#' @param type type of constraint. valid types are symbol, cardinality, group
#'   and performance
#' @param id id of constraint. Equivalent to location in constraints list
#'
#' @return list of constraints
#' @export
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



#' Check Constraints
#'
#' Check the validity of portfolio against constraints.
#'
#' Function applies all constraints on a portfolio holdings and estimates
#'
#' @param constraints constraints object
#' @param portfolio portfolio object
#' @param estimates estimates object
#'
#' @return data.frame with summary of constraint checks
#' @export
check_constraints <- function(constraints, portfolio, estimates) {
  checkmate::assert_class(constraints, "constraints")
  checkmate::assert_class(portfolio, "portfolio")
  checkmate::assert_class(estimates, "estimates")

  holdings <- get_symbol_estimates_share(portfolio, estimates)
  stats <- get_estimated_port_stats(portfolio, estimates, port_only = TRUE)

  checks <- lapply(
    constraints$constraints,
    check_constraint,
    holdings = holdings,
    stats = stats
  )
  suppressWarnings(dplyr::bind_rows(checks, .id = "id"))
}



# Constraint Class --------------------------------------------------------


#' Constraint Constructer
#'
#' Creates an object of class constraint
#'
#' @param type character value for type of constraint. valid types are symbol,
#'   cardinality, group and performance
#' @param args symbol, symbols or statistic to test
#' @param min minimum constraint value. inclusive
#' @param max maximum constraint value. inclusive
#'
#' @return object of class constraint
#' @export
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


#' Check Constraint
#'
#' Check portfolio's holdings and estimated statistics against constraint
#'
#' @param constraint constraint object
#' @param holdings portfolio holdings
#' @param stats portfolio statistics
#'
#' @return data.frame with summary of constraint check
#' @export
check_constraint <- function(constraint,
                             holdings = NULL,
                             stats = NULL) {
  UseMethod("check_constraint")
}



# Symbol Constraints ------------------------------------------------------


#' Symbol Constraint Constructer
#'
#' Inherits from constraint class
#'
#' @param symbols 0 or more symbols to constrain. If NULL, sets min and max
#'   values to all symbols
#' @param min minimum constraint value. inclusive
#' @param max maximum constraint value. inclusive
#'
#' @return
#' @export
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


#' Add Symbol Constraint to Constraints Object
#'
#' Symbol constraints constrain the share of a portfolio's market value a symbol
#' can have
#'
#' @param constraints constraints object
#' @inheritParams symbol_constraint
#'
#' @return updated constraints object
#' @export
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

#' @export
#' @rdname print
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


#' @export
#' @rdname check_constraint
check_constraint.symbol_constraint <- function(constraint, holdings, ...) {
  checkmate::assert_subset(c("symbol", "portfolio_share"), colnames(holdings))
  share <- holdings %>%
    dplyr::filter(symbol == constraint$args) %>%
    .$portfolio_share
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


#' Cardinality Constraint Constructer
#'
#' Cardinality constraints limit number of symbols portfolio can hold. Inherits
#' from constraint class
#'
#' @param min minimum constraint value. inclusive
#' @param max maximum constraint value. inclusive
#'
#' @return object of class cardinality_constraint
#' @export
cardinality_constraint <- function(min,
                                   max) {
  checkmate::assert_number(min, lower = 0.0)
  checkmate::assert_number(max, lower = 0.0)

  structure(
    constraint(type = "cardinality", args = NULL, min, max),
    class = c("cardinality_constraint", "constraint")
  )
}


#' Add Cardinality Constraint to Constraints Object
#'
#' @param constraints constraints object
#' @inheritParams cardinality_constraint
#'
#' @return updated constraints object
#' @export
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


#' @export
#' @rdname print
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

#' @export
#' @rdname check_constraint
check_constraint.cardinality_constraint <- function(constraint, holdings, ...) {
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


#' Group Constraint Constructer
#'
#' Group constraints constrain the total share of a portfolio's market value 2
#' or more symbols can have. Inherits from constraint class
#'
#' @param symbols 1 or more symbols
#' @param min minimum constraint value. inclusive
#' @param max maximum constraint value. inclusive
#'
#' @return group_constraint object
#' @export
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


#' Add Group Constraint to Constraints Object
#'
#'
#' @param constraints constraints object
#' @inheritParams group_constraint
#'
#' @return updated constraints object
#' @export
add_group_constraint <- function(constraints,
                                 symbols = NULL,
                                 min = 0.0,
                                 max = 1.0) {
  checkmate::assert_class(constraints, "constraints")
  checkmate::assert_subset(symbols, constraints$symbols)

  c1 <- group_constraint(symbols, min, max)
  add_constraint(constraints, c1)
}


#' @export
#' @rdname print
print.group_constraint <- function(constraint) {
  cat(
    "Group Constraint:",
    paste0(
      "[", paste(constraint$args, collapse=", "), "]",
      " min share = ",
      constraint$min,
      ", max share = ",
      constraint$max
    )
  )
}


#' @export
#' @rdname check_constraint
check_constraint.group_constraint <- function(constraint, holdings, ...) {
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


#' Performance Constraint Constructer
#'
#' Performance constraints constrain the value of a portfolio's estimated
#' statistics. Inherits from class constraint
#'
#' @param statistic character input for portfolio statistic. valid statistics
#'   are mu, sd, sharpe or yield
#' @param min minimum constraint value. inclusive
#' @param max maximum constraint value. inclusive
#'
#' @return performance_constraint object
#' @export
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


#' Add Minimum Return Performance Constraint to Contraints Object
#'
#' @param constraints constraints object
#' @param min minumum return value
#'
#' @return updated constraints object
#' @export
add_min_return <- function(constraints,
                           min = NULL) {
  checkmate::assert_class(constraints, "constraints")

  c1 <- performance_constraint("mu", min, max = Inf)
  add_constraint(constraints, c1)
}



#' Add Maximum Risk Performance Constraint to Contraints Object
#'
#' @param constraints constraints object
#' @param min maximum risk value
#'
#' @return updated constraints object
#' @export
add_max_risk <- function(constraints,
                         max = NULL) {
  checkmate::assert_class(constraints, "constraints")

  c1 <- performance_constraint("sd", min = 0, max = max)
  add_constraint(constraints, c1)
}


#' Add Minimum Yield Performance Constraint to Contraints Object
#'
#' @param constraints constraints object
#' @param min minumum yield value
#'
#' @return updated constraints object
#' @export
add_min_yield <- function(constraints,
                          min = NULL) {
  checkmate::assert_class(constraints, "constraints")

  c1 <- performance_constraint("yield", min = min, max = Inf)
  add_constraint(constraints, c1)
}


#' @export
#' @rdname print
print.performance_constraint <- function(constraint) {
  cat(
    "Performance Constraint:",
    paste0(
      constraint$args,
      " min = ",
      constraint$min,
      ", max = ",
      constraint$max
    )
  )
}


#' @export
#' @rdname check_constraint
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
