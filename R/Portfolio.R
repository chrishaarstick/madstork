# library(R6)
#
# # Portfolio Object
# Portfolio <- R6Class(classname = "Portfolio",
#
#                      public = list(
#                        name = NULL,
#                        cash = NA,
#                        holdings = NA,
#                        trades = NA,
#                        activity = NA
#                      ),
#
#                      initialize = function(name) {
#                        self$name <- name
#                        self$cash <- NA
#                        self$holdings <- NA
#                        self$trades <- NA
#                        self$activity <- NA
#                      }
# )
