library(R6)
#' @export
OraclePolicy <- R6Class(
  "OraclePolicy",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  public = list(
    name = "",
    initialize = function(name = "Oracle" ) {
      self$name = name
    },
    get.action = function(agent,context){
      return(index.of.max(context$oracle))
    }
  )
)
