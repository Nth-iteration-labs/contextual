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
    get_action = function(agent){
      return(sample.int(agent$bandit$k, 1))


      ################
      ################ TODO
    }
  )
)
