library(R6)
#' @export
RandomPolicy <- R6Class(
  "RandomPolicy",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  public = list(
    name = "",
    initialize = function(name = "Random" ) {
      self$name = name
    },
    get_action = function(agent){
      return(sample.int(agent$bandit$k, 1))
    }
  )
)
