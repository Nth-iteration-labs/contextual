library(R6)

#' @export
LinUCBPolicy <- R6Class(

  "LinUCBPolicy",

  inherit = UCBPolicy,

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    alpha = NULL,
    d = NULL,

    initialize = function(alpha = NA, d = NA) {

      self$alpha = alpha
      self$d = d

    },

    choose = function(agent){

      ucb = vector(mode = "numeric", length = 0)

      for (action in 1:NROW(agent$memory)) {
        A = agent$memory[[action]][['A']]
        A_inv = solve(A)
        x_t = agent$states[action]
        d_p = t(x_t) %*% A_inv
        exploration = self$alpha * sqrt(d_p %*% x_t)
        ucb = append(ucb, agent$value_estimates[action] + exploration)
      }

      ###ucb[sample(1:length(ucb), 1)] = ucb[sample(1:length(ucb), 1)] + 0.000001  # For tie values, not needed. #### and np.random.choice? did they want from array?

      action = index_of_max(ucb)

      return(action)
    }

  )
)
