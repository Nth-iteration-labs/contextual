library(R6)
library(MASS)

#' @export
LinUCBPolicy <- R6Class(

  "LinUCBPolicy",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    alpha = NULL,

    initialize = function(alpha = NA) {
      self$alpha = alpha
    },

    # returns the ucb estimates p at time t for arm a

    pta = function(inverse_cov_matrix, reward_vector_times_design_matrix, context_vector, alpha){
      theta_hat <- inverse_cov_matrix %*% reward_vector_times_design_matrix
      ucb_estimate <- t(theta_hat) %*% context_vector +
        alpha * sqrt(t(context_vector) %*% inverse_cov_matrix %*% context_vector)
      return(ucb_estimate)
    },

    get_action = function(agent, context){
      theta = agent$get_theta()
      ucb = vector(mode = "numeric", length = 0)
      for (arm in 1:context$k) {
        Ainv <- ginv(theta[[arm]][['A']])
        estimates <- pta(Ainv,  as.matrix(theta[[arm]][['b']]), as.matrix(c(context$clicked_sports, context$clicked_politics)), self$alpha)

        ucb = append(ucb, estimates)
      }
      action = index_of_max(ucb)
      return(action)

    }

  )
)

