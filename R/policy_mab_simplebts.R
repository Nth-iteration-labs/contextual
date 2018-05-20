#' @export
SimpleBTSPolicy <- R6::R6Class(
  "SimpleBTSPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    J = NULL,
    a = NULL,
    b = NULL,
    initialize = function(J = 100,
                          a = 1,
                          b = 1,
                          name = "SimpleBTSPolicy") {
      super$initialize(name)
      self$J  <- J
      self$a  <- a
      self$b  <- b
    },
    set_parameters = function() {
      self$theta_to_arms <- list('alpha' = rep(self$b,self$J),  'beta' = rep(self$b,self$J))
    },
    get_action = function(t, context) {
      point_estimate_of_mean <- vector("double", context$k)
      for (arm in 1:context$k) {
        one_replicate <- sample(self$J, 1)
        r_alpha <- self$theta$alpha[[arm]][one_replicate]
        r_beta  <- self$theta$beta[[arm]][one_replicate]
        point_estimate_of_mean[arm] <- r_alpha / (r_alpha + r_beta)
      }
      action$choice <- max_in(point_estimate_of_mean)
      action
    },
    set_reward = function(t, context, action, reward) {
      arm    <- action$choice
      reward <- reward$reward
      some_replicates <- which(rbinom(self$J, 1, .5) == 1) # double_or_nothing_bootstrap
      inc(self$theta$alpha[[arm]][some_replicates]) <- reward
      inc(self$theta$beta[[arm]][some_replicates])  <- 1 - reward
      self$theta
    }
  )
)


#' Policy: SimpleBTSPolicy
#'
#' 
#' Each time step t, \code{SimpleBTSPolicy} runs ...
#'
#' @name SimpleBTSPolicy
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#'
#' @section Usage:
#' \preformatted{
#' policy <- SimpleBTSPolicy(alpha = 1.0, name = "LinUCB")
#' }
NULL
