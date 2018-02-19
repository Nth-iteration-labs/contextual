#' @export
SimpleBTSPolicy <- R6::R6Class(
  "SimpleBTSPolicy",
  portable = FALSE,
  class = FALSE,
  inherit = AbstractPolicy,
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
      self$parameters <- list('alpha' = self$a,  'beta' = self$b)
    },
    get_action = function(context, t) {
      point_estimate_of_mean = vector("double", context$k)
      for (arm in 1:context$k) {
        replicate <- sample(self$J, 1)
        r_alpha <- theta$alpha[[replicate]][[arm]]
        r_beta  <- theta$beta[[replicate]][[arm]]
        point_estimate_of_mean[arm] <- r_alpha / (r_alpha + r_beta)
      }
      action$choice <- max_in(point_estimate_of_mean)
      action
    },
    set_reward = function(context, action, reward, t) {
      arm    <- action$choice
      reward <- reward$reward

      double_or_nothing_bootstrap <- which(rbinom(self$J, 1, .5) == 1)
      for (replicate in double_or_nothing_bootstrap) {
        inc(theta$alpha[[replicate]][[arm]]) <- reward
        inc(theta$beta[[replicate]][[arm]])  <- 1 - reward
      }
      theta
    },
    initialize_theta = function() {
      theta <- list()
      for (param_index in 1L:length(parameters)) {
        theta[[names(self$parameters)[param_index]]] <- list()
        for (replicate in 1L:self$J) {
          theta[[names(self$parameters)[param_index]]][[replicate]] <-
            rep(list(self$parameters[[param_index]]), self$k)
        }
      }
      theta
    }
  )
)


#' Policy: SimpleBTSPolicy
#'
#' Each time step t, \code{SimpleBTSPolicy} runs ...
#'
#' @name SimpleBTSPolicy
#' @family contextual policies
#'
#' @section Usage:
#' \preformatted{
#' policy <- SimpleBTSPolicy(alpha = 1.0, name = "LinUCB")
#' }
NULL
