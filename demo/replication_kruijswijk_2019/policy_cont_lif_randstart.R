#' @export
LifPolicyRandstart <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = Policy,
  public = list(
    first = NULL,
    inttime = NULL,                     # Integration time
    amplitude = NULL,                   # Amplitude
    learnrate = NULL,                   # Learnrate
    omega = NULL,                       # Omega
    x0_start = NULL,                    # x0 start value
    class_name = "LifPolicyRandstart",
    initialize = function(inttime,amplitude,learnrate,omega) {
      super$initialize()
      self$inttime   <- inttime
      self$amplitude <- amplitude
      self$learnrate <- learnrate
      self$omega     <- omega
    },
    post_initialization = function(){
      self$x0_start <- runif(1, min = 0.7, max = 1)
      self$theta <- list('x0' = x0_start, 'Y' = rep(NA, inttime), 't_real' = 1)
    },
    set_parameters = function(context_params) {
    },
    get_action = function(t, context) {
      action$choice <- self$theta$x0 + amplitude*cos(omega * self$theta$t_real)
      if(action$choice < 0){
        action$choice <- 0
      }else if(action$choice > 1){
        action$choice <- 1
      }
      action
    },
    set_reward = function(t, context, action, reward) {
      reward   <- reward$reward
      y <- amplitude*cos(omega * self$theta$t_real)*reward
      self$theta$Y <- c(y, self$theta$Y)[seq_along(self$theta$Y)]
      if (self$theta$t_real > inttime)
        self$theta$x0 <- self$theta$x0 + learnrate * sum( self$theta$Y ) / inttime
      self$theta$t_real <- self$theta$t_real + 1
      self$theta
    }
  )
)

#' Policy: Continuum Bandit Policy with Lock-in Feedback
#'
#' The continuum type Lock-in Feedback (LiF) policy is based on an approach used in physics and engineering,
#' where, if a physical variable y depends on the value of a well controllable physical variable x,
#' the search for argmax x f(x) can be solved via what is nowadays considered as standard electronics.
#' This approach relies on the possibility of making the variable x oscillate at a fixed frequency and to
#' look at the response of the dependent variable y at the very same frequency by means of a lock-in
#' amplifier. The method is particularly suitable when y is immersed in a high noise level,
#' where other more direct methods would fail. Furthermore, should the entire curve
#' shift (or, in other words, if argmax x f(x) changes in time, also known as concept drift), the circuit
#' will automatically adjust to the new situation and quickly reveal the new maximum position.
#' This approach is widely used in a very large number of applications, both in industry and research,
#' and is the basis for the Lock-in Feedback (LiF) method.
#'
#' In this, Lock in feedback goes through the following steps, again and again:
#'
#' * Oscillate a controllable independent variable X around a set value at a fixed pace.
#' * Apply the Lock-in amplifier algorithm.to obtain values of the amplitude if the outcome variable Y at
#' the pace you set at step 1.
#' * Is the amplitude of this variable zero? Congratulations, you have reached lock-in!
#' That is, you have found the optimal value of Y at the current value of X.
#' Still, this optimal value might shift over time, so move to step 1 and repeat the process to make sure we
#' maintain lock-in.
#' * Is the amplitude less than, or greater than zero?
#' Then move the set value around which we are oscillating our independent variable X up or down on the basis
#' of the outcome.
#'
#' Now move to step 1 and repeat..
#'
#' @name LifPolicy
#'
#' @section Usage:
#' \preformatted{b <- LifPolicy$new(inttime,amplitude,learnrate,omega,x0_start)}
#'
#' @references
#' Kaptein, M. C., Van Emden, R., & Iannuzzi, D. (2016). Tracking the decoy: maximizing the decoy effect
#' through sequential experimentation. Palgrave Communications, 2, 16082.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},
#' \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualLinTSPolicy}}
NULL

