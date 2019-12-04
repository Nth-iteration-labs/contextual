#' @export
OnlineOfflineContinuumBanditKernel <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  private = list(
    S = NULL,
    n = NULL
  ),
  public = list(
    class_name = "OnlineOfflineContinuumBanditKernel",
    delta = NULL,
    c1 = NULL,
    c2 = NULL,
    arm_function = NULL,
    choice = NULL,
    h = NULL,
    kernel = NULL,
    horizon = NULL,
    initialize   = function(FUN, horizon) {
      self$arm_function <- FUN
      self$k <- 1
      self$horizon <- horizon
      self$h <- horizon^(-1/5)
      self$kernel <- function(action_true, action_choice, bandwith){ 1/sqrt(2*pi)*exp(((action_choice - action_true) / bandwith)^2/2) }
    },
    post_initialization = function() {
      self$choice <- runif(self$horizon, min=0, max=1)
      private$S <- data.frame(self$choice, self$arm_function(self$choice))
      private$S <- private$S[sample(nrow(private$S)),]
      colnames(private$S) <- c('choice', 'reward')
      private$n <- 0
    },
    get_context = function(index) {
      context           <- list()
      context$k         <- self$k
      context
    },
    get_reward = function(index, context, action) {
      reward_at_index <- as.double(private$S$reward[[index]])
      #kern_value <- self$kernel(action_true = private$S$choice[[index]], action_choice = action$choice, bandwith = self$h)
      temp_u <- (action$choice - private$S$choice[[index]]) / self$h
      kern_value <- 1/sqrt(2*pi) * exp(-temp_u^2 / 2)
      #inc(private$n) <- 1
      #print(paste0("Kernel value: ", kern_value, "action choice: ", action$choice, "true action: ", private$S$choice[[index]], "divy: ", temp_u))
      reward <- list(
        reward = (kern_value * reward_at_index),
        optimal_reward = self$c2
      )
    }
  )
)
