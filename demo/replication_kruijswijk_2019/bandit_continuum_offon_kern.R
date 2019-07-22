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
    initialize   = function(horizon) {
      self$c1 <- runif(1,0.25,0.75)
      self$c2 <- runif(1,0.25,0.75)
      print(self$c2)
      self$arm_function <- function(x, c1 = 0.25, c2 = 0.75) {
        -(x - c1) ^ 2 + c2  + rnorm(length(x), 0, 0.01)
      }
      self$choice <- runif(horizon, min=0, max=1)
      private$S <- data.frame(self$choice, self$arm_function(self$choice, self$c1, self$c2))
      self$k <- 1
      self$h <- horizon^(-1/5)
      self$kernel <- function(action_true, action_choice, bandwith){ 1/sqrt(2*pi)*exp(((action_choice - action_true) / bandwith)^2/2) }
    },
    post_initialization = function() {
      private$S <- private$S[sample(nrow(private$S)),]
      colnames(private$S) <- c('choice', 'reward')
      private$n <- 0
      #print(private$S)
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
