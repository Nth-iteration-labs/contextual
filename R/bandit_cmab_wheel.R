#' @export
ContextualWheelBandit <- R6::R6Class(
  "ContextualWheelBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    rewards = NULL,
    delta = NULL,
    mean_v = NULL,
    std_v = NULL,
    mu_large = NULL,
    std_large = NULL,
    binary = NULL,
    class_name = "ContextualWheelBandit",
    precaching = FALSE,
    initialize  = function(delta, mean_v, std_v, mu_large, std_large, binary_rewards = FALSE) {
      self$k                                    <- 5
      self$d                                    <- 2
      self$binary                               <- binary_rewards
      self$delta                                <- delta
      self$mean_v                               <- mean_v         # per arm
      self$std_v                                <- std_v          # per arm
      self$mu_large                             <- mu_large
      self$std_large                            <- std_large
    },
    get_context = function(t) {

      repeat{
        context                                 <- runif(self$d, -1, 1)
        if(norm(as.matrix(context),"2") <= 1) {
          break
        }
      }

      self$rewards                              <- rnorm(self$k,self$mean_v,self$std_v)

      if (norm(as.matrix(context),"2") >= self$delta) {
        r_big                                   <- rnorm(1,self$mu_large, self$std_large)
        if (context[1] > 0) {
          if (context[2] > 0) {
            self$rewards[1]                     <- r_big
          } else {
            self$rewards[2]                     <- r_big
          }
        } else {
          if (context[2] > 0) {
            self$rewards[3]                     <- r_big
          } else {
            self$rewards[4]                     <- r_big
          }
        }
      }

      context                                   <- matrix(context, self$d,self$k)

      if (isTRUE(self$binary)) {
        reward_vector                           <- rep(0,self$k)   ## this could be smarter with divide half etc
        reward_vector[which.max(reward_vector)] <- 1
        self$rewards                            <- reward_vector
      }

      context_list <- list(
        k = self$k,
        d = self$d,
        X = context
      )

      context_list
    },
    get_reward = function(t, context_common, action) {
      rewardlist <- list(
        reward                   = self$rewards[action$choice],
        optimal_reward_value     = self$rewards[which.max(self$rewards)]
      )
      rewardlist
    }
  )
)


#' ContextualWheelBandit
#'
#' @name ContextualWheelBandit
#'
NULL
