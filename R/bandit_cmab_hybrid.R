#' @export
ContextualHybridBandit <- R6::R6Class(
  "ContextualHybridBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    betas_s = NULL,                                                 ## regression betas shared over all arms
    betas_u = NULL,                                                 ## regression betas unique per arm
    s       = NULL,                                                 ## nr shared features/betas
    u       = NULL,                                                 ## nr unique features/betas
    sigma   = NULL,                                                 ## standard deviation of noise

    class_name = "ContextualHybridBandit",
    precaching = FALSE,
    initialize  = function(k, s, u, sigma = 1.0, assign_context_type = TRUE) {

      self$sigma   <- sigma
      self$k       <- k                                             ## nr of arms
      self$s       <- s                                             ## nr shared features/betas
      self$u       <- u                                             ## nr unique features/betas
      self$d       <- u + s                                         ## total number of features

      if (assign_context_type) {                                    ## for bandits that use this
        if(s==0 || u==0) {                                          ## add shared/unique
          self$shared  <- c(1:self$d)                               ## information in context
          self$unique  <- c(1:self$d)
        } else {
          self$shared  <- c(1:s)
          self$unique  <- c((s+1):(u+s))
        }
      }
    },
    post_initialization = function() {
      self$betas_s <- runif(self$s,0,1/(self$u+1))                  ## generate  unique features/betas
      self$betas_u <- matrix(runif(self$u*self$k), self$u, self$k)  ## generate shared features/betas
    },
    get_context = function(t) {
      X <- matrix(runif(self$d*self$k, 0, 1), self$d, self$k)
      context_list <- list(
        k = self$k,
        d = self$d,
        unique = self$unique,
        shared = self$shared,
        X = X
      )
      context_list
    },
    get_reward = function(t, context, action) {
      betas        <- c(self$betas_s, self$betas_u[,action$choice])
      trb          <- betas%*%context$X[,action$choice]
      trb          <- trb + rnorm(1,0,self$sigma)
      reward       <- rbinom(1,1,1/(1+exp(-trb)))
      rewardlist   <- list(
        reward                   = reward,
        optimal_reward_value     = 1
      )
      rewardlist
    }
  )
)

#' ContextualHybridBandit
#'
#' @name ContextualHybridBandit
#'
NULL
