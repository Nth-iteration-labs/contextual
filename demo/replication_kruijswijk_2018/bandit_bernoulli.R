# bandit with two arms and one feature, which represents one out of n_users
BernoulliBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  public = list(
    class_name       = "BernoulliBandit",
    n_users       = NULL, # number of users in trial
    probs_arm_one    = NULL, # random deviates for arm one
    probs_arm_two    = NULL, # random deviates for arm two
    probs_users      = NULL, # random poisson deviates for per user sampling probability
    arm_one_shape    = NULL, # non-negative parameters of the Beta distribution arm one
    arm_two_shape    = NULL, # non-negative parameters of the Beta distribution arm two
    do_poisson       = NULL, # use poisson distribution to generate user probabilities
    lambda           = NULL, # lambda of poisson distribution generating per user sampling probability
    precaching       = FALSE,
    initialize  = function(n_users, arm_one_shape = c(2,2), arm_two_shape = c(2,2), do_poisson = FALSE, lambda = 2) {
      self$k                              <- 2                # two armed bandit
      self$d                              <- 1                # one context feature, which user
      self$n_users                        <- n_users          # number of users
      self$lambda                         <- lambda           # lambda of poisson distribution generating per user sampling probability
      self$do_poisson                     <- do_poisson       # use poisson distribution to generate user probabilities?
      self$arm_one_shape                  <- arm_one_shape    # non-negative parameters of the Beta distribution arm one
      self$arm_two_shape                  <- arm_two_shape    # non-negative parameters of the Beta distribution arm two
    },
    post_initialization = function() {
      poisson_probs                       <- rpois(self$n_users , self$lambda)
      self$probs_users                    <- poisson_probs / sum(poisson_probs)
      self$probs_arm_one                  <- rbeta(self$n_users, self$arm_one_shape[1], self$arm_one_shape[2])
      self$probs_arm_two                  <- rbeta(self$n_users, self$arm_two_shape[1], self$arm_two_shape[2])
    },
    get_context = function(t) {
      if (self$do_poisson) {
        user = sample(1:self$n_users, 1, prob = self$probs_users)
      } else {
        user = sample(1:self$n_users, 1)
      }
      contextlist <- list(
        k            = self$k,
        d            = self$d,
        user_context = user
      )
      contextlist
    },
    get_reward = function(t, context, action) {
      user              <- context$user_context
      user_weights      <- c(self$probs_arm_one[user],self$probs_arm_two[user])
      rewards           <- rbinom(2, 1, user_weights)
      optimal_arm       <- which_max_tied(user_weights)
      reward  <- list(
        reward                   = rewards[action$choice],
        optimal_arm              = optimal_arm,
        optimal_reward           = rewards[optimal_arm]
      )
    }
  )
)
