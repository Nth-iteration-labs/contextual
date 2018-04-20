# bandit with two arms and one feature, which represents one out of n_subjects users
BernoulliBandit <- R6::R6Class(
  "BernoulliBandit",
  inherit = Bandit,
  portable = TRUE,
  class = FALSE,
  public = list(
    n_subjects       = NULL, # number of subjects in trial
    probs_arm_one    = NULL, # random deviates for arm one
    probs_arm_two    = NULL, # random deviates for arm two
    probs_users      = NULL, # random poisson deviates for per user sampling probability
    arm_one_shape    = NULL, # non-negative parameters of the Beta distribution arm one
    arm_two_shape    = NULL, # non-negative parameters of the Beta distribution arm two
    poisson_subjects = NULL, # use poisson distribution to generate user probabilities
    lambda           = NULL, # lambda of poisson distribution generating per user sampling probability
    precaching       = FALSE,
    initialize  = function(n_subjects, arm_one_shape = c(10,10), arm_two_shape = c(10,10), poisson_subjects = TRUE, lambda = 2) {
      self$k                              <- 2                # two armed bandit
      self$d                              <- 1                # one context feature, which user
      self$n_subjects                     <- n_subjects       # number of subjects
      self$lambda                         <- lambda           # lambda of poisson distribution generating per user sampling probability
      self$poisson_subjects               <- poisson_subjects # use poisson distribution to generate user probabilities?
      self$arm_one_shape                  <- arm_one_shape    # non-negative parameters of the Beta distribution arm one
      self$arm_two_shape                  <- arm_two_shape    # non-negative parameters of the Beta distribution arm two
    },
    pre_calculate = function() {
      poisson_probs                       <- rpois(self$n_subjects , self$lambda)
      self$probs_users                    <- poisson_probs / sum(poisson_probs)
      self$probs_arm_one                  <- rbeta(self$n_subjects, self$arm_one_shape[1], self$arm_one_shape[2])
      self$probs_arm_two                  <- rbeta(self$n_subjects, self$arm_two_shape[1], self$arm_two_shape[2])

    },
    get_context = function(t) {
      if (self$poisson_subjects) {
        user_ctxt = sample(1:self$n_subjects, 1, prob = self$probs_users)
      } else {
        user_ctxt = sample(1:self$n_subjects, 1)
      }
      contextlist <- list(
        k = self$k,
        d = self$d,
        user_context = user_ctxt
      )
      contextlist
    },
    do_action = function(context, action, t) {
      subject              <- context$user_context
      rewardlist           <- list()
      rewardlist$opimal    <- rbinom(1, 1, max(self$probs_arm_one[subject], self$probs_arm_two[subject]))
      if (action$choice == 1)
        rewardlist$reward  <- rbinom(1, 1, self$probs_arm_one[subject])
      else
        rewardlist$reward  <- rbinom(1, 1, self$probs_arm_two[subject])
      rewardlist
    }
  )
)
