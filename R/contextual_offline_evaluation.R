library(R6)

#' @export
OfflineEvaluation <- R6Class(
  "OfflineEvaluation",
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    arms = NULL,
    num_sims = NULL,
    horizon = NULL,
    chosen_arms = NULL,
    rewards = NULL,
    sim_nums = NULL,
    times = NULL,
    features = NULL,
    n_arms = NULL,
    feature_vector = NULL,
    feature_vector_n = NULL,
    policy_vector = NULL,

    initialize = function(policy = NA, context_arms_matrix = NA, num_sims = NA, horizon = NA, feature_vector = NA) {

      self$num_sims <- num_sims
      self$horizon <- horizon
      self$feature_vector <- feature_vector
      self$feature_vector_n <- length(feature_vector)
      self$arms <- context_arms_matrix
      self$n_arms <- length(self$arms)/self$feature_vector_n

      self$policy_vector <- c()
      for (i in 1:feature_vector_n) self$policy_vector[[i]] <- policy$clone()

      self$chosen_arms        <- rep(0.0, times = num_sims * horizon)
      self$rewards            <- rep(0.0, times = num_sims * horizon)
      self$sim_nums           <- rep(0.0, times = num_sims * horizon)
      self$times              <- rep(0.0, times = num_sims * horizon)
      self$features           <- rep(0.0, times = num_sims * horizon)
    },
    run = function() {
      index <- 1L
      sim <- 1L
      t <- 1L
      reward <- 1L
      chosen_arm <- 1L
      feature_nr <- 1L
      # partly replace by apply like functions?
      for (sim in 1L:self$num_sims) {
        for (i in 1:feature_vector_n) self$policy_vector[[i]]$reset(self$n_arms)
        for (t in 1L:self$horizon) {
          index = (sim - 1L) * self$horizon + t

          # choose which context
          feature_nr = sample(1:feature_vector_n,1)
          self$features[index] <- feature_vector[feature_nr]

          self$sim_nums[index] <- sim
          self$times[index] <- t
          chosen_arm <- self$policy_vector[[feature_nr]]$select_arm()
          self$chosen_arms[index] <- chosen_arm

          reward <- self$arms[[feature_nr,chosen_arm]]$draw()
          self$rewards[index] <- reward
          self$policy_vector[[feature_nr]]$update(chosen_arm, reward)
        }
      }
      df <- data.frame(self$sim_nums, self$times, self$chosen_arms, self$rewards, self$features)
      return(df)
    }
  )
)




