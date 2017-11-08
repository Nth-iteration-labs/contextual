library(R6)

#' @export
OfflineEvaluation <- R6Class(
  "OfflineEvaluation",

  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,

  public = list(

    last_cummulative = 0L,

    policy = NULL,
    arms = NULL,
    num_sims = NULL,
    horizon = NULL,

    chosen_arms = NULL,
    rewards = NULL,
    cumulative_rewards = NULL,
    sim_nums = NULL,
    times = NULL,

    initialize = function(policy = NA, arms = NA, num_sims = NA, horizon = NA) {
      self$policy <- policy
      self$arms <- arms
      self$num_sims <- num_sims
      self$horizon <- horizon

      self$chosen_arms <- rep(0.0, times = num_sims * horizon)
      self$rewards <- rep(0.0, times = num_sims * horizon)
      self$cumulative_rewards <- rep(0.0, times = num_sims * horizon)
      self$sim_nums <- rep(0.0, times = num_sims * horizon)
      self$times <- rep(0.0, times = num_sims * horizon)
    },
    run = function() {
      index <- 1L
      sim <- 1L
      t <- 1L
      reward <- 1L
      chosen_arm <- 1L
      for (sim in 1L:self$num_sims) {
        self$policy$set_arms(length(self$arms))
        for (t in 1L:self$horizon) {
          index = (sim - 1L) * self$horizon + t
          self$sim_nums[index] <- sim
          self$times[index] <- t
          chosen_arm <- self$policy$select_arm()
          self$chosen_arms[index] <- chosen_arm
          reward <- self$arms[[chosen_arm]]$draw()
          self$rewards[index] <- reward
          self$last_cummulative <- self$last_cummulative + reward
          self$cumulative_rewards[index] <- self$last_cummulative
          self$policy$update(chosen_arm, reward)
        }
      }
      df <- data.frame(self$sim_nums, self$times, self$chosen_arms, self$rewards, self$cumulative_rewards)
      return(df)
    }
  )
)



