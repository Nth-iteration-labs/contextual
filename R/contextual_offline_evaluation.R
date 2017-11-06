library(R6)

#' @export
OfflineEvaluation <- R6Class(
  "OfflineEvaluation",

  public = list(
    policy = NULL,
    arms = NULL,
    num_sims = NULL,
    horizon = NULL,

    #private? initialize these values here?
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

      for (sim in 1:self$num_sims) {

        self$policy$set_arms(length(self$arms))

        for (t in 1:self$horizon) {

          index = (sim - 1) * self$horizon + t

          self$sim_nums[index] <- sim
          self$times[index] <- t

          chosen_arm <- self$policy$select_arm()

          self$chosen_arms[index] <- chosen_arm

          chosen_arm_object = self$arms[chosen_arm][[1]]

          reward <- chosen_arm_object$draw()
          self$rewards[index] <- reward

          if (t == 1) {
            self$cumulative_rewards[index] <- reward
          } else {
            self$cumulative_rewards[index] <- self$cumulative_rewards[index - 1] + reward
          }
          self$policy$update(chosen_arm, reward)


        }
      }

      ret = data.frame(self$sim_nums, self$times, self$chosen_arms, self$rewards, self$cumulative_rewards)
      ret

    }
  )
)



