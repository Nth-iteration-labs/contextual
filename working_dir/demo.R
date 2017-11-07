# library(contextual)

if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)
source("../R/contextual_arm_bernoulli.R")
source("../R/contextual_offline_evaluation.R")
source("../R/contextual_policy_epsilon_greedy.R")

###################################### basic ######################################

# Define a policy.

policy = EpsilonGreedy$new(0.1)

# Define some arms.

list_of_pvalues <- c(0.1, 0.1, 0.1, 0.1, 0.9)

arms <- lapply(list_of_pvalues, BernoulliArm$new)

# Run OfflineEvaluation.

num_sims <-  1000L
horizon  <-  250L

offline <-  OfflineEvaluation$new(policy, arms, num_sims, horizon)

results <-  offline$run()

# Print the results.

names(results) <- c("Sim","T","ChosenArm","Reward","CumulativeReward")

print(results)

###################################### chart ######################################

source("charts.R")
