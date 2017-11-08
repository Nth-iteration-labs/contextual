
# library(contextual)

# setwd("~/GitHub/contextual/scripts")

# if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)

source("../R/contextual_utility.R")
source("../R/contextual_arm_bernoulli.R")
source("../R/contextual_offline_evaluation.R")
source("../R/contextual_policy_epsilon_greedy.R")

###################################### basic ######################################

set.seed(10)

# Define a policy

policy                    <- EpsilonGreedy$new(0.1) # multiple polcies should be possible

# Define some contextual features, and their arms.. if one feature, MAB - multiple features: cMAB
# fun to make possible function that changes over time

pvalues_feature_man       <- c(0.1, 0.1, 0.1, 0.1, 0.9)
pvalues_feature_woman     <- c(0.1, 0.9, 0.1, 0.1, 0.1)
pvalues_feature_child     <- c(0.5, 0.1, 0.1, 0.1, 0.1)
pvalues_feature_cat       <- c(0.1, 0.1, 0.1, 0.3, 0.9)
pvalues_feature_dog       <- c(0.1, 0.9, 0.2, 0.1, 0.1)
pvalues_feature_mouse     <- c(0.5, 0.1, 0.1, 0.4, 0.1)

# build a nice matrix out of that

context_pvalues_matrix    <- rbind(pvalues_feature_man,pvalues_feature_woman,pvalues_feature_child,
                                   pvalues_feature_cat,pvalues_feature_dog,pvalues_feature_mouse)
context_features_vector   <- rownames(context_pvalues_matrix)

# create context x arms matrix and context feature vector

context_arms_matrix       <- as.matrix(apply(context_pvalues_matrix, c(1,2), BernoulliArm$new))

# Run OfflineEvaluation

num_sims                  <- 300L
horizon                   <- 250L
contextual_offline        <- OfflineEvaluation$new(policy, context_arms_matrix, num_sims, horizon, context_features_vector)
results                   <- contextual_offline$run()

# Print the results

names(results)            <- c("Sim","T","ChosenArm","Reward", "Feature")

# print(results)

###################################### chart ######################################

# for calculation nice to know which optimal
# optimal <- c(5,2,1) # can also calculate, of course :D cumulative etc.

# chart per feature, all arms, in red best arm.

source("charts.R")




