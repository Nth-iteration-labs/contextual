# library(contextual)
library("plyr")
library("ggplot2")

source("R/contextual_arm_bernoulli.R")
source("R/contextual_offline_evaluation.R")
source("R/contextual_policy_epsilon_greedy.R")

#arm <- BernoulliArm$new(0.5)
#drawing = arm$draw()
#cat(drawing)
#result <-  replicate(10, arm$draw())
#cat(result)

epsilon = 0.1

policy = EpsilonGreedy$new(epsilon)

arm1 <- BernoulliArm$new(0.9)
arm2 <- BernoulliArm$new(0.1)
arm3 <- BernoulliArm$new(0.1)
arm4 <- BernoulliArm$new(0.1)
arm5 <- BernoulliArm$new(0.1)

arms <- c(arm1,arm2,arm3,arm4,arm5)

num_sims <-  100
horizon  <-  250

offline <-  OfflineEvaluation$new(policy, arms, num_sims, horizon)

results <-  offline$run()

results$Epsilon <- rep(0.1,nrow(results))

names(results) <- c("Sim", "T", "ChosenArm", "Reward", "CumulativeReward","Epsilon")

results <- transform(results, Epsilon = factor(Epsilon))

# Plot average reward as a function of time.
stats <- ddply(results,
               c("Epsilon", "T"),
               function (df) {mean(df$Reward)})
p <- ggplot(stats, aes(x = T, y = V1, group = Epsilon, color = Epsilon)) +
  geom_line() +
  ylim(0, 1) +
  xlab("Time") +
  ylab("Average Reward") +
  ggtitle("Performance of the Epsilon Greedy Algorithm")

print(p)

# Plot frequency of selecting correct arm as a function of time.
# In this instance, 5 is the correct arm.
stats <- ddply(results,
               c("Epsilon", "T"),
               function (df) {mean(df$ChosenArm == 5)})
p <- ggplot(stats, aes(x = T, y = V1, group = Epsilon, color = Epsilon)) +
  geom_line() +
  ylim(0, 1) +
  xlab("Time") +
  ylab("Probability of Selecting Best Arm") +
  ggtitle("Accuracy of the Epsilon Greedy Algorithm")

print(p)

# Plot variance of chosen arms as a function of time.
stats <- ddply(results,
               c("Epsilon", "T"),
               function (df) {var(df$ChosenArm)})
p <- ggplot(stats, aes(x = T, y = V1, group = Epsilon, color = Epsilon)) +
  geom_line() +
  xlab("Time") +
  ylab("Variance of Chosen Arm") +
  ggtitle("Variability of the Epsilon Greedy Algorithm")

print(p)

# Plot cumulative reward as a function of time.
stats <- ddply(results,
               c("Epsilon", "T"),
               function (df) {mean(df$CumulativeReward)})
p <- ggplot(stats, aes(x = T, y = V1, group = Epsilon, color = Epsilon)) +
  geom_line() +
  xlab("Time") +
  ylab("Cumulative Reward of Chosen Arm") +
  ggtitle("Cumulative Reward of the Epsilon Greedy Algorithm")

print(p)


#print(results)
