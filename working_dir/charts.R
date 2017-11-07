library("plyr")
library("ggplot2")

# add epsilon to the results, can be multiple Epsilons .. Intersting to see of other multiples, ie policies, generic
results$Epsilon <- rep(0.1, nrow(results))

results <- transform(results, Epsilon = factor(Epsilon))

# Plot average reward as a function of time.
stats <- ddply(results,
               c("Epsilon", "T"),
               function(df) {
                 mean(df$Reward)
               })
p <-
  ggplot(stats, aes(
    x = T,
    y = V1,
    group = Epsilon,
    color = Epsilon
  )) +
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
               function(df) {
                 mean(df$ChosenArm == 5)
               })
p <-
  ggplot(stats, aes(
    x = T,
    y = V1,
    group = Epsilon,
    color = Epsilon
  )) +
  geom_line() +
  ylim(0, 1) +
  xlab("Time") +
  ylab("Probability of Selecting Best Arm") +
  ggtitle("Accuracy of the Epsilon Greedy Algorithm")

print(p)

# Plot variance of chosen arms as a function of time.
stats <- ddply(results,
               c("Epsilon", "T"),
               function(df) {
                 var(df$ChosenArm)
               })
p <-
  ggplot(stats, aes(
    x = T,
    y = V1,
    group = Epsilon,
    color = Epsilon
  )) +
  geom_line() +
  xlab("Time") +
  ylab("Variance of Chosen Arm") +
  ggtitle("Variability of the Epsilon Greedy Algorithm")

print(p)

# Plot cumulative reward as a function of time.
stats <- ddply(results,
               c("Epsilon", "T"),
               function(df) {
                 mean(df$CumulativeReward)
               })
p <-
  ggplot(stats, aes(
    x = T,
    y = V1,
    group = Epsilon,
    color = Epsilon
  )) +
  geom_line() +
  xlab("Time") +
  ylab("Cumulative Reward of Chosen Arm") +
  ggtitle("Cumulative Reward of the Epsilon Greedy Algorithm")

print(p)
