library("plyr")
library("ggplot2")

# add Feature to the results, can be multiple Features .. Intersting to see of other multiples, ie policies, generic
# results$Feature <- rep(0.1, nrow(results))

results <- transform(results, Feature = factor(Feature))

# Plot average reward as a function of time.
stats <- ddply(results,
               c("Feature", "T"),
               function(df) {
                 mean(df$Reward)
               })
p <-
  ggplot(stats, aes(
    x = T,
    y = V1,
    group = Feature,
    color = Feature
  )) +
  geom_line() +
  ylim(0, 1) +
  xlab("Time") +
  ylab("Average Reward") +
  ggtitle("Performance of the Feature Greedy Algorithm")

print(p)

# Plot frequency of selecting correct arm as a function of time.
# In this instance, 5 is the correct arm.
stats <- ddply(results,
               c("Feature", "T"),
               function(df) {
                 mean(df$ChosenArm == 5)
               })
p <-
  ggplot(stats, aes(
    x = T,
    y = V1,
    group = Feature,
    color = Feature
  )) +
  geom_line() +
  ylim(0, 1) +
  xlab("Time") +
  ylab("Probability of Selecting Best Arm") +
  ggtitle("Accuracy of the Feature Greedy Algorithm")

print(p)

# Plot variance of chosen arms as a function of time.
stats <- ddply(results,
               c("Feature", "T"),
               function(df) {
                 var(df$ChosenArm)
               })
p <-
  ggplot(stats, aes(
    x = T,
    y = V1,
    group = Feature,
    color = Feature
  )) +
  geom_line() +
  xlab("Time") +
  ylab("Variance of Chosen Arm") +
  ggtitle("Variability of the Feature Greedy Algorithm")

print(p)
