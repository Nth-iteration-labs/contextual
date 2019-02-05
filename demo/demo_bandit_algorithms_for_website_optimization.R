library(contextual)

# Bandit algorithms for website optimization -----------------------------------------------------------------

## Simulation of the multi-armed Bandit examples in
## of "Bandit algorithms for website optimization"
## by John Miles White.

# The code from the book chooses the arm with the first index when all arms are equal.
# Contextuals policies correctly picks one of the max arms.
# That's why the plots below are slightly different from the book - they are correct, though.

# Chapter 4 - Debugging and epsilon greedy -------------------------------------------------------------------

prob_per_arm       <- c(0.1, 0.1, 0.1, 0.1, 0.9)
horizon            <- 250
simulations        <- 5000

bandit             <- BasicBernoulliBandit$new(prob_per_arm)

agents             <- list(Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "Epsilon = 0.1"),
                           Agent$new(EpsilonGreedyPolicy$new(0.2), bandit, "Epsilon = 0.2"),
                           Agent$new(EpsilonGreedyPolicy$new(0.3), bandit, "Epsilon = 0.3"),
                           Agent$new(EpsilonGreedyPolicy$new(0.4), bandit, "Epsilon = 0.4"),
                           Agent$new(EpsilonGreedyPolicy$new(0.5), bandit, "Epsilon = 0.5"))

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

# Figure 4-2. How often does the epsilon greedy algorithm select the best arm?

plot(history, type = "optimal", legend_position = "bottomright", ylim = c(0,1))

# Figure 4-3. How much reward does the epsilon greedy algorithm earn on average?

plot(history, type = "average", regret = FALSE, legend_position = "bottomright", ylim = c(0,1))

# Figure 4-4. How much reward has the epsilon greedy algorithm earned by trial t?

plot(history, type = "cumulative", regret = FALSE)

# Chapter 5 - Softmax ----------------------------------------------------------------------------------------

agents             <- list(Agent$new(SoftmaxPolicy$new(0.1), bandit, "Tau = 0.1"),
                           Agent$new(SoftmaxPolicy$new(0.2), bandit, "Tau = 0.2"),
                           Agent$new(SoftmaxPolicy$new(0.3), bandit, "Tau = 0.3"),
                           Agent$new(SoftmaxPolicy$new(0.4), bandit, "Tau = 0.4"),
                           Agent$new(SoftmaxPolicy$new(0.5), bandit, "Tau = 0.5"))

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

# Figure 5-2. How often does the softmax algorithm select the best arm?

plot(history, type = "optimal", legend_position = "bottomright", ylim = c(0,1))

# Figure 5-3. How much reward does the softmax algorithm earn on average?

plot(history, type = "average", regret = FALSE, legend_position = "bottomright", ylim = c(0,1))

# Figure 5-4. How much reward has the softmax algorithm earned by trial t?

plot(history, type = "cumulative", regret = FALSE)


# Chapter 6 - UCB --------------------------------------------------------------------------------------------

agents             <- list(Agent$new(SoftmaxPolicy$new(0.1), bandit, "Softmax"),
                           Agent$new(EpsilonGreedyPolicy$new(0.1), bandit, "EpsilonGreedy"),
                           Agent$new(UCB1Policy$new(), bandit, "UCB1"))

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

# Figure 6-3. How often does the UCB algorithm select the best arm?

plot(history, type = "optimal", legend_position = "bottomright", ylim = c(0,1))

# Figure 6-4. How much reward does the UCB algorithm earn on average?

plot(history, type = "average", regret = FALSE, legend_position = "bottomright", ylim = c(0,1))

# Figure 6-5. How much reward has the UCB algorithm earned by trial t?

plot(history, type = "cumulative", regret = FALSE)
