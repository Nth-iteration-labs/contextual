library(contextual)

horizon                           <- 1500
simulations                       <- 100L

# Lets set up a minimal contextual data generator or "bandit",
# that represents a website frontpage with sports and movie
# articles for arms, and male and female visitors for the context.
#
# Within this setup, we define the following click-probabilities,
# where a click is a reward of 1, an no click a reward of 0:
#
#  Contexts   | Sport (arm) |  Movie (arm)
# -----------------------------------------
#  Male*      | 0.4 x 0.5   |  0.7 x 0.5
#  Female*    | 0.8 x 0.5   |  0.3 x 0.5
# -----------------------------------------
#  CTR total  | 0.7         |  0.5
#
# So, overall, sports is the most clicked frontpage article type,
# but males actually click movie links more.
#
#
#                      S----M------------> Arm 1:   Sport
#                      |    |              Arm 2:   Movie
#                      |    |
weights  <- matrix( c(0.4, 0.7,    #-----> Context: Male
                      0.8, 0.3),   #-----> Context: Female
                      nrow = 2, ncol = 2, byrow = TRUE)

bandit   <- ContextualBernoulliBandit$new(weights = weights)

# Lets now run an epsilon greedy policy against this bandit.
# Epsilon greedy is a non-contextual bandit, so has no knowledge
# of the context, and is expected to prefer the sports arm (nr1):

policy   <- EpsilonGreedyPolicy$new(epsilon = 0.1)

agents   <- list(Agent$new(policy, bandit, "EG"))

sim      <- Simulator$new(agents, horizon, simulations, save_context = TRUE)
history  <- sim$run()

plot(history, regret = TRUE)
plot(history, type = "arms")
plot(history, type = "arms", limit_context= c("X.1"))
plot(history, type = "arms", limit_context= c("X.2"))

# Now run an contextual epsilon greedy policy against this bandit.

policy   <- ContextualEpsilonGreedyPolicy$new(0.1)

agents   <- list(Agent$new(policy,bandit, "cEpsilonGreedy"))

sim      <- Simulator$new(agents, horizon, simulations, save_context = TRUE)
history  <- sim$run()

plot(history, regret = TRUE)
plot(history, type = "arms")
plot(history, type = "arms", limit_context= c("X.1"))
plot(history, type = "arms", limit_context= c("X.2"))


# Finaly, ContextualEpochGreedyPolicy

policy   <- ContextualEpochGreedyPolicy$new(10)

agents   <- list(Agent$new(policy, bandit, "cEpochGreedy"))

sim      <- Simulator$new(agents, horizon, simulations, save_context = TRUE)
history  <- sim$run()

plot(history, regret = TRUE)
plot(history, type = "arms")
plot(history, type = "arms", limit_context= c("X.1"))
plot(history, type = "arms", limit_context= c("X.2"))

