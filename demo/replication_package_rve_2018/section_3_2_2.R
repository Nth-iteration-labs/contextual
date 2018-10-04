library(contextual)

# Load and attach the contextual package.
library(contextual)
# Define for how long the simulation will run.
horizon <- 400
# Define how many times to repeat the simulation.
simulations <- 10000
# Define the probability that each ad will be clicked.

click_probabilities <- matrix( c( 0.2, 0.3, 0.1,   # --> d1: old (p=.5)
                                  0.6, 0.1, 0.1 ), # --> d2: young (p=.5)
                                nrow = 2, ncol = 3, byrow = TRUE)
# Initialize a SyntheticBandit with contextual weights
context_bandit <- ContextualBernoulliPrecachingBandit$new(weights = click_probabilities, sum_weights = TRUE)
# Initialize LinUCBDisjointPolicy
lu_policy <- LinUCBDisjointPolicy$new(0.6)
# Initialize three Agents, binding each policy to a bandit.
ef_agent <- Agent$new(ef_policy, context_bandit)
eg_agent <- Agent$new(eg_policy, context_bandit)
lu_agent <- Agent$new(lu_policy, context_bandit)
# Assign all agents to a list.
agents <- list(ef_agent, eg_agent, lu_agent)
# Initialize a Simulator with the agent list, horizon, and number of simulations.
simulator <- Simulator$new(agents, horizon, simulations)
# Now run the simulator.
history <- simulator$run()

par(mfrow = c(1, 2), mar = c(2,4,1,1))
# Finally, plot the average reward per time step t
plot(history, type = "average", regret = FALSE, lwd = 2, no_par = TRUE, legend_border = FALSE, legend_position = "bottomright")
# And the cumulative reward rate, which equals the Click Through Rate)
plot(history, type = "cumulative", regret = FALSE, rate = TRUE, lwd = 2, no_par = TRUE, legend_border = FALSE, legend_position = "bottomright")
par(mfrow = c(1, 1))

