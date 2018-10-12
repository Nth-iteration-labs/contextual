# Load and attach the contextual package.
library(contextual)
# Define for how long the simulation will run.
horizon <- 400
# Define how many times to repeat the simulation.
simulations <- 10000
# Define the probability that each ad will be clicked.
click_probabilities <- matrix(c(0.6, 0.4, 0.2), nrow = 1, ncol = 3, byrow = TRUE)
# Initialize a SyntheticBandit, which takes probabilites per arm for an argument.
bandit <- ContextualBasicBandit$new(weights = click_probabilities)
# Initialize EpsilonGreedyPolicy with a 40% exploiration rate.
eg_policy <- EpsilonGreedyPolicy$new(epsilon = 0.4)
# Initialize EpsilonFirstPolicy with a .25 x 400 = 100 step exploration period.
ef_policy <- EpsilonFirstPolicy$new(epsilon = 0.25, N = horizon)
# Initialize two Agents, binding each policy to a bandit.
ef_agent <- Agent$new(ef_policy, bandit)
eg_agent <- Agent$new(eg_policy, bandit)
# Assign both agents to a list.
agents <- list(ef_agent, eg_agent)
# Initialize a Simulator with the agent list, horizon, and number of simulations.
simulator <- Simulator$new(agents, horizon, simulations, do_parallel = TRUE)
# Now run the simulator.
history <- simulator$run()
# Finally, plot the average reward per time step t
par(mfrow = c(1, 2), mar = c(2,4,1,1), cex=1.4)
plot(history, type = "average", regret = FALSE, no_par = TRUE, legend_border = FALSE, legend_position = "bottomright")
# And the cumulative reward rate, which equals the Click Through Rate)
plot(history, type = "cumulative", regret = FALSE, rate = TRUE,  no_par = TRUE, legend_border = FALSE, legend_position = "bottomright")
par(mfrow = c(1, 1))
