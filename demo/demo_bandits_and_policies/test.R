# Load and attach the contextual package.
library(contextual)

# Define for how long the simulation will run.
horizon             <- 300

# Define how many times to repeat the simulation.
simulations         <- 400

# Define the probability that each add will be clicked.
click_probabilities <- c(0.8, 0.2)

# Initialize a SyntheticBandit, which takes probabilites per arm for an argument.
bandit              <- SyntheticBandit$new(weights = click_probabilities)

# Initialize an EpsilonGreedyPolicy with a 20% exploiration rate.
eg_policy           <- EpsilonGreedyPolicy$new(epsilon = 0.2)

# Initialize an EpsilonFirstPolicy with a 50 step exploration period.
ef_policy           <- EpsilonFirstPolicy$new(first = 50)

# Initialize two Agents, binding each policy to a bandit.
ef_agent            <- Agent$new(ef_policy, bandit)
eg_agent            <- Agent$new(eg_policy, bandit)

# Assign both agents to a list.
agents              <- list(ef_agent, eg_agent)

# Initialize a Simulator with the agent list, horizon, and number of simulations.
simulator           <- Simulator$new(agents, horizon, simulations)

# Now run the simulator.
history             <- simulator$run()

# And plot the cumulative reward rate (equals Click Through Rate)
plot(history, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright")


# Define the probability that each add will be clicked per category

#                                   add1  add2
click_probabilities <- matrix(  c(  0.8,  0.2,     # context: older (p=.5)
                                    0.2,  0.8  ),  # context: young (p=.5)
                       nrow = 2, ncol = 2, byrow = TRUE)

# Initialize a SyntheticBandit
context_bandit      <- SyntheticBandit$new(weights = click_probabilities)

# Initialize a LinUCBDisjointPolicy with an alpha of 0.6
ef_agent            <- Agent$new(ef_policy, context_bandit)
eg_agent            <- Agent$new(eg_policy, context_bandit)
lud_policy          <- Agent$new(LinUCBDisjointPolicy$new(0.6), context_bandit)

# Assign all agents to a list.
agents              <- list(ef_agent, eg_agent, lud_policy)

# Initialize a Simulator with the agent list, horizon, and number of simulations.
simulator           <- Simulator$new(agents, horizon, simulations)

# Now run the simulator.
history             <- simulator$run()

# And plot the cumulative reward rate again.
plot(history, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright")




