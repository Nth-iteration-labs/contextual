# Load and attach the contextual package.
library(contextual)
# Define for how long the simulation will run.
horizon             <- 400
# Define how many times to repeat the simulation.
simulations         <- 1000
# Define the probability that each ad will be clicked.
click_probabilities <- c(0.8, 0.4, 0.1)
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
plot(history, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright", do_par = F )


#                                    +-----+----+----------->  arms:      three ads
#                                    |     |    |
click_probabilities <- matrix(  c(  0.8,  0.4, 0.1,     # -->  context 1: older (p=.5)
                                    0.4,  0.8, 0.1   ), # -->  context 2: young (p=.5)

                                  nrow = 2, ncol = 3, byrow = TRUE)

# Initialize a SyntheticBandit with the *contextual* click probabilities
context_bandit      <- SyntheticBandit$new(weights = click_probabilities)
# Initialize a contextual LinUCBDisjointPolicy with an alpha of 0.6
lu_policy           <- LinUCBDisjointPolicy$new(0.6)
# Initialize three Agents, binding each policy to a bandit.
ef_agent            <- Agent$new(ef_policy, context_bandit)
eg_agent            <- Agent$new(eg_policy, context_bandit)
lu_agent            <- Agent$new(lu_policy, context_bandit)
# Assign all agents to a list.
agents              <- list(ef_agent, eg_agent, lu_agent)
# Initialize a Simulator with the agent list, horizon, and number of simulations.
simulator           <- Simulator$new(agents, horizon, simulations)
# Now run the simulator.
history             <- simulator$run()
# And plot the cumulative reward rate again.
plot(history, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright", do_par = F )
