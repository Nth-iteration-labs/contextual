# Load and attach the contextual package.
library(contextual)
# Define for how long the simulation will run.
horizon <- 400
# Define how many times to repeat the simulation.
simulations <- 10000
# Define the probability that each ad will be clicked.
click_probabilities <- matrix(c(0.6, 0.4, 0.2), nrow = 1, ncol = 3, byrow = TRUE)
# Initialize a SyntheticBandit, which takes probabilites per arm for an argument.
bandit <- ContextualBernoulliBandit$new(weights = click_probabilities)
# Initialize EpsilonGreedyPolicy with a 40% exploiration rate.
eg_policy <- EpsilonGreedyPolicy$new(epsilon = 0.4)
# Initialize EpsilonFirstPolicy with a .25 x 400 = 100 step exploration period.
ef_policy <- EpsilonFirstPolicy$new(epsilon = 0.25, N = horizon)
# Initialize two Agents, binding each policy to a bandit.
ef_agent <- Agent$new(ef_policy, bandit)
eg_agent <- Agent$new(eg_policy, bandit)
# Assign both agents to a list.

##################################################################################################
#                        +-----+----+----------->  arms:  three ads
#                        |     |    |
click_probs <- matrix(c(0.5,  0.7, 0.1,  # -> context 1: older (p=.5)
                        0.7,  0.1, 0.3), # -> context 2: young (p=.5)

                      nrow = 2, ncol = 3, byrow = TRUE)

# Initialize a SyntheticBandit with contextual weights
context_bandit <- ContextualBernoulliBandit$new(weights = click_probs)
# Initialize LinUCBDisjointPolicy
lucb_policy    <- LinUCBDisjointPolicy$new(0.6)
# Initialize three Agents, binding each policy to a bandit.
ef_agent       <- Agent$new(ef_policy,   context_bandit)
eg_agent       <- Agent$new(eg_policy,   context_bandit)
lucb_agent     <- Agent$new(lucb_policy, context_bandit)
# Assign all agents to a list.
agents <- list(ef_agent, eg_agent, lucb_agent)
# Initialize a Simulator with the agent list, horizon, and nr of simulations
simulator <- Simulator$new(agents, horizon, simulations)
# Now run the simulator.
history <- simulator$run()

par(mfrow = c(1, 2), mar = c(2,4,1,1) , cex=1.4)
# Finally, plot the average reward per time step t
plot(history, type = "average", regret = FALSE, no_par = TRUE, legend_border = FALSE, legend_position = "bottomright")
# And the cumulative reward rate, which equals the Click Through Rate)
plot(history, type = "cumulative", regret = FALSE, rate = TRUE, no_par = TRUE, legend_border = FALSE, legend_position = "bottomright")
par(mfrow = c(1, 1))
