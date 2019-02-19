library(contextual)
library(data.table)

# Import myocardial infection dataset

url  <- "http://d1ie9wlkzugsxr.cloudfront.net/data_propensity/myocardial_propensity.csv"
data            <- fread(url)

simulations     <- 3000
horizon         <- nrow(data)

# arms always start at 1
data$trt        <- data$trt + 1

# turn death into alive, making it a reward
data$alive      <- abs(data$death - 1)

# calculate propensity weights

m      <- glm(I(trt-1) ~ age + risk + severity, data=data, family=binomial(link="logit"))
data$p <- predict(m, type = "response")

# run bandit - when leaving out p, Propensity Bandit uses marginal prob per arm for propensities:
# table(private$z)/length(private$z)

f          <- alive ~ trt | age + risk + severity | p

bandit     <- OfflinePropensityWeightingBandit$new(formula = f, data = data, threshold = 0.01)

# Define agents.
agents      <- list(Agent$new(LinUCBDisjointOptimizedPolicy$new(0.2), bandit, "LinUCB"))

# Initialize the simulation.

simulation  <- Simulator$new(agents = agents, simulations = simulations, horizon = horizon)

# Run the simulation.
sim  <- simulation$run()

# plot the results
plot(sim, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright")
