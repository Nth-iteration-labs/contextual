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

# Run regression per arm, predict outcomes, and save results, a column per arm

f                <- alive ~ age + risk + severity

model_f          <- function(arm) glm(f, data=data[trt==arm],
                                      family=binomial(link="logit"),
                                      y=FALSE, model=FALSE)
arms             <- sort(unique(data$trt))
model_arms       <- lapply(arms, FUN = model_f)

predict_arm      <- function(model) predict(model, data, type = "response")
r_data           <- lapply(model_arms, FUN = predict_arm)
r_data           <- do.call(cbind, r_data)
colnames(r_data) <- paste0("r", (1:max(arms)))

# Bind data and model predictions

data             <- cbind(data,r_data)

# calculate propensity weights

m      <- glm(I(trt-1) ~ age + risk + severity, data=data, family=binomial(link="logit"))
data$p <-predict(m, type = "response")

# run bandit - when leaving out P1, Doubly Robust Bandit uses marginal prob per arm for propensities:
# table(private$z)/length(private$z)

f          <- alive ~ trt | age + risk + severity | r1 + r2  | p

bandit     <- OfflineDoublyRobustBandit$new(formula = f, data = data)

# Define agents.
agents      <- list(Agent$new(LinUCBDisjointOptimizedPolicy$new(0.2), bandit, "LinUCB"))

# Initialize the simulation.

simulation  <- Simulator$new(agents = agents, simulations = simulations, horizon = horizon)

# Run the simulation.
sim  <- simulation$run()

# plot the results
plot(sim, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright")
plot(sim, type = "arms", limit_agents = "LinUCB")
