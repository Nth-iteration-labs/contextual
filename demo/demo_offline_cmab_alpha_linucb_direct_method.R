library(contextual)
library(data.table)
library(Formula)

# Import personalization data-set

data         <- fread("http://pwy.nl/d") # 0/1 reward, 10 arms, 100 features
                                         # arms always start from 1

#      z y x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15  .. x100
#   1: 2 0  5  0  0 37  6  0  0  0  0  25   0   0   7   1   0  ..    0
#   2: 8 0  1  3 36  0  0  0  0  0  0   0   0   1   0   0   0  ..   10
#   3: . .  .  .  .  .  .  .  .  .  .   .   .   .   .   .   .  ..    .

simulations <- 1
horizon     <- nrow(data)

# Run regression per arm, predict outcomes, and save results, a column per arm

x                <- reformulate(names(data)[3:102],response="y")     # x: x1 .. x100
f                <- Formula::as.Formula(x)                           # y ~ x

model_f          <- function(arm) glm(f, data=data[z==arm], family=binomial(link="logit"), y=F, model=F)
arms             <- sort(unique(data$z))
model_arms       <- lapply(arms, FUN = model_f)

predict_arm      <- function(model) predict(model, data, type = "response")
r_data           <- lapply(model_arms, FUN = predict_arm)
r_data           <- do.call(cbind, r_data)
colnames(r_data) <- paste0("r", (1:max(arms)))

# Bind data and model predictions

data             <- cbind(data,r_data)

# Run direct method style offline bandit

x                <- reformulate(names(data)[3:102], response="y")
z                <- ~ z
r                <- ~ r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9 + r10

f                <- as.Formula(z,x,r)    # Resulting in: y ~ z | x1 + x2 .. | r1 + r2 + ..

bandit           <- OfflineDirectMethodBandit$new(formula = f, data = data)

# Define agents.
agents      <- list(Agent$new(LinUCBDisjointOptimizedPolicy$new(0.01), bandit, "alpha = 0.01"),
                    Agent$new(LinUCBDisjointOptimizedPolicy$new(0.05), bandit, "alpha = 0.05"),
                    Agent$new(LinUCBDisjointOptimizedPolicy$new(0.1),  bandit, "alpha = 0.1"),
                    Agent$new(LinUCBDisjointOptimizedPolicy$new(1.0),  bandit, "alpha = 1.0"))

# Initialize the simulation.

simulation  <- Simulator$new(agents = agents, simulations = simulations, horizon = horizon)

# Run the simulation.
sim  <- simulation$run()

# plot the results
plot(sim, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright", ylim = c(0,1))
