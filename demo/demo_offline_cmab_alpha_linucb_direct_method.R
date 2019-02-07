library(contextual)
library(data.table)

# Import personalization data-set

url         <- "http://d1ie9wlkzugsxr.cloudfront.net/data_cmab_basic/dataset.txt"
data        <- fread(url)

simulations <- 1
horizon     <- nrow(data)

# Run regression per arm, predict outcomes, and save results, a column per arm

x                <- reformulate(names(data)[3:102],response="V2")       # x: V3 .. V102 | y: V2
f                <- as.Formula(x)                                       # y ~ x

model_f          <- function(arm) glm(f, data=data[V1==arm], family=binomial(link="logit"), y=F, model=F)
arms             <- sort(unique(data$V1))
model_arms       <- lapply(arms, FUN = model_f)

predict_arm      <- function(model) predict(model, data, type = "response")
r_data           <- lapply(model_arms, FUN = predict_arm)
r_data           <- do.call(cbind, r_data)
colnames(r_data) <- paste0("R", (1:max(arms)))

# Bind data and model predictions

data             <- cbind(data,r_data)

# Run direct method style offline bandit

x                <- reformulate(names(data)[3:102],response="V2")       # x: V3 .. V102 | y: V2
z                <- ~ V1                                                # z: V1
r                <- ~ R1+R2+R3+R4+R5+R6+R7+R8+R9+R10                    # r: R1 .. R10

f                <- as.Formula(z,x,r)                                   # y ~ z | x | r

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
