# here, we did setweights, can also generate based on d/k, but check if either
setwd("~/GitHub/contextual/demo")
source("dev.R")

bandit <- SyntheticBandit$new(
  weight_distribution = "Uniform",
  reward_type         = "Bernoulli",
  seed                = 1,
  precache            = TRUE
)
                            #d1  #d2  #d3
bandit$set_weights(matrix(c(0.9, 0.1, 0.1,  #k1
                            0.1, 0.2, 0.1,  #k2
                            0.2, 0.1, 0.2), #k3
                            nrow = 3L,
                            ncol = 3L))

agents <- list(
  Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
  Agent$new(RandomPolicy$new("Random"), bandit),
  Agent$new(OraclePolicy$new("Oracle"), bandit),
  Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
  Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit)
)

ptm            <- proc.time()

simulation     <- Simulator$new(
  agents,
  horizon      = 100L,
  simulations  = 100L,
  save_context = TRUE,
  save_theta   = FALSE,
  worker_max   = 7
)

history        <- simulation$run()
h              <- history$get_data_table()

print(proc.time() - ptm)

plot <- Plot$new()
plot$grid(history)

simulation$object_size()
