setwd("~/GitHub/contextual/demo")
source("dev.R")

horizon            <- 100L
simulations        <- 100L
weight_per_arm     <- c(0.9, 0.1, 0.1)

policy             <- EpsilonFirstPolicy$new(first = 50, name = "EpsilonFirst")
bandit             <- SyntheticBandit$new(weights = weight_per_arm, precache = FALSE)
agent              <- Agent$new(policy, bandit)

history            <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)$run()

plot(history, type = "grid")

plot(history, type = "arms")

############################################# LIFFFFFFFFFFFFFFF!

horizon            <- 1500
simulations        <- 100

continuum_of_arms  <- function(x) {
  -0.1*(x - 5) ^ 2 + 3.5  + rnorm(length(x),0,0.4)
}

int_time    <- 100
amplitude   <- 0.2
learn_rate  <- 0.3
omega       <- 2*pi/int_time
x0_start    <- 2.0

policy             <- LifPolicy$new(int_time, amplitude, learn_rate, omega, x0_start, name = "LiF")

bandit             <- ContinuumBandit$new(FUN = continuum_of_arms)

agent              <- Agent$new(policy,bandit)

history            <- Simulator$new(     agents = agent,
                                         horizon = horizon,
                                         simulations = simulations,
                                         save_theta = TRUE             )$run()

h <- history$get_data_table()

plot(history, type = "average", regret = FALSE)


##################### thinking on this #####################

x = seq(from = 0, to = 10, by = 1)
#error = rnorm(100,0,0.4)
#y = (-0.1*(x - 5) ^ 2 + 3.5) + error
continuum_arm      <- function(x) { -0.1*(x - 5) ^ 2 + 3.5  + rnorm(length(x),0,0.4) }   #####  can the error be set without ...
y = continuum_arm(x)
plot(x,y)
mod <- lm(y ~ x + I(x^2))

summary(mod)

sim_res = simulate(mod)
plot(x,sim_res$sim_1)

