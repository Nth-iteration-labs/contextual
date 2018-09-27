library(contextual)

horizon            <- 1000
simulations        <- 100

continuous_arms  <- function(x) {
  -0.1*(x - 5) ^ 2 + 3.5  + rnorm(length(x),0,0.4)
}

int_time    <- 100
amplitude   <- 0.2
learn_rate  <- 0.3
omega       <- 2*pi/int_time
x0_start    <- 2.0

policy             <- LifPolicy$new(int_time, amplitude, learn_rate, omega, x0_start)

bandit             <- ContinuumBandit$new(FUN = continuous_arms)

agent              <- Agent$new(policy,bandit)

history            <- Simulator$new(agents      = agent,
                                    horizon     = horizon,
                                    simulations = simulations)$run()

plot(history, type = "average", regret = FALSE)
