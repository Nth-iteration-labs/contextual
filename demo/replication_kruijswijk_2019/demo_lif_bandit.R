library(contextual)
library(here)
setwd(here("demo","replication_kruijswijk_2019"))

#source("./bandit_continuum_function_hardcoded.R")
source("./bandit_continuum_function.R")
source("./policy_tbl.R")
source("./policy_unifcont.R")
source("./policy_efirst_regression.R")
source("./policy_cont_lif_randstart.R")


set.seed(1)

horizon            <- 10000
simulations        <- 50

continuous_arms_standard    <- function(x, c1 = 0.25, c2 = 0.75) {
  -(x - c1) ^ 2 + c2  + rnorm(length(x), 0, 0.01)
}

continuous_arms    <- function(x) {
  c1 <- runif(1, 0.25, 0.75)
  #c2 <- runif(1, 0.25, 0.75)
  c2 <- 1
  -(x - c1) ^ 2 + c2  + rnorm(length(x), 0, 0.01)
}

int_time           <- 50
amplitude          <- 0.025
amplitude_list     <- seq(0.002, 0.2, length.out = 20)
#amplitude_list     <- c(0.15, 0.1, 0.075, 0.05, 0.025, 0.01)
learn_rate         <- 1
omega              <- 1#2*pi/int_time
x0_start           <- runif(1)#2.0

bandit             <- ContinuumBandit$new(FUN = continuous_arms)

agents <- list()

for (i in 1:length(amplitude_list)){
  agents <- append(agents, Agent$new(LifPolicyRandstart$new(int_time, amplitude_list[i], learn_rate, omega), bandit))
}

# agents              <- list(Agent$new(UniformRandomContinuousPolicy$new(), bandit),
#                             Agent$new(ThompsonBayesianLinearPolicy$new(), bandit),
#                             Agent$new(LifPolicy$new(int_time, amplitude, learn_rate, omega, x0_start), bandit),
#                             Agent$new(EFirstRegressionPolicy$new(epsilon = 2000), bandit))

history            <- Simulator$new(agents      = agents,
                                    horizon     = horizon,
                                    simulations = simulations)$run()

plot(history, type = "cumulative", regret = FALSE, rate = TRUE, disp = "ci",
     legend_position = 'bottomright',
     legend_labels = amplitude_list
)

iters <- length(amplitude_list)
reward_rate <- c()

for(i in 1:iters){
  #print(i)
  #print(history$cumulative[[i]]$cum_reward_rate)
  reward_rate[[i]] <- history$cumulative[[i]]$cum_reward_rate
}

#plot(amplitude_list, reward_rate, xaxt = "n")
#axis(1, at = amplitude_list)
plot(amplitude_list, reward_rate)
df <- data.frame(amp = amplitude_list, reward = reward_rate)
ggplot(data = df, aes(x=amp, y=reward)) + geom_line(colour = 'red')
