library(contextual)

# Inspired by Dai Shi's thesis
# "Exploring Bandit Algorithms for Automatic Content Selection"

horizon     <- 600L
simulations <- 300L

TwoArmedSineBandit <- R6::R6Class(
   inherit = Bandit,
   portable = TRUE,
   class = FALSE,
   public = list(
     sigma = NULL,
     class_name = "TwoArmedSineBandit",
     initialize  = function(k = 2, sigma = 0.2) {
       self$k = k
       self$sigma = sigma
     },
     get_context = function(t) {
       context_list <- list(k = self$k)
     },
     get_reward = function(t, context, action) {
       rseq        <- seq(0,2, by = 2/self$k)[-(self$k+1)]
       reward      <- sapply(rseq,self$noisy_sine,t,self$sigma)
       rewardlist <- list(
         reward                   = reward[action$choice],
         optimal_reward           = reward[which.max(reward)]
       )
     },
     noisy_sine = function(phi, t, sigma) {
       omega <- 0.125; A <- 0.5; p <- 1.0;
       A * (sin(omega * pi * t /10 + phi * pi) + p) + rnorm(1, sd = sigma)
     }
   )
 )

bandit     <- TwoArmedSineBandit$new()

agents     <- list(Agent$new(Exp3Policy$new(0.1), bandit),
                   Agent$new(UCB1Policy$new(), bandit))

simulation <- Simulator$new(agents, horizon = horizon, simulations = simulations, progress_file = TRUE)

history    <- simulation$run()

plot(history, type = "average", regret = FALSE, disp = "var", plot_only_disp = TRUE)
plot(history, type = "average", regret = TRUE, disp = "var", plot_only_disp = TRUE)
plot(history, type = "cumulative", disp = "var", rate = TRUE, plot_only_disp = TRUE)
plot(history, type = "average", regret = FALSE, disp = "var")
plot(history, type = "average", regret = TRUE, disp = "var")
plot(history, type = "cumulative", disp = "var", rate = TRUE)
