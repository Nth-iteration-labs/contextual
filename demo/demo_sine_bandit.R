library(contextual)

# Baded on a section of Dai Shi's thesis
# "Exploring Bandit Algorithms for Automatic Content Selection"

horizon            <- 600L
simulations        <- 300L

TwoArmedSineBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  public = list(
    sigma = NULL,
    class_name = "TwoArmedSineBandit",
    initialize  = function(k = 2, sigma = 0.2) {
      self$k = k
      self$sigma = sigma
    },
    get_context = function(t) {
      context     <- list(k = self$k)
    },
    get_reward = function(t, context, action) {
      rseq        <- seq(0,2, by = 2/self$k)[-(self$k+1)]
      sine        <- sapply(rseq,self$sine,t)
      reward      <- sine + rnorm(1, sd = self$sigma)
      reward      <- list(
        reward                   = reward[action$choice],
        optimal_reward           = sine[which_max_tied(sine)]
      )
    },
    sine = function(phi, t) {
      omega       <- 0.125; A <- 0.5; p <- 1.0;
      A * (sin(omega * pi * t /10 + phi * pi) + p)
    }
  )
)

bandit             <- TwoArmedSineBandit$new()

agents             <- list(Agent$new(Exp3Policy$new(0.1), bandit),
                           Agent$new(UCB1Policy$new(), bandit))

simulation         <- Simulator$new(agents, horizon = horizon, simulations = simulations)

history            <- simulation$run()

plot(history, type = "average", regret = FALSE, disp = "var", plot_only_disp = TRUE)
plot(history, type = "average", regret = TRUE, disp = "var", plot_only_disp = TRUE)
plot(history, type = "cumulative", disp = "var", rate = TRUE, plot_only_disp = TRUE)
plot(history, type = "average", regret = FALSE, disp = "var")
plot(history, type = "average", regret = TRUE, disp = "var")
plot(history, type = "cumulative", disp = "var", rate = TRUE)
