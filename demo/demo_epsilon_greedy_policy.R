library(contextual)


policy             <- EpsilonGreedyPolicy$new(epsilon = 0.1)

bandit             <- BasicBernoulliBandit$new(weights = c(0.6, 0.1, 0.1))
agent              <- Agent$new(policy,bandit)

simulator          <- Simulator$new(agents      = agent,
                                   horizon     = 100,
                                   simulations = 1000)


##  Option 1: save history object, extract data.table  -------------------------------------------------------

history            <- simulator$run()

plot(history, type = "cumulative", regret = TRUE, disp = "ci",
     traces_max = 100, traces_alpha = 0.1,
     traces = TRUE, smooth = FALSE, interval = 1)

summary(history)

data               <- history$get_data_table()

##  Option 2: when very big history log, don't copy it around ------------------------------------------------

simulator$run()

plot(simulator$history, type = "cumulative", regret = TRUE, disp = "ci",
     traces_max = 100, traces_alpha = 0.1,
     traces = TRUE, smooth = FALSE, interval = 1)

summary(simulator$history)

simulator$history$data
