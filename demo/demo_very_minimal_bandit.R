########################### package dev helpers ################################

#library(contextual)
# setwd("~/GitHub/contextual/demo")
# source("dev.R")
#
# set.seed(21)
#
# ptm <- proc.time()
#
# bandit      <- BasicBandit$new()
#
# bandit      <- BasicBandit$new()
# bandit$set_weights(c(0.1, 0.9))
# policy      <- EpsilonGreedyPolicy$new()
# agent       <- Agent$new(policy, bandit)
# simulation  <- Simulator$new(agent, horizon = 100L, simulations = 100L)
# history     <- simulation$run()
#
# plot = Plot$new()
# plot$grid(history)

############

## first time with context and theta
setwd("~/GitHub/contextual/demo")
source("dev.R")

bandit      <- SyntheticBandit$new()
                            #k1  #k2  #k3
bandit$set_weights(matrix(c(0.9, 0.1, 0.1,  #d1
                            0.1, 0.2, 0.1,  #d2
                            0.2, 0.1, 0.2), #d3
                          nrow = 3L,
                          ncol = 3L))
policy      <- EpsilonGreedyPolicy$new()
agent       <- Agent$new(policy, bandit)
simulation  <-
  Simulator$new(
    agent,
    horizon = 30L,
    simulations = 30L,
    worker_max = 1
  )
history     <- simulation$run()

#Plot$new()$grid(history)

reward = list()
reward["reward"]  = 10
reward["optimal"] = 0

action = list()
action$arm = 2
action$propensity = 1

#history$save(
#  counter = 1,
#  t = 1,
#  action = action,
#  reward = reward,
#  policy_name = "EpsilonGreedy",
#  s = 31 ################################################ that destroys the plot :D
#)

print(history$data$reward[1])

history$save(
  index = 1,
  t = 30,
  action = action,
  reward = reward,
  policy_name = "EpsilonGreedy",
  s = 30
)

print(history$data$reward[1])

history$save_data("test.RData")
#expect_true(file.exists(test.RData))
history$reset()
history$load_data("test.RData")
file.remove("test.RData")

print(history$data$reward[1])

df <- history$get_data_frame()
history$reset()
history$set_data_frame(df)

print(history$data$reward[1])

dt <- history$get_data_table()
history$reset()
history$set_data_table(dt)

print(history$data$reward[1])

print(nrow(history$data))
history$save(                       ### store_state , plus one that saves agent by arg agent object
  index = 30,
  t = 0,
  action = action,
  reward = reward,
  policy_name = "EpsilonGreedy",
  s = 0
)
history$delete_empty_rows()
print(history$data$context[1])
print(nrow(history$data))

##  with theta.. ######################################################################

bandit      <- SyntheticBandit$new()
bandit$set_weights(matrix(c(0.9, 0.1, 0.1,  #k1
                            0.1, 0.2, 0.1,  #k2
                            0.2, 0.1, 0.2), #k3
                          nrow = 3L,
                          ncol = 3L))
policy      <- EpsilonGreedyPolicy$new()
agent       <- Agent$new(policy, bandit)
simulation  <-
  Simulator$new(
    agent,
    horizon = 30L,
    simulations = 30L,
    worker_max = 1,
    save_context = TRUE,
    save_theta = TRUE
  )
history     <- simulation$run()

reward = list()
reward["reward"]  = 10
reward["arm"] = 1
reward["optimal"] = 0
action = list()
action$arm = 2

print(unlist(history$data$context[1]))

history$save(
  index = 1,
  t = 30,
  action = action,
  reward = reward,
  policy_name = "EpsilonGreedy",
  s = 30
)

print(unlist(history$data$context[1]))

history$save_data("test.RData")
#expect_true(file.exists(test.RData))
history$reset()
history$load_data("test.RData")
file.remove("test.RData")

print(unlist(history$data$context[1]))

df <- history$get_data_frame()
history$reset()
history$set_data_frame(df)

print(unlist(history$data$context[1]))

dt <- history$get_data_table()
history$reset()
history$set_data_table(dt)

print(unlist(history$data$context[1]))

print(nrow(history$data))
history$save(                                                             ### store_state , plus one that saves agent by arg agent object
  index = 30,                                                                   ### call this not counter but index..
  t = 0,
  action = action,
  reward = reward,
  policy_name = "EpsilonGreedy",
  s = 0
)
history$delete_empty_rows()
print(unlist(history$data$context[1]))                                          ## unlist .. should return unlisted?
print(history$data$theta[[1]][[2]]$chosen)
print(nrow(history$data))

h <- history$get_data_frame()



