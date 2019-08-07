amplitude_list <- c(0.15, 0.1, 0.075, 0.05, 0.025, 0.01)
iters <- length(amplitude_list)
reward_rate <- c()

for(i in 1:iters){
  print(i)
  print(history$cumulative[[i]]$cum_reward_rate)
  reward_rate[[i]] <- history$cumulative[[i]]$cum_reward_rate
}

plot(amplitude_list, reward_rate, xaxt = "n")
axis(1, at = amplitude_list)
