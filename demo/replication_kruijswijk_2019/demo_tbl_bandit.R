# This is the demo file for the online and offline parameter tuning of Thompson sampling
# for Bayesian linear regression. For policy and bandit specific code, please look at
# the files (as sourced above). First make sure to install contextual
# (see https://github.com/Nth-iteration-labs/contextual for a how to).
# 
# For any questions, please contact the authors.

library(contextual)
library(here)
library(ggplot2)

source("./bandit_continuum_function_unimodal.R")
source("./bandit_continuum_function_bimodal.R")
source("./bandit_continuum_offon.R")
source("./bandit_continuum_offon_kern.R")
source("./policy_tbl.R")

#############################################################
#                                                           #
#  Online evaluation for TBL                                #
#                                                           #
#############################################################

### Set seed
set.seed(1)

### Set number of interactions (horizon) and number of repeats (simulations)
### In the paper we used a horizon of 10000 and 10000 simulations
horizon            <- 10000
simulations        <- 10

### Set TBL specific parameters
J <- matrix(c(5, 4, -4), nrow=1, ncol=3, byrow = TRUE)
err <- 1
precision_list <- list(matrix(diag(c(0.01,0.01,0.02)), nrow=3, ncol=3, byrow = TRUE),
                       matrix(diag(c(0.1,0.1,0.2)), nrow=3, ncol=3, byrow = TRUE),
                       matrix(diag(c(0.4,0.4,1)), nrow=3, ncol=3, byrow = TRUE),
                       matrix(diag(c(1,1,2)), nrow=3, ncol=3, byrow = TRUE),
                       matrix(diag(c(2,2,5)), nrow=3, ncol=3, byrow = TRUE),
                       matrix(diag(c(10,10,20)), nrow=3, ncol=3, byrow = TRUE))

### Set up two different bandits
bandits <- c(ContinuumBanditUnimodal$new(), ContinuumBanditBimodal$new())

### Set up all agents with different prior precisions and run them for each bandit
for (bandit in bandits){
  
  agents <- list()
  
  for (prec in precision_list){
    agents <- append(agents, Agent$new(ThompsonBayesianLinearPolicy$new(J = J, P = prec, err = err), bandit))
  }
  
  history            <- Simulator$new(agents      = agents,
                                      horizon     = horizon,
                                      simulations = simulations,
                                      do_parallel = TRUE,
                                      save_interval = 10)$run()
  
  ### Post-processing for plotting
  iters <- length(precision_list)
  reward_rate <- c()
  confs <- c()
  
  for(i in 1:iters){
    reward_rate[[i]] <- history$cumulative[[i]]$cum_reward_rate
    confs[[i]] <- history$cumulative[[i]]$cum_reward_rate_sd / sqrt(simulations) * qnorm(0.975)
  }
  
  df <- data.frame(prec = 1:length(precision_list), reward = reward_rate, ci = confs, delta = rep("Online"))
  g <- ggplot(data = df, aes(x=prec, y=reward)) +
    geom_line(colour = 'red') +
    geom_errorbar(aes(ymin=reward-ci, ymax=reward+ci, color='red'), width=.2) +
    labs(x = "Prior precision", y = "Average reward per interaction") +
    scale_x_continuous(breaks = c(1,6), labels=c("Low", "High")) +
    theme_bw(base_size = 15) + 
    theme(legend.position = "none")
  
  ### Saving data to use later
  if (bandit$class_name == "ContinuumBanditBimodal"){
    online_tbl_bimodal <- df
  } else if (bandit$class_name == "ContinuumBanditUnimodal"){
    online_tbl_unimodal <- df
  }
  
  print(g)
  print(precision_list[[which.max(reward_rate)]])
}

#############################################################
#                                                           #
#  Offline evaluation for TBL                               #
#                                                           #
#############################################################

### Set seed
set.seed(1)

### Set number of interactions (horizon) and number of repeats (simulations)
### Typically same as in online evaluation
horizon            <- 1000
simulations        <- 2

### Set TBL specific parameters
### Typically same as in online evaluation
J <- matrix(c(5, 4, -4), nrow=1, ncol=3, byrow = TRUE)
err <- 1
precision_list <- list(matrix(diag(c(0.01,0.01,0.02)), nrow=3, ncol=3, byrow = TRUE),
                       matrix(diag(c(0.1,0.1,0.2)), nrow=3, ncol=3, byrow = TRUE),
                       matrix(diag(c(0.4,0.4,1)), nrow=3, ncol=3, byrow = TRUE),
                       matrix(diag(c(1,1,2)), nrow=3, ncol=3, byrow = TRUE),
                       matrix(diag(c(2,2,5)), nrow=3, ncol=3, byrow = TRUE),
                       matrix(diag(c(10,10,20)), nrow=3, ncol=3, byrow = TRUE))

### Set up functions to make offline dataset
unimodal_data    <- function(x) {
  c1 <- runif(1, 0.25, 0.75)
  c2 <- 1
  return(-(x - c1) ^ 2 + c2  + rnorm(length(x), 0, 0.01))
}

bimodal_data <- function(x){
  mu1 <- runif(1, 0.15, 0.2)
  sd1 <- runif(1, 0.1, 0.15)
  mu2 <- runif(1, 0.7, 0.85)
  sd2 <- runif(1, 0.1, 0.15)
  y1 <- truncnorm::dtruncnorm(x, a=0, b=1, mean=mu1, sd=sd1)
  y2 <- truncnorm::dtruncnorm(x, a=0, b=1, mean=mu2, sd=sd2)
  return(y1 + y2 + rnorm(length(x), 0, 0.01))
}

functions <- list(list("unimodal", unimodal_data), list("bimodal", bimodal_data))

### Set up different delta's for the delta and kernel method. If delta = 0 we resort to the kernel method.
deltas <- c(0, 0.01, 0.1, 0.5)

### Pre-allocation
offline_tbl_unimodal_kernel <- data.frame()
offline_tbl_unimodal <- data.frame()
offline_tbl_bimodal_kernel <- data.frame()
offline_tbl_bimodal <- data.frame()

### Set up all agents with different prior precisions and run them for each bandit
### Do this for each specified delta
for (f in functions){
  for (d in deltas){
    if(d == 0){
      bandit             <- OnlineOfflineContinuumBanditKernel$new(FUN = f[[2]], horizon = horizon)
    } else {
      bandit             <- OnlineOfflineContinuumBandit$new(FUN = f[[2]], delta = d, horizon = horizon)
    }
    
    agents <- list()
    
    for (prec in precision_list){
      agents <- append(agents, Agent$new(ThompsonBayesianLinearPolicy$new(J = J, P = prec, err = err), bandit))
    }
    
    history            <- Simulator$new(agents      = agents,
                                        horizon     = horizon,
                                        simulations = simulations,
                                        policy_time_loop = FALSE,
                                        save_interval = 20)$run()
    
    ### Post-processing for plotting
    iters <- length(precision_list)
    reward_rate <- c()
    confs <- c()
    
    for(k in 1:iters){
      reward_rate[[k]] <- history$cumulative[[k]]$cum_reward_rate
      dt <- history$get_data_table()
      df_split <- split(dt, dt$agent)
      for(dd in df_split){
        dd <- as.data.table(dd)
        maxes <- dd[, .I[which.max(t)], by=sim]$V1
        select <- dd[maxes]$cum_reward_rate
        confs[[k]] <- sd(select) / sqrt(simulations) * qnorm(0.975)
      }
    }
    
    history$clear_data_table()
    if (d == 0){
      df <- data.frame(prec = 1:length(precision_list), reward = reward_rate, delta = as.factor(rep("Kernel")), ci = confs)
    } else {
      df <- data.frame(prec = 1:length(precision_list), reward = reward_rate, delta = as.factor(rep(d)), ci = confs)
    }
    
    if (f[[1]] == "bimodal"){
      if (d == 0){
        offline_tbl_bimodal_kernel <- rbind(offline_tbl_bimodal_kernel, df)
      } else {
        offline_tbl_bimodal <- rbind(offline_tbl_bimodal, df)
      }
    } else if(f[[1]] == "unimodal"){
      if (d == 0){
        offline_tbl_unimodal_kernel <- rbind(offline_tbl_unimodal_kernel, df)
      } else {
        offline_tbl_unimodal <- rbind(offline_tbl_unimodal, df)
      }
    }
  }
}

### Plotting both online and offline data together
different_plots <- list(
  list("unimodal", rbind(online_tbl_unimodal, offline_tbl_unimodal)),
  list("unimodal_kernel", offline_tbl_unimodal_kernel),
  list("bimodal", rbind(online_tbl_bimodal, offline_tbl_bimodal)),
  list("bimodal_kernel", offline_tbl_bimodal_kernel)
)

for (dif_plot in different_plots){
  if(dif_plot[[1]] == "unimodal_kernel"){
    g <- ggplot(data = dif_plot[[2]], aes(x=prec, y=reward, label = delta)) +
      geom_line(aes(colour = as.factor(delta))) +
      geom_errorbar(data = dif_plot[[2]], aes(ymin=reward-ci, ymax=reward+ci, color=as.factor(delta)), width=.2) +
      geom_vline(xintercept = 4, linetype = "dotted", color = "black", size = 1.5) +
      theme(legend.position = "right") + #"none"
      labs(x = "Prior precision", y = "Average reward per interaction", color="", fill="") +
      scale_x_continuous(breaks = c(1,6), labels=c("Low", "High")) +
      theme_bw(base_size = 15)
    ggsave(g, file=paste0("offline_tbl_function_",dif_plot[[1]],".eps"), device="eps")
    print(g)
  } else if(dif_plot[[1]] == "bimodal_kernel"){
    g <- ggplot(data = dif_plot[[2]], aes(x=prec, y=reward, label = delta)) +
      geom_line(aes(colour = as.factor(delta))) +
      geom_errorbar(data = dif_plot[[2]], aes(ymin=reward-ci, ymax=reward+ci, color=as.factor(delta)), width=.2) +
      geom_vline(xintercept = 4, linetype = "dotted", color = "black", size = 1.5) +
      theme(legend.position = "right") + #"none"
      labs(x = "Prior precision", y = "Average reward per interaction", color="", fill="") +
      scale_x_continuous(breaks = c(1,6), labels=c("Low", "High")) +
      theme_bw(base_size = 15)
    ggsave(g, file=paste0("offline_tbl_function_",dif_plot[[1]],".eps"), device="eps")
    print(g)
  } else if(dif_plot[[1]] == "unimodal" || dif_plot[[1]] == "bimodal") {
    g <- ggplot(data = dif_plot[[2]], aes(x=prec, y=reward, label = delta)) +
      geom_line(aes(colour = as.factor(delta))) +
      geom_errorbar(data = dif_plot[[2]], aes(ymin=reward-ci, ymax=reward+ci, color=as.factor(delta)), width=.2) +
      theme(legend.position = "right") + #"none"
      labs(x = "Prior precision", y = "Average reward per interaction", color="", fill="") +
      scale_x_continuous(breaks = c(1,6), labels=c("Low", "High")) +
      theme_bw(base_size = 15)
    ggsave(g, file=paste0("offline_tbl_function_",dif_plot[[1]],"_delta.eps"), device="eps")
    print(g)
  }
}