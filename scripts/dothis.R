library(dplyr)
library(broom)
library(MASS)
library(ggplot2)
library(purrr)
library(tidyr)
library(knitr)
# set number of observations
n <- 1000
# make this reproducible
set.seed(7)
# simulate some data!
bandit_data <- data_frame(
  clicked_sports = sample(c(0,1), n, prob = c(0.6, 0.4), replace = T),
  clicked_politics = sample(c(0,1), n, prob = c(0.7, 0.3), replace = T),
  arm = sample(c(1:3), n, replace =  T),
  sports_coef = case_when(arm == 1 ~ .5,
                          arm == 2 ~ .1,
                          arm == 3 ~ .1),
  politics_coef = case_when(arm == 1 ~ .1,
                            arm == 2 ~ .1,
                            arm == 3 ~ .4),
  arm_baseline = case_when(arm == 1 ~ .1,
                           arm == 2 ~ .2,
                           arm == 3 ~ .1),
  rand_draw = runif(n)
) %>%
  mutate(click_factor = arm_baseline + sports_coef * clicked_sports + politics_coef * clicked_politics) %>%
  mutate(click = ifelse(click_factor >= rand_draw, 1, 0))


bandit_data %>%
  group_by(arm, clicked_sports, clicked_politics) %>%
  summarise(ct = n(), reward = sum(click), mean_clk_rt = mean(click)) %>%
  group_by(clicked_sports, clicked_politics) %>%
  filter(mean_clk_rt == max(mean_clk_rt)) %>%
  kable()

alpha = 7

library(MASS)
# this function returns the ucb estimates or p_t_a from above
inside_for_func <- function(inverse_cov_matrix, reward_vector_times_design_matrix, context_vector, alpha){
  theta_hat <- inverse_cov_matrix %*% reward_vector_times_design_matrix
  ucb_estimate <- t(theta_hat) %*% context_vector +
    alpha * sqrt(t(context_vector) %*% inverse_cov_matrix %*% context_vector)
  return(ucb_estimate)
}
# This function updates the covariate matrix
update_cov_matrix <- function(cov_matrix, context_vector){
  return(cov_matrix + context_vector %*% t(context_vector))
}
# this one updates b_a from above
update_reward_vector_times_design_matrix <- function(reward_vector_times_design_matrix, reward, context_vector){
  return(reward_vector_times_design_matrix + reward * context_vector)
}


arms <- c(1:3)
d <- 2
arm_choice <- c()
cov_matrix <- list()
reward_vector_times_design_matrix <- list()
ucb_estimate <- matrix(0, n, length(arms))

for (t in 1:n){
  context <- bandit_data[t,]
  for (a in arms){
    if(t == 1){
      cov_matrix[[a]] <- diag(d)
      reward_vector_times_design_matrix[[a]] <- rep(0, d)
    }
    inverse_cov_matrix <- ginv(cov_matrix[[a]])
    ucb_estimate[t, a] <- inside_for_func(inverse_cov_matrix,
                                          as.matrix(reward_vector_times_design_matrix[[a]]),
                                          as.matrix(c(context$clicked_sports, context$clicked_politics)),
                                          alpha)

  }
  trial_arm <- which(ucb_estimate[t,] == max(ucb_estimate[t,]))
  if(length(trial_arm) > 1){
    trial_arm <- sample(trial_arm, 1)
  }
  if(trial_arm == context$arm){
    arm_choice[t] <- trial_arm
  }else{
    arm_choice[t] <- t*10 # need to do this so I can filter out unused observations from bandit dataset
    next
  }
  cov_matrix[[arm_choice[t]]] <- update_cov_matrix(cov_matrix[[arm_choice[t]]],
                                                   as.matrix(c(context$clicked_sports, context$clicked_politics)))
  reward_vector_times_design_matrix[[arm_choice[t]]] <- update_reward_vector_times_design_matrix(
    as.matrix(reward_vector_times_design_matrix[[arm_choice[t]]]),
    context$click,
    as.matrix(c(context$clicked_sports, context$clicked_politics))
  )
}
