


library(dplyr)

# set number of observations
n <- 10000
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
  mutate(click = ifelse(click_factor >= rand_draw, 1, 0)) %>%
  mutate(t = 1:n) %>%
  mutate(k = 3) %>%  # this is
  mutate(d = 2)      # stupid :D
