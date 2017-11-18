library(MASS)

### Adding another comment
### Adding a comment
######################################################
### Three central functions within main loop
######################################################

get_theta_hat_alpha <- function(b_rec, A_rec, B_rec, beta_hat){
  return(ginv(A_rec) %*% (b_rec - B_rec %*% beta_hat))
}

get_s_t_rec <- function(z_t_rec, A_0, B_rec, A_rec, x_t_rec){
  A_0_inv <- ginv(A_0)
  A_rec_inv <- ginv(A_rec)
  s_t_rec <- (t(z_t_rec) %*% A_0_inv %*% z_t_rec) -
    2*(t(z_t_rec) %*% A_0_inv %*% t(B_rec) %*% A_rec_inv %*% x_t_rec) +
    (t(x_t_rec) %*% A_rec_inv %*% x_t_rec) +
    (t(x_t_rec) %*% A_rec_inv %*% B_rec %*% A_0_inv %*% t(B_rec) %*% A_rec_inv %*% x_t_rec)
  return(s_t_rec)
}

get_p_t_rec <- function(z_t_rec, beta_hat, x_t_rec, theta_hat_alpha, alpha, s_t_rec){
  p_t_rec <- (t(z_t_rec) %*% beta_hat) +
    (t(x_t_rec) %*% theta_hat_alpha) +
    alpha * sqrt(s_t_rec)
  return(p_t_rec)
}

######################################################
### Update shtuff
######################################################

update_A_0 <- function(A_0, B_rec, A_rec){
  A_rec_inv <- ginv(A_rec)
  New_A_0 <- A_0 + (t(B_rec) %*% A_rec_inv %*% B_rec)
  return(New_A_0)
}

update_b_0 <- function(b_0, B_rec, A_rec, b_rec){
  A_rec_inv <- ginv(A_rec)
  new_b_0 <- b_0 + (t(B_rec) %*% A_rec_inv %*% b_rec)
  return(new_b_0)
}

update_A_rec <- function(A_rec, x_t_rec){
  return(A_rec + x_t_rec %*% t(x_t_rec))
}

update_B_rec <- function(B_rec, x_t_rec, z_t_rec){
  return(B_rec + x_t_rec %*% t(z_t_rec))
}

update_b_rec <- function(b_rec, r_t, x_t_rec){
  return(b_rec + r_t * x_t_rec)
}

update_A_0_again <- function(A_0, z_t_rec, B_rec, A_rec){
  A_rec_inv <- ginv(A_rec)
  A_0_new <- A_0 + (z_t_rec %*% t(z_t_rec)) - (t(B_rec) %*% A_rec_inv %*% B_rec)
  return(A_0_new)
}

update_b_0_again <- function(b_0, r_t, z_t_rec, B_rec, A_rec, b_rec){
  A_rec_inv <- ginv(A_rec)
  new_b_0 <- b_0 +
    (r_t * z_t_rec) -
    (t(B_rec) %*% A_rec_inv %*% b_rec)
  return(new_b_0)
}

######################################################
### Test this thing!
######################################################
library(dplyr)

user_data <- data_frame(
  age = sample(18:70, 1000, replace = T),
  gender = sample(c(0, 1), 1000, replace = T)
)

article_data <- data_frame(
  name = c(1:5),
  type = c(1,1,1,2,2),
  var = c(1,2,2,2,2)
)


# Trials, the number of trials
trials <- 1000
# arms TODO: fix this so arms can vary
arms <- 5
# alpha, arbitarily set to 3
alpha <- 3
# dimensions of the single arm features, d
d <- 3
# dimensions of the shared features, k
k <- 5
# A0, a k-dimensional identity matrix
A_0 <- diag(k)
# B0, a k-length zero vector
b_0 <- matrix(0, k, 1)
# A, list of matrices for each arm
A <- list()
# B, list of matrices for each arm
B <- list()
# b, list of vectors for each arm
b <- list()
#theta, list to hold theta hat
theta <- list()
#s_t_a matrix
s_t_a <- matrix(0, trials, arms)
p_t_a <- matrix(0, trials, arms)
#arm choice vector
arm_choice <- c()
#reward vector
reward <- c()

for (t in 1:trials){

  beta_hat <- ginv(A_0) %*% b_0

  for (a in 1:arms){

    if(t == 1){
      A[[a]] <- diag(d)
      B[[a]] <- matrix(0, d, k)
      b[[a]] <- matrix(0, d, 1)
    }

    theta[[a]] <- get_theta_hat_alpha(b[[a]], A[[a]], B[[a]], beta_hat)

    z_t_rec <- matrix(as.numeric(c(user_data[t,], article_data[a,])), 5, 1)
    x_t_rec <- matrix(as.numeric(c(user_data[t,], a)), 3, 1)

    s_t_a[[t, a]] <- get_s_t_rec(z_t_rec, A_0, B[[a]], A[[a]], x_t_rec)
    p_t_a[[t, a]] <- get_p_t_rec(z_t_rec, beta_hat, x_t_rec, theta[[a]], alpha, s_t_a[[t, a]])

  }

  arm_choice[t] <- which(p_t_a[t,] == max(p_t_a[t,]))
  #changing this reward policy will cause the alg to solve differently (meaning this works!!!!)
  reward[t] <- ifelse(z_t_rec[2] == 0, ifelse(sqrt(arm_choice[t])*runif(1) > 1, 1, 0),
                      ifelse(1/(arm_choice[t])*runif(1) > .4, 1, 0))

  z_t_rec <- matrix(as.numeric(c(user_data[t,], article_data[arm_choice[t],])), 5, 1)
  x_t_rec <- matrix(as.numeric(c(user_data[t,], arm_choice[t])), 3, 1)

  A_0 <- update_A_0(A_0, B[[arm_choice[t]]], A[[arm_choice[t]]])
  b_0 <- update_b_0(b_0, B[[arm_choice[t]]], A[[arm_choice[t]]], b[[arm_choice[t]]])
  A[[arm_choice[t]]] <- update_A_rec(A[[arm_choice[t]]], x_t_rec)
  B[[arm_choice[t]]] <- update_B_rec(B[[arm_choice[t]]], x_t_rec, z_t_rec)
  b[[arm_choice[t]]] <- update_b_rec(b[[arm_choice[t]]], reward[t], x_t_rec)
  A_0 <- update_A_0_again(A_0, z_t_rec, B[[arm_choice[t]]], A[[arm_choice[t]]])
  b_0 <- update_b_0_again(b_0, reward[t], z_t_rec, B[[arm_choice[t]]], A[[arm_choice[t]]], b[[arm_choice[t]]])

}


dat <- data.frame(arm_choice, reward)
dat <- cbind(dat, user_data)
dat %>%
  group_by(arm_choice, gender) %>%
  summarise(ct = n(), r = sum(reward)) %>%
  mutate(prop = r/ct)

dat %>%
  inner_join(article_data, by = c("arm_choice" = "name")) -> dat

reg <- lm(reward ~ arm_choice + age + gender + type + var, dat)
summary(reg)

