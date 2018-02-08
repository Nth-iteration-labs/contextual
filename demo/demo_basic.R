#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

weights     <- c(0.9, 0.1, 0.1)

# model k * X , linear .. by default. can set differently..

bandit      <- SyntheticBandit$new(reward_family = "Bernoulli",
                                   data = weights,
                                   precache = T)

policy      <- EpsilonGreedyPolicy$new(0.05)

agent       <- Agent$new(policy, bandit)

simulation  <- Simulator$new(
                              agent,
                              horizon      = 100L,
                              simulations  = 300L,
                              save_context = FALSE,
                              save_theta   = FALSE,
                              do_parallel  = F
                            )

history     <- simulation$run()

plot(history, type = "grid")











#x1 = c(56.1, 26.8)
#x2 = c(12.29, 11.42)

#y = (x1 + x2)

#modb1 <- lm(y ~ x1 + x2)
#S3 <- glm.simulate(modb1, nsim = 4)
#
#x <- rnorm(15)
#y <- x + rnorm(15)
#predict(lm(y ~ x))


#x1 <- c(0.9, 0.1, 0.1)
#x2 <- c(0.9, 0.2, 0.3)
#y <- (x1 + x2) / 2
#model <- glm(y ~ x1 + x2, family = binomial)
#predict(model)
#simulate(model, nsim = 4)

#x <- matrix(c(0.9, 0.1, 0.1,0.8, 0.2, 0.2), 2, 3)
#mod <- lm(c(0.9, 0.1, 0.1) ~ x)
#simulate(mod, nsim = 4)



