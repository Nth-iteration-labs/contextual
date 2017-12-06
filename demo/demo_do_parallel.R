#library(contextual)
source("dev.R")
setwd("~/GitHub/contextual/demo")

ptm <- proc.time()
#Rprof ( tf <- "log.log",  memory.profiling = TRUE )

plot = Plot$new()                                                               # initialize plot.. TODO: change to within class
plot$set_external(T, 11, 6L)                                                    # set external for Rstudio
agents = list()                                                                 # to keep track of our agents
set.seed(21L)                                                                   # set seed, to be able to repeat our tests with the same data

bandit  = SyntheticBandit$new(
  k = 3L,
  d = 3L,
  weight_distribution = "Uniform",
  reward_family =       "Bernoulli",
  feature_type =        "Bernoulli"
)

                     #d1  #d2  #d3
bandit$set_weights(c(0.9, 0.0, 0.1,  #k1                                        # override auto-generated weights
                     0.1, 0.9, 0.1,  #k2                                        # d stands for a feature,
                     0.9, 0.1, 0.9)) #k3                                        # k for an arm

policyEG       = EpsilonGreedyPolicy$new(0.1, "\U190-greedy")                   # which policy do we want to test?
agents$EG      = BasicAgent$new(policyEG, bandit)                               # define an agent, who uses an policy, to find out more about a bandit

policyTS       = ThompsonSamplingPolicy$new(1.0, 1.0, "TSampling")              # which policy do we want to test?
agents$TS      = BasicAgent$new(policyTS, bandit)                               # define an agent, who uses an policy, to find out more about a bandit

policyRandom   = RandomPolicy$new("Random")                                     # which policy do we want to test?
agents$Random  = BasicAgent$new(policyRandom, bandit)                           # define an agent, who uses an policy, to find out more about a bandit

policyOracle   = OraclePolicy$new("Oracle")                                     # which policy do we want to test?
agents$Oracle  = BasicAgent$new(policyOracle, bandit)                           # define an agent, who uses an policy, to find out more about a bandit

policyExp3     = Exp3Policy$new(0.1, "Exp3")                                    # which policy do we want to test?
agents$Exp3    = Exp3Agent$new(policyExp3, bandit)                              # define an agent, who uses an policy, to find out more about a bandit

policyLinUCB   = LinUCBPolicy$new(1.0, "LinUCB")                                # which policy do we want to test?
agents$LinUCB  = LinUCBAgent$new(policyLinUCB, bandit)                          # define an agent, who uses an policy, to find out more about a bandit

simulations    = 100L                                                           # define how many simulations
horizon        = 100L                                                           # define how many each sim
simulation     = Simulator$new(agents , parallel = TRUE)                        # let's see what our cunning agent can find out about the bandit
history        = simulation$run(horizon, simulations)                           # go!

plot$grid(history)                                                              # plot the results...

#Rprof ( NULL ) ; print ( summaryRprof ( tf )  )
print(proc.time() - ptm)
