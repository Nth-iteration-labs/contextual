if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)

#system("R CMD Rd2pdf contextual")

source("../R/utility.R")
source("../R/history.R")
source("../R/simulator.R")
source("../R/plot.R")
source("../R/bandit.synthetic.R")
source("../R/policy.linucb.R")
source("../R/policy.thompsonsampling.R")
source("../R/policy.epsilongreedy.R")
source("../R/policy.oracle.R")
source("../R/policy.random.R")
source("../R/policy.exp3.R")
source("../R/agent.basic.R")
source("../R/agent.linucb.R")
source("../R/agent.exp3.R")

# MAB Bandit
# bandit  = SyntheticBandit$new(k = 3L, d = 1, "uniform", "binary", "single")  # define a bandit, with k arms and d features
# bandit$set.weights( c(0.1,
#                       0.9,
#                       0.1) )

# theta are all the variables in the policy (function) that we are looking to optimize

# TODO theta setup, some mem stuff
# output some info how far along, particularly if not chart
# add ways to do in batches
# for demo do manyprocessor testshow
# time calculating, O notation?
# make sure no object sees/accesses too much!
# mult and div apply
# do sapply or not exp3/ts
# UCB1, exp3, exp4, epoch..
# create data from existing
# online and offline bandits
# Then more interesting stuff from Maurits
# display baysian and ucb live changes
# add/remove arms
# changes over time
# documentation and article(s)
# parallelization
# parameter tuning?
# blog about all this, do JS versions

# theta memory - put whole reward object in there?
