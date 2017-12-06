if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)

#system("R CMD Rd2pdf contextual")
source("../R/contextual.R")
source("../R/history.R")
source("../R/simulator.R")
source("../R/plot.R")
source("../R/bandit_synthetic.R")
source("../R/policy_linucb.R")
source("../R/policy_thompsonsampling.R")
source("../R/policy_epsilongreedy.R")
source("../R/policy_oracle.R")
source("../R/policy_random.R")
source("../R/policy_exp3.R")
source("../R/agent_basic.R")
source("../R/agent_linucb.R")
source("../R/agent_exp3.R")

# theta are all the variables in the policy (function) that we are looking to optimize

# add tests with TESTr

# TODO theta setup, some mem stuff
# assign <- not =
# output some info how far along, particularly if not chart
# add ways to do in batches
# for demo do manyprocessor testshow
# time calculating, O notation?
# make sure no object sees/accesses too much!
# do sapply or not exp3/ts
# UCB1, exp3, exp4, epoch..
# create data from existing
# online and offline bandits
# Then more interesting stuff from Maurits
# display baysian and ucb live changes
# add/remove arms
# changes over time
# documentation and article(s)
# parameter tuning?
# blog about all this, do JS versions

# theta memory - put whole reward object in there?
