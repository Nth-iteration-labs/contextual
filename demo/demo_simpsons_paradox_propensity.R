library(contextual)

# ------------------------------------------------------------------------------------------------------------
#
# ### Offline Bandits and Simpson's Paradox ###
#
# ------------------------------------------------------------------------------------------------------------

# In this scenario, imagine a website with Sport and Movie related articles.
#
# The actual preference of men and women for Sport and Movie articles is the following:
#
#  Contexts   | Sport (arm) |  Movie (arm)
# -----------------------------------------
#  Male       | 0.4         |  0.3
#  Female     | 0.8         |  0.7
#
# In other words, both male and female visitors actually prefer sports articles over movie articles.
#
# When visitors are randomly assigned to types of articles, the overall CTR rate per category reflects this:
#
#  Contexts   | Sport (arm) |  Movie (arm)
# -----------------------------------------
#  Male*      | 0.4 x 0.5   |  0.3 x 0.5
#  Female*    | 0.8 x 0.5   |  0.7 x 0.5
# -----------------------------------------
#  CTR total  | 0.6         |  0.5
#
# ------------------------------------------------------------------------------------------------------------
#
# Now suggest the site's editor just "knows" that men like sports, and women like movie related articles.
# So the editor has some business logic implemented, assigning movie related articles, on average,
# to 75% of female visitors, and sports articles, on average, to 75% of male visitors:
#
#  Contexts   | Sport (arm) |  Movie (arm)
# -----------------------------------------
#  Male*      | 0.4 x 0.75  |  0.3 x 0.25
#  Female*    | 0.8 x 0.25  |  0.7 x 0.75
# -----------------------------------------
#  CTR total  | 0.5         |  0.6
#
# This results in a higher CTR for movies than for sports related articles - even though these CTR's do
# not actually reflect the overall preferences of website visitors, but rather the editor's prejudice.
#
# A perfect example of Simpson's Paradox!
#
# Below an illustration of how Simpson's Paradox can give rise to a biased log,
# resulting in biased offline evaluations of bandit policies. Next, we demonstrate
# how inverse propensity scores can sometimes help make such logs usable for offline evaluation after all.

# ------------------------------------------------------------------------------------------------------------
#
# ### Emulation with contextual ###
#
# ------------------------------------------------------------------------------------------------------------

horizon                           <- 10000L
simulations                       <- 1L

# Bandit representing Male and Female actual preferences for sports and movies.
#
#                     S----M------------> Arm 1:   Sport
#                     |    |              Arm 2:   Movie
#                     |    |
weights <- matrix( c(0.4, 0.3,    #-----> Context: Male
                     0.8, 0.7),   #-----> Context: Female

                    nrow = 2, ncol = 2, byrow = TRUE)

# ------------------------------------------------------------------------------------------------------------
# ----------------------------------   Unbiased policy       -------------------------------------------------
# ------------------------------------------------------------------------------------------------------------

# Run a basic random policy, assigning both males and females randomly to either Sport or Movie articles.

policy                            <- RandomPolicy$new()
bandit                            <- ContextualBernoulliBandit$new(weights = weights)
agent                             <- Agent$new(policy, bandit, "Random")

simulation                        <- Simulator$new(agent, horizon, simulations, save_context = TRUE, do_parallel = FALSE)
history                           <- simulation$run()

u_dt                              <- history$get_data_table()

print("1a. Unbiased data generation.")

print(paste("Sport:",sum(u_dt[choice==1]$reward)/nrow(u_dt[choice==1]))) # 0.6 CTR Sport - correct.
print(paste("Movie:",sum(u_dt[choice==2]$reward)/nrow(u_dt[choice==2]))) # 0.5 CTR Movie - correct.

# ----------------------------------   Use unbiased as offline data    ---------------------------------------

# This produces a data.table with *unbiased* historical data that reproduces the original CTR on replay.

bandit                            <- OfflineReplayEvaluatorBandit$new(u_dt,2,2)
policy                            <- EpsilonGreedyPolicy$new(0.1)
agent                             <- Agent$new(policy, bandit, "OfflineLinUCB")

simulation                        <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)
history                           <- simulation$run()

ru_dt                             <- history$get_data_table()

print("1b. Offline unbiased policy evaluation.")

print(paste("Sport:",sum(ru_dt[choice==1]$reward)/nrow(ru_dt[choice==1]))) # 0.6 CTR Sport - correct.
print(paste("Movie:",sum(ru_dt[choice==2]$reward)/nrow(ru_dt[choice==2]))) # 0.5 CTR Movie - correct.

# ------------------------------------------------------------------------------------------------------------
# ----------------------------------   Biased policy       ---------------------------------------------------
# ------------------------------------------------------------------------------------------------------------

# Now suggest some editor just "knows' that men like Sport, and women like Movie. So some business logic
# was added to the site assigning Movie related articles, on average, to 75% of Female visitors,
# and Sport articles, on average, to 75% of Male visitors.
#
# This business logic might be implemented through the following policy:

BiasedPolicy                      <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = RandomPolicy,
  public = list(
    class_name = "BiasedPolicy",
    get_action = function(t, context) {
      if(context$X[1]==1) {           # 1: Male || 0: Female.
        prob                      <- c(0.75,0.25)    # Editor thinks men like Sport articles more.
      } else {
        prob                      <- c(0.25,0.75)    # Editor thinks women like Movie articles more.
      }
      action$choice               <- sample.int(context$k, 1, replace = TRUE, prob = prob)

      # Store the propensity score for the current action too:
      action$propensity           <- prob[action$choice]
      action
    }
  )
)

# ------------------------------------------------------------------------------------------------------------

# Create offline data once again - but this time round, it will be biased.

policy                            <- BiasedPolicy$new()
bandit                            <- ContextualBernoulliBandit$new(weights = weights)
agent                             <- Agent$new(policy, bandit, "Random")

simulation                        <- Simulator$new(agent, horizon, simulations, save_context = TRUE, do_parallel = FALSE)
history                           <- simulation$run()

b_dt                              <- history$get_data_table()

print("2a. Biased data generation.")

print(paste("Sport:",sum(b_dt[choice==1]$reward)/nrow(b_dt[choice==1]))) # 0.5 CTR Sport - Simpson's..
print(paste("Movie:",sum(b_dt[choice==2]$reward)/nrow(b_dt[choice==2]))) # 0.6 CTR Movie - Simpson's..

# ----------------------------------   Use biased as offline data    -----------------------------------------

# So we now have a data.table with *biased* historical data.
# Lets see what happens if we run the same simulation again:

bandit                            <- OfflineReplayEvaluatorBandit$new(b_dt,2,2)
policy                            <- EpsilonGreedyPolicy$new(0.1)
agent                             <- Agent$new(policy, bandit, "rb")

simulation                        <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)
history                           <- simulation$run()
rb_dt                             <- history$get_data_table()

# which is also the case when we use this data to do offline simulation to test other policy:

print("2b. Offline biased policy evaluation.")

print(paste("Sport:",sum(rb_dt[choice==1]$reward)/nrow(rb_dt[choice==1]))) # 0.5 CTR Sport - Simpson's..
print(paste("Movie:",sum(rb_dt[choice==2]$reward)/nrow(rb_dt[choice==2]))) # 0.6 CTR Movie - Simpson's..

# ------------------------------------------------------------------------------------------------------------
# ----------------------------------   Biased policy repaired with prop      ---------------------------------
# ------------------------------------------------------------------------------------------------------------


bandit                            <- OfflinePropensityWeightingBandit$new(b_dt,2,2, stabilize = TRUE)
policy                            <- EpsilonGreedyPolicy$new(0.1)
agent                             <- Agent$new(policy, bandit, "prop")

simulation                        <- Simulator$new(agent, horizon, simulations, do_parallel = FALSE)
history                           <- simulation$run()
prop_dt                           <- history$get_data_table()

# Happily, (stabilized) inverse propensity scoring can help remove the bias again:

print("2c. Offline biased policy evaluation, inverse propensity scores.")

print(paste("Sport:",sum(prop_dt[choice==1]$reward)/nrow(prop_dt[choice==1]))) # 0.6 CTR Sport again, yay!
print(paste("Movie:",sum(prop_dt[choice==2]$reward)/nrow(prop_dt[choice==2]))) # 0.5 CTR Movie again, yay!


