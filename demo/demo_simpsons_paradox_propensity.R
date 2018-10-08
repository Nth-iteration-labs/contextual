library(contextual)

# ------------------------------------------------------------------------------------------------------------
#
# ### Offline bandits and Simpson's Paradox ###
#
# ------------------------------------------------------------------------------------------------------------
#
# Where randomly assigned to contexts
# CTR reflects preferences:
#
#  Contexts   | Sport (arm) |  Movie (arm)
# -----------------------------------------
#  Male*      | 0.4 x 0.5   |  0.3 x 0.5
#  Female*    | 0.8 x 0.5   |  0.7 x 0.5
# -----------------------------------------
#  CTR total  | 0.6         |  0.5
#
# Most popular individual and overall: Sport.
#
# *click_probability x fraction_assigned_to_arm
#
# ------------------------------------------------------------------------------------------------------------
#
# Yet when contexts not randomly assigned, but for instance
# biased as below:
#
#  Contexts   | Sport (arm) |  Movie (arm)
# -----------------------------------------
#  Male*      | 0.4 x 0.75  |  0.3 x 0.25
#  Female*    | 0.8 x 0.25  |  0.7 x 0.75
# -----------------------------------------
#  CTR total  | 0.5         |  0.6
#
# Now, the overall most popular arm becomes Movie,
# even though male and female groups still prefer Sport!
#
# This is an example of Simpson's Paradox.

# ------------------------------------------------------------------------------------------------------------

# Lets emulate the above with Contextual.

# ------------------------------------------------------------------------------------------------------------

# First, define a bandit, and pose that it realistically simulates visitor's behavior:

horizon     <- 10000L
simulations <- 1L

# Bandit assigns to male and female randomly and equally to each context,
# representing the actual preferences of males and females.
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

# Now we run a basic random policy, assigning both males and females randomly to either
# Sport and Movie articles.

policy             <- RandomPolicy$new()
bandit             <- ContextualBasicBandit$new(weights = weights)
agent              <- Agent$new(policy, bandit, "Random")

simulation         <- Simulator$new(agent, horizon, simulations, save_context = TRUE, do_parallel = FALSE)
history            <- simulation$run()

u_dt               <- history$get_data_table()

print((nrow(u_dt)))

# choice:1 is Sport choice:2 is Movie, and is_male:1 is male is_male:2 is female
u_dt[, is_male := lapply(.SD, function(x) paste( unlist(x)[1], collapse=',') ),
          by=1:u_dt[, .N], .SDcols = c("context")]

print("-offline unbiased data generation-")

print(paste("Sport:",sum(u_dt[choice==1]$reward)/nrow(u_dt[choice==1]))) # 0.6 CTR Sport - correct.
print(paste("Movie:",sum(u_dt[choice==2]$reward)/nrow(u_dt[choice==2]))) # 0.5 CTR Movie - correct.

# ----------------------------------   Use unbiased as offline data    ---------------------------------------

### now we have a data.table with *unbiased* historical data.
### So we can run a different policy on this data and still
### obtain the same outcome we would have obtained for the bandit directly (simulate this as well?)

bandit             <- OfflineReplayEvaluatorBandit$new(u_dt,2,2)
policy             <- LinUCBDisjointOptimizedPolicy$new(1.0)
agent              <- Agent$new(policy, bandit, "OfflineLinUCB")

simulation         <- Simulator$new(agent, horizon, simulations, reindex = TRUE, do_parallel = FALSE)
history            <- simulation$run()

ru_dt              <- history$get_data_table()

print("-offline unbiased policy evaluation-")

print((nrow(ru_dt)))

print(paste("Sport:",sum(ru_dt[choice==1]$reward)/nrow(ru_dt[choice==1]))) # 0.6 CTR Sport - correct.
print(paste("Movie:",sum(ru_dt[choice==2]$reward)/nrow(ru_dt[choice==2]))) # 0.5 CTR Movie - correct.

# ------------------------------------------------------------------------------------------------------------
# ----------------------------------   Biased policy       ---------------------------------------------------
# ------------------------------------------------------------------------------------------------------------

# Lets now suggest some editor just "knows' that men like Sport, and
# women like Movie. So some business logic was added to the site
# assigning Movie related articles, on average, to 75% of Female visitors,
# and Sport articles, on average, to 75% of Male visitors.
#
# This business logic might be implemented through the following policy:

BiasedPolicy <- R6::R6Class(
  portable = FALSE,
  class = FALSE,
  inherit = RandomPolicy,
  public = list(
    class_name = "BiasedPolicy",
    get_action = function(t, context) {
      if(context$X[1,1]==1) {           # 1: Male || 0: Female.
        prob <- c(0.75,0.25)            # Editor thinks men like Sport articles more.
      } else {
        prob <- c(0.25,0.75)            # Editor thinks women like Movie articles more.
      }
      action$choice               <- sample.int(context$k, 1, replace = TRUE, prob = prob)
      action$propensity           <- prob[action$choice]
      action
    }
  )
)

# ------------------------------------------------------------------------------------------------------------

# We create offline data once again - but this time round, it will be biased.

policy             <- BiasedPolicy$new()
bandit             <- ContextualBasicBandit$new(weights = weights)
agent              <- Agent$new(policy, bandit, "Random")

simulation         <- Simulator$new(agent, horizon, simulations, save_context = TRUE, do_parallel = FALSE)
history            <- simulation$run()

b_dt               <- history$get_data_table()

# choice:1 is Sport choice:2 is Movie, and is_male:1 is male is_male:2 is female
#b_dt[, is_male := lapply(.SD, function(x) paste( unlist(x)[1], collapse=',') ),
#            by=1:b_dt[, .N], .SDcols = c("context")]

print("-offline biased data generation-")

print((nrow(b_dt)))

print(paste("Sport:",sum(b_dt[choice==1]$reward)/nrow(b_dt[choice==1]))) # 0.5 CTR Sport - Simpson's..
print(paste("Movie:",sum(b_dt[choice==2]$reward)/nrow(b_dt[choice==2]))) # 0.6 CTR Movie - Simpson's..

# so here, it would make sense to choose arm 1 overall..

# ----------------------------------   Use biased as offline data    -----------------------------------------

# So we now have a data.table with *biased* historical data.
# Lets see what happens if we run the same simulation again:

bandit             <- OfflineReplayEvaluatorBandit$new(b_dt,2,2)
policy             <- UCB1Policy$new()
agent              <- Agent$new(policy, bandit, "rb")

simulation         <- Simulator$new(agent, horizon, simulations, reindex = TRUE, do_parallel = FALSE)
history            <- simulation$run()
rb_dt              <- history$get_data_table()

# which is also the case when we use this data to do offline simulation to test other policy:

print("-offline biased policy evaluation-")

print(nrow(rb_dt))

print(paste("Sport:",sum(rb_dt[choice==1]$reward)/nrow(rb_dt[choice==1]))) # 0.5 CTR Sport - Simpson's..
print(paste("Movie:",sum(rb_dt[choice==2]$reward)/nrow(rb_dt[choice==2]))) # 0.6 CTR Movie - Simpson's..

# ------------------------------------------------------------------------------------------------------------
# ----------------------------------   Biased policy repaired with prop      ---------------------------------
# ------------------------------------------------------------------------------------------------------------


bandit                 <- OfflinePropensityWeightingBandit$new(b_dt,2,2)
policy                 <- UCB1Policy$new()
agent                  <- Agent$new(policy, bandit, "prop")

simulation             <- Simulator$new(agent, horizon, simulations, reindex = TRUE, do_parallel = FALSE)
history                <- simulation$run()
prop_dt                <- history$get_data_table()

# which is also the case when we use this data to do offline simulation to test other policy:

print("-offline biased policy evaluation with propensity scores-")

print(nrow(prop_dt))

print(paste("Sport:",sum(prop_dt[choice==1]$reward)/nrow(prop_dt[choice==1]))) # 0.6 CTR Sport again, yay!
print(paste("Movie:",sum(prop_dt[choice==2]$reward)/nrow(prop_dt[choice==2]))) # 0.5 CTR Movie again, yay!

# The number of time steps (2953) that offline bandit could replay:
#print(rb_dt$cumulative$OfflineLinUCB$t)
# LinUCB overwhelmingly chooses Movie: 91% of all choices
#print(nrow(rb_dt$data[choice==1])/nrow(rb_dt$data)*100)
# And is able to attain a 0.59 CTR on that arm
#print(rb_dt$cumulative$OfflineLinUCB$cum_reward_rate)



