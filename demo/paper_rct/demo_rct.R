library(here)
library(data.table)
setwd(here("demo", "paper_rct"))
source("../dev.R")
#library(contextual)
library(here)
library(data.table)
setwd(here("demo", "paper_rct"))

# TODO: repeat randomized

context_cols <- c(3,4)

################ Phase I - Importing and parsing RCT data #############

# Import rct data

url        <- "https://raw.githubusercontent.com/robinvanemden/contextual_rct/master/data/"
file       <- "data.csv"
rct_dt     <- fread(paste0(url,file))

# Extract site Bhubaneswar (B) and Rourkela (R)

rct_dt[, c("site") := tstrsplit(Study_ID, "", fixed=TRUE, keep=c(1))]

# Import per site treatment vs placebo data

url        <- "https://raw.githubusercontent.com/robinvanemden/contextual_rct/master/data/"
file       <- "treatment_placebo.csv"
tp_dt      <- fread(paste0(url,file))

# Enrich rct_dt with treatment_placebo column, where treatment (t) and placebo (p)

rct_dt[tp_dt, on=.(site==site, RCT_Code==rct_code), treatment_placebo:= treatment_placebo]

# Delete not completed

rct_dt <- rct_dt[Completed_Study=="Completed study"]

# Delete unknown treatment / placebo (Site R> 152)

rct_dt <- rct_dt[!is.na(treatment_placebo)]

################ Phase II - Importing and parsing RCT data #############

# move context columns to context vectors

rct_dt[,Sex_of_Infant:=ifelse(Sex_of_Infant=="Male",1,2)]

rct_dt[, context := as.list(as.data.frame(t(rct_dt[, ..context_cols])))]

# create stream object

log_S <- rct_dt[, .(treatment_placebo, SEPSISDEATH, context)]
setnames(log_S, c("treatment_placebo", "SEPSISDEATH"), c("choice", "reward"))

# Recode reward/SEPSISDEATH: sepsis or death is 0, no death is 1

#log_S[,reward:=ifelse(reward==1,0,1)]
log_S[.(reward = c(0,1), to = c(1,0)), on = "reward", reward := i.to]

# Recode choice/treatment_placebo: P is 1, T is 2

#log_S[,choice:=ifelse(choice=="P",1,2)]
log_S[.(choice = c("T","P"), to = c(1,2)), on = "choice", choice := i.to]

# Add t, sim, and agent

log_S[, t := .I]
log_S[, sim := 1]
log_S[, agent := "rct"]

################ Phase II - Running Li Bandit ############################

simulations <- 1000
horizon     <- nrow(log_S)

# initiate Li bandit, with 10 arms, and 100 dimensions

bandit      <- LiSamplingOfflineBandit$new(data_stream = log_S, k = 2, d = length(context_cols))

# define two LinUCBDisjointSmPolicy agents

agents <-
  list(
    Agent$new(RandomPolicy$new(), bandit, name = "Random"),
    Agent$new(ThompsonSamplingPolicy$new(), bandit, name = "TS"),
    Agent$new(ContextualDisjointThompsonSamplingPolicy$new(), bandit, name = "C_TS"),
    Agent$new(LinUCBDisjointOptimizedPolicy$new(alpha=0.1), bandit, name = "LinUCB"),
    Agent$new(UCB1Policy$new(), bandit, name = "UCB1")
  )

# define the simulation

simulation <-
  Simulator$new(
    agents = agents,
    simulations = simulations,
    horizon = horizon,
    do_parallel = TRUE,
    reindex = TRUE
  )

# run the simulation
rct_sim  <- simulation$run()

################ Phase III - Take a look at the results ##################

# total duration sim

print(rct_sim$meta$sim_total_duration)

# plot the results

plot(rct_sim, regret = FALSE, type = "cumulative", rate = TRUE, traces = FALSE,
     ylim = c(0.922,0.942), legend_position = "bottomright" )


total_babies_tested   <- rct_sim$get_cumulative_result()$Random$t

total_died_rct        <- total_babies_tested - rct_sim$get_cumulative_result()$Random$cum_reward

total_died_ts        <- total_babies_tested - rct_sim$get_cumulative_result()$TS$cum_reward

ts_saves_babies      <- total_died_rct - total_died_ts

print(paste0("Out of ",total_babies_tested," babies tested, ",
             total_died_rct," got sepsis or died in the RCT experiment, in contrast to ",total_died_ts,
             " in the TS experiment. Thompson Sampling saved ",ts_saves_babies," babies!" ))


