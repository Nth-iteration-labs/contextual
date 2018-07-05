# ----------------------------------------------------------------------
# Description
# ----------------------------------------------------------------------

# Script for running the Bernoulli bandit simulations from Section 3 in
# Chapelle, Li 2011 article

library(here)

# ----------------------------------------------------------------------
# Loading the librarires and functions
# ----------------------------------------------------------------------

# set the directory if using the script interactively,
# below you see example of my path to the folder
setwd(here("demo","demo_chapelle_li_2011"))

# house keeping
rm(list = ls())

# load libraries and functions
source("utils.R")
source("banditAlgorithms.R")


# ----------------------------------------------------------------------
# Setting the parallelization
# ----------------------------------------------------------------------

# to speed up simulations, we run it in parallel  if multiple cores available
seed <- 1234
noCores <-  ifelse(detectCores() == 1, 1, detectCores() - 1)
registerDoParallel(noCores)
registerDoRNG(seed)


# ----------------------------------------------------------------------
# Function for simulations
# ----------------------------------------------------------------------

simulate <- function(
    condInfo,
    noSim,
    subsample = 1,
    outDir = "",
    filename = "results"
    ) {

    # data frame for all results
    resultsAvg <- data.frame()
    resultsFin <- data.frame()

    # iterating over conditions
    for (cond in 1:nrow(condInfo)) {
        # cond = 2
        cat("-------------------------------------------------\n
             Condition: ", cond, " out of ", nrow(condInfo), "\n
             -------------------------------------------------\n")

        # prepare the input for the algorithm
        algo <- algoInfo[[condInfo$algo[cond]]][[1]]
        algoPars <- algoInfo[[condInfo$algo[cond]]][["pars"]]
        noTrials <- condInfo$noTrials[cond]
        env <- list(
            reward = genBernoulliBandit,
            noTrials = noTrials,
            dynamic = condInfo$dynamic[cond],
            noArms = condInfo$noArms[cond],
            prob = condInfo$prob[cond],
            epsilon = condInfo$epsilon[cond],
            aEnv = condInfo$aEnv[cond],
            bEnv = condInfo$bEnv[cond],
            pChange = condInfo$pChange[cond]
        )

        # we have a special treatment of ALB as it can be computed exactly
        if (condInfo[cond, "algo"] == "Asymptotic Lower Bound") {

            # computing the regret bound
            algoRes <- algo(env, algoPars)

            # extracting the results
            algoResults <- algoRes %>%
                filter(trial %in%
                    c(1, seq(subsample, noTrials, subsample))) %>%
                mutate(iter = 1)

        # else we simulate, in parallel if possible
        } else {

            # iterating over simulation iterations in parallel
            algoResults <- foreach(sim = 1:noSim, .combine = rbind) %dorng% {

                # single simulation of an algorithm
                startTime <-  Sys.time()
                algoRes <- algo(env, algoPars)
                endTime <-  Sys.time()
                runTime <- endTime - startTime
                cat("Simulation: ", sim, " | Execution time: ",
                    as.numeric(runTime, "mins"), "min\n")

                # computing regret
                algoCumRegret <- data.frame(iter = sim, algoRes) %>%
                    filter(trial %in%
                        c(1, seq(subsample, noTrials, subsample)))

                # provide some estimate of remaining runs duration
                cat("Estimated remaining time: ",
                    (as.numeric(runTime, "mins")*(noSim-sim)/noCores) *
                    (nrow(condInfo) - cond + 1),
                    "min\n")

                return(algoCumRegret)
            }
        }

        # computing averages across simulation runs
        condResults <- algoResults %>%
            group_by(trial) %>%
            summarise(reg_mean = mean(cumreg), reg_sd = sd(cumreg)) %>%
            mutate(
                algo = condInfo$algo[cond],
                noArms = condInfo$noArms[cond],
                epsilon = condInfo$epsilon[cond]
            )
        condResultsFin <- algoResults %>%
            filter(trial == noTrials) %>%
            mutate(
                algo = condInfo$algo[cond],
                noArms = condInfo$noArms[cond],
                epsilon = condInfo$epsilon[cond]
            )
        resultsAvg <- rbind(resultsAvg, condResults)
        resultsFin <- rbind(resultsFin, condResultsFin)

        # saving interim results
        # save(condResults, condResultsFin,
        #     file = paste0(outDir, filename, "_", cond, ".RData"))
    }

    # if everything run succesfully, saving the data
    save(resultsAvg, resultsFin, file = paste0(outDir, filename, ".RData"))
    cat("---------------- DONE --------------------\n")

}  # END of the function



# ----------------------------------------------------------------------
# Simulations for figure 1 - basic Thompson vs UCB
# ----------------------------------------------------------------------

# parameters for simulation
noSim <- 100
subsample <- 100  # we dont save all the data at the end

# defining the bandit problem characteristics
noTrials <- 10^7
noArms <- c(10, 100)
epsilon <- c(0.1, 0.02)
prob <- 0.5
dynamic <- FALSE
aEnv <- NA
bEnv <- NA
pChange <- NA

# algorithm info
algoInfo <- list(
    "Thompson" = list(Thompson),
    "UCB" = list(UCB),
    "Asymptotic Lower Bound" = list(ALB)
)

# setting condition info for simulation iterations
condInfo <- expand.grid(
    noTrials = noTrials, noArms = noArms, epsilon = epsilon, prob = prob,
    dynamic = dynamic, aEnv = aEnv, bEnv = aEnv, pChange = pChange,
    algo = names(algoInfo), stringsAsFactors = FALSE
)

# executing the simulations
simulate(
    condInfo, noSim, subsample,
    outDir = "../data/", filename = "banditBasic"
)


# ----------------------------------------------------------------------
# Simulations for Thompson sampling with prior mismatch
# ----------------------------------------------------------------------

# parameters for simulation
noSim <- 100
subsample <- 100  # we dont save all the data at the end

# defining the bandit problem characteristics
noTrials <- 10^7
noArms <- c(10, 100)
epsilon <- 0.02
prob <- 0.1
dynamic <- FALSE
aEnv <- NA
bEnv <- NA
pChange <- NA


# algorithm info
algoInfo <- list(
    "Thompson" = list(Thompson),
    "UCB" = list(UCB),
    "Asymptotic Lower Bound" = list(ALB)
)

# setting condition info for simulation iterations
condInfo <- expand.grid(
    noTrials = noTrials, noArms = noArms, epsilon = epsilon, prob = prob,
    dynamic = dynamic, aEnv = aEnv, bEnv = aEnv, pChange = pChange,
    algo = names(algoInfo), stringsAsFactors = FALSE
)


# executing the simulations
simulate(
    condInfo, noSim, subsample,
    outDir = "../data/", filename = "banditPriorMismatch"
)


# ----------------------------------------------------------------------
# Simulations for figure 2 - optimistic Thompson sampling
# ----------------------------------------------------------------------

# parameters for simulation
noSim <- 100
subsample <- 100

# defining the bandit problem characteristics
noTrials <- 10^7
noArms <- c(10, 100)
epsilon <- c(0.1, 0.02)
prob <- 0.5
dynamic <- FALSE
aEnv <- NA
bEnv <- NA
pChange <- NA

# algorithm info
algoInfo <- list(
    "OBS" = list(Thompson, pars = list(optimistic = TRUE))
)

# setting condition info for simulation iterations
condInfo <- expand.grid(
    noTrials = noTrials, noArms = noArms, epsilon = epsilon, prob = prob,
    dynamic = dynamic, aEnv = aEnv, bEnv = aEnv, pChange = pChange,
    algo = names(algoInfo), stringsAsFactors = FALSE
)

# executing the simulations
simulate(
    condInfo, noSim, subsample,
    outDir = "../data/", filename = "banditOptimistic"
)


# ----------------------------------------------------------------------
# Simulations for figure 3 - posterior reshaping for Thompson sampling
# ----------------------------------------------------------------------

# parameters for simulation
noSim <- 1000
subsample <- 100  # we dont save all the data at the end

# defining the bandit problem characteristics
noTrials <- 10^7
noArms <- c(10)
epsilon <- c(0.02)
prob <- 0.5
dynamic <- FALSE
aEnv <- NA
bEnv <- NA
pChange <- NA

# algorithm info
algoInfo <- list(
    "Thompson_2" = list(Thompson, pars = list(alpha = 2)),
    "Thompson_1" = list(Thompson, pars = list(alpha = 1)),
    "Thompson_05" = list(Thompson, pars = list(alpha = 0.5)),
    "Thompson_025" = list(Thompson, pars = list(alpha = 0.25)),
    "Asymptotic Lower Bound" = list(ALB, pars = list(constant = 0))
)

# setting condition info for simulation iterations
condInfo <- expand.grid(
    noTrials = noTrials, noArms = noArms, epsilon = epsilon, prob = prob,
    dynamic = dynamic, aEnv = aEnv, bEnv = aEnv, pChange = pChange,
    algo = names(algoInfo), stringsAsFactors = FALSE
)

# executing the simulations
simulate(
    condInfo, noSim, subsample,
    outDir = "../data/", filename = "banditPosteriorReshaping"
)


# ----------------------------------------------------------------------
# Simulations for table 1 - impact of delay, Thompson vs UCB
# ----------------------------------------------------------------------

# parameters for simulation
noSim <- 100
subsample <- 100  # we dont save all the data at the end

# defining the bandit problem characteristics
noTrials <- 10^6
noArms <- c(10)
epsilon <- NA
prob <- NA
dynamic <- TRUE
aEnv <- 4
bEnv <- 4
pChange <- 10^(-3)

# algorithm info
algoInfo <- list(
    "Thompson_1" = list(Thompson, pars = list(batch = 1)),
    "Thompson_3" = list(Thompson, pars = list(batch = 3)),
    "Thompson_10" = list(Thompson, pars = list(batch = 10)),
    "Thompson_32" = list(Thompson, pars = list(batch = 32)),
    "Thompson_100" = list(Thompson, pars = list(batch = 100)),
    "Thompson_316" = list(Thompson, pars = list(batch = 316)),
    "Thompson_1000" = list(Thompson, pars = list(batch = 1000)),
    "UCB_1" = list(UCB, pars = list(batch = 1)),
    "UCB_3" = list(UCB, pars = list(batch = 3)),
    "UCB_10" = list(UCB, pars = list(batch = 10)),
    "UCB_32" = list(UCB, pars = list(batch = 32)),
    "UCB_100" = list(UCB, pars = list(batch = 100)),
    "UCB_316" = list(UCB, pars = list(batch = 316)),
    "UCB_1000" = list(UCB, pars = list(batch = 1000))
)

# setting condition info for simulation iterations
condInfo <- expand.grid(
    noTrials = noTrials, noArms = noArms, epsilon = epsilon, prob = prob,
    dynamic = dynamic, aEnv = aEnv, bEnv = aEnv, pChange = pChange,
    algo = names(algoInfo), stringsAsFactors = FALSE
)

# executing the simulations
simulate(
    condInfo, noSim, subsample,
    outDir = "../data/", filename = "banditDelay"
)
