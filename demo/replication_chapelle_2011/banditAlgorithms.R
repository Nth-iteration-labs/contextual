# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# This script defines the algorithms from the paper for the 
# Bernoulli bandit problem


# ----------------------------------------------------------------------
# Auxiliary functions
# ----------------------------------------------------------------------

# finding a max value in a vector with real numbers, randomly breaking
# the ties
idxMax <- function (x, tol = 1e-16) {
    stopifnot(is.numeric(x), is.numeric(tol), all(!is.na(x)))
    if (all(is.infinite(x)) || abs(max(x) - min(x)) < tol) {
        idx <- sample(1:length(x), 1)
    }
    else {
        idx <- which(x == max(x))
        if (length(idx) > 1) {
            idx <- sample(idx, 1)
        }
    }
    return(idx)
}

# a function for generating the Bernoulli bandit observations 
genBernoulliBandit <- function(noArms, prob) {
    # basic checks
    stopifnot(noArms == length(prob))
    # make draws 
    draws <- function() rbinom(noArms, 1, prob)
    return(draws)
}


# ----------------------------------------------------------------------
# Algorithms
# ----------------------------------------------------------------------


Thompson <- function(env, pars = list(alpha = 1, batch = 1)) {

    # basic vars
    aPrior <- ifelse(is.null(pars[["a"]]), 1, pars[["a"]]) 
    bPrior <- ifelse(is.null(pars[["b"]]), 1, pars[["b"]])
    alphaReshape <- ifelse(is.null(pars[["alpha"]]), 1, pars[["alpha"]])
    batch <- ifelse(is.null(pars[["batch"]]), 100, pars[["batch"]])
    optimistic <- ifelse(is.null(pars[["optimistic"]]), FALSE, 
        pars[["optimistic"]])
    dynamic <- env[["dynamic"]]
    noTrials <- env[["noTrials"]]
    noArms <- env[["noArms"]]
    rewardFnc <- env[["reward"]]
    if (dynamic) {
        aEnv <- env[["aEnv"]]
        bEnv <- env[["bEnv"]]
        probs <- rbeta(noArms, aEnv, bEnv)
        probMax <- probs[idxMax(probs)]
        pChange <- env[["pChange"]]
    } else {
        probMax <- env[["prob"]] 
        probs <- c(probMax, rep(probMax - env[["epsilon"]], noArms - 1))
    }
    
    # objects for results
    choices <- rep(NA, noTrials)
    success <- rep(0, noArms)
    failure <- rep(0, noArms)
    successBatch <- rep(0, noArms)
    failureBatch <- rep(0, noArms)
    regret <- rep(NA, noTrials)
    
    for (trial in 1:noTrials) {
        # trial=100

        # draw prob of reward based on the model of the environment 
        # i.e. from the posterior distribution P(theta|data)
        thetat <- rbeta(
            noArms, 
            (success + aPrior)/alphaReshape, 
            (failure + bPrior)/alphaReshape
        )

        # optimistic bayesian sampling version 
        if (optimistic) {
            expVals <- (success + aPrior)/(success + aPrior + failure + bPrior)
            thetat <- apply(rbind(thetat, expVals), 2, max)
        }

        # choice according to the draws from the posterior
        choice <- which.max(thetat)
        choices[trial] <- choice

        # environment returns a reward for a given choice
        if (dynamic && (runif(1) < pChange)) {
            idx <- sample(1:noArms, 1)
            probs[idx] <- rbeta(1, aEnv, bEnv)
            success[idx] <- 0; failure[idx] <- 0
            probMax <- probs[idxMax(probs)]
        } 
        reward <- rewardFnc(noArms, probs)()[choice]
        regret[trial] <- probMax - probs[choice]

        # update the batch
        if(reward == 0) {
            failureBatch[choice] <- failureBatch[choice] + 1
        } else {
            successBatch[choice] <- successBatch[choice] + 1
        }

        # update the model of the environment, after batch is full 
        if (trial %% batch == 0) {
            failure <- failure + failureBatch
            success <- success + successBatch
            failureBatch <- rep(0, noArms)
            successBatch <- rep(0, noArms)
        }
    }

    # computing and returning cumulative regret
    regret <- data.frame(
        trial = 1:noTrials,
        cumreg = cumsum(regret)
    ) 
    return(regret)
}


UCB <- function(env, pars = list(batch = 1)) {

    # basic vars
    batch <- ifelse(is.null(pars[["batch"]]), 100, pars[["batch"]])
    dynamic <- env[["dynamic"]]
    noTrials <- env[["noTrials"]]
    noArms <- env[["noArms"]]
    rewardFnc <- env[["reward"]]
    if (dynamic) {
        aEnv <- env[["aEnv"]]
        bEnv <- env[["bEnv"]]
        probs <- rbeta(noArms, aEnv, bEnv)
        probMax <- probs[idxMax(probs)]
        pChange <- env[["pChange"]]
    } else {
        probMax <- env[["prob"]] 
        probs <- c(probMax, rep(probMax - env[["epsilon"]], noArms - 1))
    }

    # objects for results
    choices <- rep(NA, noTrials)
    k <- rep(0, noArms)  
    m <- rep(0, noArms)
    kBatch <- rep(0, noArms)  
    mBatch <- rep(0, noArms)
    regret <- rep(NA, noTrials)

    # initialize the algo
    choices[1:noArms] <- 1:noArms
    reward <- rewardFnc(noArms, probs)()
    regret[1:noArms] <- probMax - probs
    k <- k + reward
    m <- m + 1

    for (trial in (noArms + 1):noTrials) {
        # trial = 11
        
        idxNontried <- m == 0
        if (any(idxNontried)) {
            ucbEst <- idxNontried*1
        } else {
            # update delta 
            delta <- sqrt(1 / trial)

            # computing the upper confidence bound
            ucbEst <- k / m + 
                      sqrt((2*(k/m)*log(1/delta)) / m) + 
                      (2*log(1/delta)) / m
        }

        # choice according to the best bound
        choice <- idxMax(ucbEst)
        choices[trial] <- choice

        # environment returns a reward for a given choice
        if (dynamic && (runif(1) < pChange)) {
            idx <- sample(1:noArms, 1)
            k[idx] <- 0; m[idx] <- 0
            probs[idx] <- rbeta(1, aEnv, bEnv)
            probMax <- probs[idxMax(probs)]
        } 
        reward <- rewardFnc(noArms, probs)()[choice]
        regret[trial] <- probMax - probs[choice]

        # update the batch
        kBatch[choice] <- kBatch[choice] + reward
        mBatch[choice] <- mBatch[choice] + 1

        # update the model of the environment, after batch is full 
        if (trial %% batch == 0) {
            k <- k + kBatch
            m <- m + mBatch
            kBatch <- rep(0, noArms)
            mBatch <- rep(0, noArms)
        }
    }

    # computing and returning cumulative regret
    regret <- data.frame(
        trial = 1:noTrials,
        cumreg = cumsum(regret)
    ) 
    return(regret)
}


ALB <- function(env, pars = list(constant = 0)) {

    # basic vars
    noTrials <- env[["noTrials"]]
    noArms <- env[["noArms"]]
    probMax <- env[["prob"]]
    probs <- rep(probMax - env[["epsilon"]], noArms - 1)
    constant <- ifelse(is.null(pars[["constant"]]), 0, pars[["constant"]])

    # help function for KL divergence
    klDivergence <- function(p, q) {
        p *log(p/q) + (1-p)*log((1-p)/(1-q))
    }

    # computing the bound
    bound <- 
        log(1:noTrials) * 
        (sum((probMax - probs) / 
        sapply( probs, function(x) klDivergence(x, probMax)), na.rm = TRUE) + 
        constant)

    # computing and returning cumulative regret
    regret <- data.frame(
        trial = 1:noTrials,
        cumreg = bound
    ) 
    return(regret)
}
