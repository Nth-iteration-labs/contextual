#' Increment
#'
#' \code{inc<-} increments \code{x} by value. Equivalent to \code{x <- x + value.}
#'
#' @param x object to be incremented
#' @param value value by which x will be modified
#'
#' @examples
#' x <- 1:5
#' inc(x) <- 5
#' x
#'
#' @export
"inc<-" <- function(x, value) {
  x + value
}
#' Sherman-Morrisson inverse
#'
#' @param inv to be updated inverse matrix
#' @param x column vector to update inv with
#'
#' @export
sherman_morrisson <- function(inv, x) {
  inv - c((inv %*% (outer(x, x) %*% inv))) / c(1.0 + (crossprod(x,inv) %*% x))
}
#' Clip vectors
#'
#' Clips values to a mininum and maximum value. That is, all values below the lower clamp
#' value and the upper clamp value become the lower/upper value specified
#'
#' @param x to be clipped vector
#' @param min numeric. lowest value
#' @param max numeric. highest value
#'
#' @export
clipr <- function(x, min, max) {
  pmax( min, pmin( x, max))
}
#' Decrement
#'
#' \code{dec<-} decrements \code{x} by value. Equivalent to \code{x <- x - value.}
#'
#' @param x object to be decremented
#' @param value value by which x will be modified
#'
#' @examples
#' x <- 6:10
#' dec(x) <- 5
#' x
#'
#' @export
"dec<-" <- function(x, value) {
  x - value
}
#' Get maximum value in list
#'
#' Returns the index of the maximum value in list \code{x}.
#'
#' If there is a tie and \code{equal_is_random} is \code{TRUE},
#' the index of one of the tied maxima is returned at random.
#'
#' If \code{equal_is_random} is \code{FALSE},
#' the maximum with the lowest index number is returned.
#'
#' @param x vector of values
#' @param equal_is_random boolean
#'
#' @examples
#'
#' theta = list(par_one = list(1,2,3), par_two = list(2,3,4))
#' which_max_list(theta$par_one)
#'
#' @export
which_max_list <- function(x, equal_is_random = TRUE) {
  which_max_tied(unlist(x, FALSE, FALSE), equal_is_random)
}
#' Get maximum value randomly breaking ties
#'
#' Returns the index of the maximum value in vector \code{vec}.
#'
#' If there is a tie,
#' the index of one of the tied maxima is returned at random.
#'
#' @param x vector of values
#' @param equal_is_random boolean
#'
#' @export
which_max_tied <- function(x, equal_is_random = TRUE) {
  x <- seq_along(x)[x == max(x)]
  if (length(x) > 1L && equal_is_random)  {
      return(sample(x, 1L, replace = TRUE))
  } else {
    return(x[1])
  }
}
#' Sum of list
#'
#' Returns the sum of the values of the elements of a list \code{x}.
#'
#' @param x List
#'
#' @examples
#'
#' theta = list(par_one = list(1,2,3), par_two = list(2,3,4))
#' sum_of(theta$par_one)
#'
#' @export
sum_of <- function(x) {
  sum(unlist(x, FALSE, FALSE))
}
#' Inverse from Choleski (or QR) Decomposition.
#'
#' Invert a symmetric, positive definite square matrix from its Choleski decomposition.
#'
#' @param M matrix
#'
#' @examples
#' inv(cbind(1, 1:3, c(1,3,7)))
#'
#' @export
inv <- function(M) {
  chol2inv(chol(M))
}
#' Check if in RStudio
#'
#' Detects whether R is open in RStudio.
#'
#' @return A \code{logical} value that indicates whether R is open in RStudio.
#'
#' @examples
#' is_rstudio()
#'
#' @export
is_rstudio <- function() {
  .Platform$GUI == "RStudio"    #nocov
}
#' @title Change Default Graphing Device from RStudio
#' @description
#' Checks to see if the user is in RStudio. If so, then it changes the device to a popup window.
#' @param ext A \code{logical} indicating whether to plot in a popup or within the RStudio UI.
#' @param width Width in pixels of the popup window
#' @param height Height in pixels of the popup window
#' @details
#' Depending on the operating system, the default drivers attempted to be used are:
#'
#' OS X: quartz()
#'
#' Linux: x11()
#'
#' Windows: windows()
#'
#' Note, this setting is not permanent. Thus, the behavioral change will last
#' until the end of the session.
#'
#' Also, the active graphing environment will be killed.
#' As a result, any graphs that are open will be deleted.
#'
#' @examples
#' \dontrun{
#'
#' # Turn on external graphs
#' external_graphs()
#'
#' # Turn off external graphs
#' external_graphs(F)
#' }
#'
#' @importFrom grDevices graphics.off
#' @importFrom R.devices devOptions
#'
#' @export
set_external <- function(ext = TRUE,
                        width = 10,
                        height = 6) {
  # nocov start
  if (is_rstudio()) {
    if (isTRUE(ext)) {
      sysname <- tolower(Sys.info()["sysname"])
      device.name <- "x11"
      switch(sysname,
             darwin = {
               device.name <- "quartz"
             },
             windows = {
               device.name <- "windows"
             })
      options("device" = device.name)
      R.devices::devOptions(sysname, width = width, height = height)
    } else{
      options("device" = "RStudioGD")
    }
    graphics.off()

  }
} # nocov end

#' Sample one element from vector or list
#'
#' Takes one sample from a vector or list. Does not throw an error for zero length lists.
#'
#' @param x A vector of one or more elements from which to choose
#'
#' @return One value, drawn from x.
#'
#' @export
sample_one_of <- function(x) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(sample(x,1))
  }
}
#' Format difftime objects
#'
#' @param x difftime object
#'
#' @return string "days, h:mm:ss.ms"
#'
#' @export
formatted_difftime <- function(x) {
  units(x) <- "secs"
  x <- unclass(x)
  y <- abs(x)
  if (y %/% 86400 > 0) {
    sprintf("%s%d days, %d:%02d:%02d%s",
            ifelse(x < 0, "-", ""), # sign
            y %/% 86400,  # days
            y %% 86400 %/% 3600,  # hours
            y %% 3600 %/% 60,  # minutes
            y %% 60 %/% 1,
            strtrim(substring(as.character(as.numeric(y) %% 1), 2), 4))
  } else {
    sprintf("%s%d:%02d:%02d%s",
            ifelse(x < 0, "-", ""), # sign
            y %% 86400 %/% 3600,  # hours
            y %% 3600 %/% 60,  # minutes
            y %% 60 %/% 1,
            strtrim(substring(as.character(as.numeric(y) %% 1), 2), 4))
  }
}
#' Welford's variance
#'
#' Welford described a method for 'robust' one-pass computation of the
#' standard deviation. By 'robust', we mean robust to round-off caused
#' by a large shift in the mean.
#'
#' @param z vector
#'
#' @return variance
#'
#' @export
var_welford <- function(z){
  n = length(z)
  M = list()
  S = list()
  M[[1]] = z[[1]]
  S[[1]] = 0

  for(k in 2:n){
    M[[k]] = M[[k-1]] + ( z[[k]] - M[[k-1]] ) / k
    S[[k]] = S[[k-1]] + ( z[[k]] - M[[k-1]] ) * ( z[[k]] - M[[k]] )
  }
  return(S[[n]] / (n - 1))
}

#' The Inverse Gamma Distribution
#'
#' Density, distribution function, quantile function and random
#' generation for the inverse gamma distribution.
#'
#' The inverse gamma distribution with parameters shape and rate has
#' density \emph{f(x) = rate^shape/Gamma(shape) x^(-1-shape)
#' e^(-rate/x)} it is the inverse of the standard gamma
#' parameterization in R.
#'
#' The functions (d/p/q/r)invgamma simply wrap those of the standard
#' (d/p/q/r)gamma R implementation, so look at, say,
#' \code{\link{dgamma}} for details.
#'
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(n) > 1, the length is
#'   taken to be the number required.
#' @param shape inverse gamma shape parameter
#' @param rate inverse gamma rate parameter
#' @param scale alternative to rate; scale = 1/rate
#' @param log,log.p logical; if TRUE, probabilities p are given as
#'   log(p).
#' @param lower.tail logical; if TRUE (default), probabilities are P(X <= x) otherwise, P(X > x).
#' @name invgamma
#' @importFrom stats dgamma pgamma qgamma rgamma
#' @examples
#'
#' s <- seq(0, 5, .01)
#' plot(s, dinvgamma(s, 7, 10), type = 'l')
#'
#' f <- function(x) dinvgamma(x, 7, 10)
#' q <- 2
#' integrate(f, 0, q)
#' (p <- pinvgamma(q, 7, 10))
#' qinvgamma(p, 7, 10) # = q
#' mean(rinvgamma(1e5, 7, 10) <= q)
NULL

#' @rdname invgamma
#' @export
dinvgamma <- function(x, shape, rate = 1, scale = 1/rate, log = FALSE) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  log_f <- dgamma(1/x, shape, rate, log = TRUE) - 2*log(x)
  if(log) return(log_f)
  exp(log_f)
}

#' @rdname invgamma
#' @export
pinvgamma <- function(q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  pgamma(1/q, shape, rate, lower.tail = !lower.tail, log.p = log.p)
}

#' @rdname invgamma
#' @export
qinvgamma <- function(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, log.p = FALSE) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  qgamma(1-p, shape, rate, lower.tail = lower.tail, log.p = log.p)^(-1)
}

#' @rdname invgamma
#' @export
rinvgamma <- function(n, shape, rate = 1, scale = 1/rate) {
  if(missing(rate) && !missing(scale)) rate <- 1/scale
  1 / rgamma(n, shape, rate)
}

#' Inverse Logit Function
#'
#' Given a numeric object return the inverse logit of the values.
#'
#' @param x A numeric object.
#'
#' @return An object of the same type as x containing the inverse logits of the input values.
#'
#' @export
invlogit <- function(x){
  exp(x)/(1+exp(x))
}

#' A vector of zeroes and ones
#'
#' @param vector_length How long will the vector be?
#' @param index_of_one Where to insert the one?
#'
#' @return Vector of zeroes with one(s) at given index position(s)
#'
#' @export
ones_in_zeroes <- function(vector_length, index_of_one) {
  x <- rep(0, vector_length)
  x[index_of_one] <- 1
  return(x[1:vector_length])
}


#' Return context vector of an arm
#'
#' Given d x k matrix or d dimensional vector X,
#' returns a vector with arm's context.
#'
#' @param X d x k Matrix.
#' @param arm index of arm.
#' @param select_features indices of to be returned features.
#'
#' @return Vector that represents context related to an arm
#'
#' @export
get_arm_context <- function(X, arm, select_features = NULL) {
  # X <- as.numeric(levels(X))[X]
  if(is.null(select_features)) {
    if(is.vector(X)) return(X) else return(X[, arm])
  } else {
    if(is.vector(X)) return(X[select_features])
    else return(X[select_features, arm])
  }
}

#' Get full context matrix over all arms
#'
#' Given matrix or d dimensional vector X,
#' number of arms k and number of features d
#' returns a matrix with d x k context matrix
#'
#' @param X d x k Matrix or d dimensional context vector.
#' @param d number of features.
#' @param k number of arms.
#' @param select_features indices of to be returned feature rows.b
#'
#' @return A d x k context Matrix
#'
#' @export
get_full_context <- function(X, d, k, select_features = NULL) {
  if(is.null(select_features)) {
    if(is.vector(X)) return(matrix(X,d,k)) else return(X)
  } else {
    if(is.vector(X)) return(X[select_features])
    else return(X[select_features,])
  }
}

#' @title
#' One Hot Encoding of data.table columns
#'
#' @description
#' One-Hot-Encode unordered factor columns of a data.table mltools. From ben519's "mltools" package.
#'
#' @details
#' One-hot-encoding converts an unordered categorical vector (i.e. a factor) to multiple binarized vectors
#' where each binary vector of
#' 1s and 0s indicates the presence of a class (i.e. level) of the of the original vector.
#'
#' @param dt A data.table
#' @param cols Which column(s) should be one-hot-encoded? DEFAULT = "auto" encodes all unordered
#' factor columns.
#' @param sparsifyNAs Should NAs be converted to 0s?
#' @param naCols Should columns be generated to indicate the present of NAs? Will only apply to factor
#' columns with at least one NA
#' @param dropCols Should the resulting data.table exclude the original columns which are one-hot-encoded?
#' @param dropUnusedLevels Should columns of all 0s be generated for unused factor levels?
#'
#' @examples
#' library(data.table)
#'
#' dt <- data.table(
#'   ID = 1:4,
#'   color = factor(c("red", NA, "blue", "blue"), levels=c("blue", "green", "red"))
#' )
#'
#' one_hot(dt)
#' one_hot(dt, sparsifyNAs=TRUE)
#' one_hot(dt, naCols=TRUE)
#' one_hot(dt, dropCols=FALSE)
#' one_hot(dt, dropUnusedLevels=TRUE)
#'
#' @export
#' @import data.table

one_hot <- function(dt, cols="auto", sparsifyNAs=FALSE, naCols=FALSE, dropCols=TRUE, dropUnusedLevels=FALSE){
  # One-Hot-Encode unordered factors in a data.table
  # If cols = "auto", each unordered factor column in dt will be encoded.
  # (Or specifically a vector of column names to encode)
  # If dropCols=TRUE, the original factor columns are dropped
  # If dropUnusedLevels = TRUE, unused factor levels are dropped

  #--------------------------------------------------
  # Hack to pass 'no visible binding for global variable' notes from R CMD check

  OHEID <- NULL

  #--------------------------------------------------

  # Automatically get the unordered factor columns
  if(cols[1] == "auto") cols <- colnames(dt)[which(sapply(dt, function(x) is.factor(x) & !is.ordered(x)))]

  # If there are no columns to encode, return dt
  if(length(cols) == 0) return(dt)

  # Build tempDT containing and ID column and 'cols' columns
  tempDT <- dt[, cols, with=FALSE]
  tempDT[, OHEID := .I]
  for(col in cols) set(tempDT, j=col, value=factor(paste(col, tempDT[[col]], sep="_"),
                                                   levels=paste(col, levels(tempDT[[col]]), sep="_")))

  # One-hot-encode
  melted <- melt(tempDT, id = 'OHEID', value.factor = T, na.rm=TRUE)
  if(dropUnusedLevels == TRUE){
    newCols <- dcast(melted, OHEID ~ value, drop = T, fun.aggregate = length)
  } else{
    newCols <- dcast(melted, OHEID ~ value, drop = F, fun.aggregate = length)
  }

  # Fill in potentially missing rows
  newCols <- newCols[tempDT[, list(OHEID)]]
  newCols[is.na(newCols[[2]]), setdiff(paste(colnames(newCols)), "OHEID") := 0L]

  #--------------------------------------------------
  # Deal with NAs

  if(!sparsifyNAs | naCols){

    # Determine which columns have NAs
    na_cols <- character(0)
    for(col in cols) if(any(is.na(tempDT[[col]]))) na_cols <- c(na_cols, col)

    # If sparsifyNAs is TRUE, find location of NAs in dt and insert them in newCols
    if(!sparsifyNAs)
      for(col in na_cols) newCols[is.na(tempDT[[col]]), intersect(levels(tempDT[[col]]),
                                                                  colnames(newCols)) := NA_integer_]

    # If naCols is TRUE, build a vector for each column with an NA value and 1s indicating the location of NAs
    if(naCols)
      for(col in na_cols) newCols[, eval(paste0(col, "_NA")) := is.na(tempDT[[col]]) * 1L]
  }

  #--------------------------------------------------
  # Clean Up

  # Combine binarized columns with the original dataset
  result <- cbind(dt, newCols[, !"OHEID"])

  # Reorder columns
  possible_colnames <- character(0)
  for(col in colnames(dt)){
    possible_colnames <- c(possible_colnames, col)
    if(col %in% cols){
      possible_colnames <- c(possible_colnames, paste0(col, "_NA"))
      possible_colnames <- c(possible_colnames, paste(levels(tempDT[[col]])))
    }
  }
  sorted_colnames <- intersect(possible_colnames, colnames(result))
  setcolorder(result, sorted_colnames)

  # If dropCols = TRUE, remove the original factor columns
  if(dropCols == TRUE) result <- result[, !cols, with=FALSE]

  return(result)
}



#' Simulate from a Multivariate Normal Distribution
#'
#' Produces one or more samples from the specified
#' multivariate normal distribution.
#'
#' @param n the number of samples required.
#' @param mu a vector giving the means of the variables.
#' @param sigma a positive-definite symmetric matrix specifying the covariance matrix of the variables.
#'
#' @return If \code{n = 1} a vector of the same length as \code{mu}, otherwise an \code{n} by
#' \code{length(mu)} matrix with one sample in each row.
#'
#' @export
mvrnorm = function(n, mu, sigma)
{
  ncols <- ncol(sigma)
  mu <- rep(mu, each = n)
  mu + matrix(stats::rnorm(n * ncols), ncol = ncols) %*% chol(sigma)
}

#' Potential Value Remaining
#'
#' Compute "value_remaining" in arms not
#' currently best in binomial bandits
#'
#' @author Thomas Lotze and Markus Loecher
#'
#' @param x Vector of the number of successes per arm.
#' @param n Vector of the number of trials per arm.
#' @param alpha Shape parameter alpha for the prior beta distribution.
#' @param beta Shape parameter beta for the prior beta distribution.
#' @param ndraws Number of random draws from the posterior.
#'
#' @return Value_remaining distribution; the distribution of
#' improvement amounts that another arm might have over the current best arm.
#'
#' @examples
#'
#' x <- c(10,20,30,80)
#' n <- c(100,102,120,240)
#' vr <- value_remaining(x, n)
#' hist(vr)
#'
#' # "potential value" remaining in the experiment
#' potential_value <- quantile(vr, 0.95)
#'
#' @export
value_remaining <- function(x, n, alpha = 1, beta = 1, ndraws = 10000)
{
  post <- sim_post(x,n,alpha,beta,ndraws)
  postWin <- prob_winner(post)
  iMax <- which.max(postWin)
  thetaMax <- apply(post,1,max)
  #value_remaining:
  vR <- (thetaMax-post[,iMax])/post[,iMax]
  return(vR)
}

#' Binomial Posterior Simulator
#'
#' Simulates the posterior distribution of
#' the Bayesian probabilities for each arm being the
#' best binomial bandit.
#'
#' @author Thomas Lotze and Markus Loecher
#'
#' @param x Vector of the number of successes per arm.
#' @param n Vector of the number of trials per arm.
#' @param alpha Shape parameter alpha for the prior beta distribution.
#' @param beta Shape parameter beta for the prior beta distribution.
#' @param ndraws Number of random draws from the posterior.
#'
#' @return Matrix of bayesian probabilities for each arm being the best binomial bandit
#'
#' @examples
#'
#' x <- c(10,20,30,50)
#' n <- c(100,102,120,130)
#' sp <- sim_post(x,n)
#'
#' @export
sim_post <- function(x, n, alpha = 1, beta = 1, ndraws = 5000) {
  k <- length(x)
  ans <- matrix(nrow=ndraws, ncol=k)
  no <- n-x
  for (i in (1:k))
    ans[,i] <- stats::rbeta(ndraws, x[i] + alpha, no[i] + beta)
  return(ans)
}

#' Binomial Win Probability
#'
#' Function to compute probability that each arm is the winner,
#' given simulated posterior results.
#'
#' @author Thomas Lotze and Markus Loecher
#'
#' @param post Simulated results from the posterior, as provided by sim_post()
#'
#' @return Probabilities each arm is the winner.
#'
#' @examples
#'
#' x <- c(10,20,30,50)
#' n <- c(100,102,120,130)
#' betaPost <- sim_post(x,n)
#' pw <- prob_winner(betaPost)
#'
#' @export
prob_winner <- function(post){
  k <- ncol(post)
  w <- table(factor(max.col(post), levels = 1:k))
  return(w/sum(w))
}

#' On-the-fly indicator function for use in formulae
#'
#' @param cond a logical condition to be evaluated
#' @return a binary (0/1) coded variable indicating whether the condition is true
#'
#' @export
ind <- function(cond) {
  ifelse(cond, 1L, 0L)
}


#' Convert all factor columns in data.table to numeric
#'
#' @param dt a data.table
#' @return the data.table with column factors converted to numeric
#'
#' @export
data_table_factors_to_numeric <- function(dt){
  setDT(dt)
  factor_cols <- names(which(sapply(dt, class)=="factor"))
  if(length(factor_cols) > 0) {
    suppressWarnings(dt[,(factor_cols) :=
                          lapply(.SD, function(x) as.numeric(as.character(x))),.SDcols=factor_cols])
  }
  return(dt)
}
