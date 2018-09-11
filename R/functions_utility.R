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
#' Get maximum value
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
#' max_in(theta$par_one)
#'
#' @export
max_in <- function(x, equal_is_random = TRUE) {
  x <- unlist(x, FALSE, FALSE)
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L)  {
    if (equal_is_random) {
      return(sample(y, 1L, replace = TRUE))
    } else {
      return(y[1])
    }
  } else {
    return(y)
  }
}
#' Get maximum value
#'
#' Returns the index of the maximum value in vector \code{vec}.
#'
#' If there is a tie,
#' the index of one of the tied maxima is returned at random.
#'
#' @param vec vector of values
#'
#' @export
which_max_tied <- function(vec) {
  maxima <- which(vec == max(vec))
  if(length(maxima) > 1){ maxima <- sample(maxima, 1) }
  maxima
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
  .Platform$GUI == "RStudio"
}
#' @title Change Default Graphing Device from RStudio
#' @description
#' Checks to see if the user is in RStudio. If so, then it changes the device to a popup window.
#' @param ext A \code{logical} indicating whether the graph should be done externally or internally in RStudio.
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
#' As a result, any graphs that are open will be deleted. You will have to regraph them.
#'
#' @examples
#' \dontrun{
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
}
#' Sample one of
#'
#' Sample one of the values in a vector.
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
formatted_difftime <- function(x) UseMethod("formatted_difftime")
formatted_difftime.difftime <- function(x) {
  units(x) <- "secs"
  x <- unclass(x)
  NextMethod()
}
formatted_difftime.default <- function(x) {
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
#' parameterzation in R.
#'
#' The functions (d/p/q/r)invgamma simply wrap those of the standard
#' (d/p/q/r)gamma R implementation, so look at, say,
#' \code{\link{dgamma}} for details.
#'
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
#'
#'
#'
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
inv_logit <- function(x){
  exp(x)/(1+exp(x))
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
