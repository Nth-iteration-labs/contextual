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
