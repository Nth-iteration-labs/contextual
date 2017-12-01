library(R6)
library(data.table)
#' @export
History <- R6Class(
  "History",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  public = list(
    n = 1000,
    data = data.table(),
    initialize = function(n = 1000) {
      self$n = n
      self$reset(n)
    },
    reset = function(n){
      self$data = data.table(
        reward          = rep(0L,     n), #1
        optimal         = rep(0L,     n), #2
        agent           = rep("",     n), #3
        t               = rep(0L,     n), #4
        sim             = rep(0L,     n), #5
        arm             = rep(0L,     n)  #6
      )
    },
    save = function(counter,t,s,action,reward,policy.name) {
      set(self$data,counter,4L,t)
      set(self$data,counter,5L,s)
      set(self$data,counter,6L,action)
      set(self$data,counter,1L,reward$reward)
      set(self$data,counter,3L,policy.name)
      if (reward$is.optimal.choice) set(self$data,counter,2L,1L)
      #set(self$data,counter,7L,bandit.instance[[a,s]]$get.weights())
    },
    get.data.table = function() {
      return(self$data)
    }
  )
)

#' External History
#'
#' History intro
#'
#' @section Usage:
#' \preformatted{b <- History$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{History} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new History, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name History
#' @examples
#'\dontrun{}
#'
NULL

