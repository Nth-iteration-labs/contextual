#' @export
DoublyRobustOfflineBandit <- R6::R6Class(
  "DoublyRobustOfflineBandit",
  inherit = BasicBandit,
  portable = TRUE,
  class = FALSE,
  private = list(
    S = NULL
  ),
  public = list(
    initialize   = function(data_file, k, d) {
      self$k <- k
      self$d <- d
      private$S <- data_file$get_data_table()

      # do doubly robust stuff here - regression model?

    },
    get_context = function(index) {
      private$X <- array(matrix(private$S$context[[index]], self$d, self$k),
                         dim = c(self$d, self$k, 1))
      contextlist <- list(
        k = self$k,
        d = self$d,
        X = private$X[,,1]
      )
      contextlist
    },
    do_action = function(action, index) {

      # results of previous policy

      action_at_index      <- as.double(private$S$propensity[[index]])
      reward_at_index      <- as.double(private$S$reward[[index]])
      context_at_index     <- array(matrix(private$S$context[[index]], self$d, self$k), dim = c(self$d, self$k, 1))
      propensity_at_index  <- as.double(private$S$propensity[[index]])


      #matrix(unlist(b$context),nrow = dim(b)[1],byrow = TRUE)[1,]

      # what actually has been chosen
      action$choice

      print(context_at_index)

      # what you can do now: use propensity to compensate for inequal p of rewards
      # and run an (live? online updating? regression, to estimate the reward
      # in this context for this actually chosen action

      # ok, so this is left over from rejection sampling..
      if (private$S$choice[[index]] == action$choice) {
        list(
          reward = self$ips_weighed_reward(reward_at_index, propensity_at_index),
          opimal = self$ips_weighed_reward(as.double(private$S$opimal[[index]]), propensity_at_index)
        )
      } else {
        NULL
      }
    },
    ips_weighed_reward = function(reward_at_index, propensity_at_index) {
      reward_at_index / propensity_at_index
    }
  )
)

#' Bandit: Doubly Robust Offline Bandit
#'
#' \code{DoublyRobustOfflineBandit} evaluates rewards based on offline data
#' collected by some other policy.
#'
#' @name DoublyRobustOfflineBandit
#' @family contextual bandits
#'
#' @section Usage:
#' \preformatted{
#' bandit <- DoublyRobustOfflineBandit(data_file, k, d)
#' }
#'
#' @references
#'
#'  Dudík, M., Langford, J., & Li, L. (2011). Doubly robust policy evaluation and learning. arXiv preprint arXiv:1103.4601.
#'
#'  Dudík, M., Erhan, D., Langford, J., & Li, L. (2014). Doubly robust policy evaluation and optimization. Statistical Science, 485-511.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Contextual}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}, \code{\link{Policy}}
#'
#' @examples
#'
#' horizon            <- 100L
#'
NULL
