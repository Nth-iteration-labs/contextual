# create from scratch if not cache, otherwise do cahce
#' @export
ParserBandit <- R6::R6Class(
  "ParserBandit",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  private = list(
    .W = NULL,      # weights k*d
    .R = NULL,      # rewards matrix
    .X = NULL,      # context matrix
    .oracle = NULL  # oracle
  ),
  public = list(
    d            = 0L,
    k            = 0L,
    means        = 0.0,
    stds         = 0.0,
    reward_family  = NULL,
    feature_type = NULL,
    weight_distribution  = NULL,
    initialize   = function() {
    },
    get_weights = function() {
      private$.W
    },
    set_weights = function(weight_matrix) {
      if (length(weight_matrix) != (self$d * self$k))
        stop("Weight needs to be of length k*d.")
      private$.W <- matrix(weight_matrix,  self$d, self$k)
      invisible(self)
    },
    get_reward = function(action, t) {
        setNames(
          list(
            private$.R[action$choice, t],
            action$choice,
            argmax(private$.R[, t]) == action$choice,
            action$propensity
          ),
          c("reward",
            "choice",
            "is_optimal_choice",
            "propensity")
        )
    },
    get_context = function(t) {
        return(
          setNames(list(self$k, self$d, private$.X[t, ], private$.oracle[, t]),
                c("k", "d", "X", "oracle")))
      }
  )
)

#' External ParserBandit
#'
#' ParserBandit intro
#'
#' @section Usage:
#' \preformatted{b <- ParserBandit$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{ParserBandit} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new ParserBandit, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name ParserBandit
#' @examples
#'\dontrun{}
#'
NULL
