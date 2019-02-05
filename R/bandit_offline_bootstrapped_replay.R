#' @export
#' @import Formula
OfflineBootstrappedReplayBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  private = list(
    S = NULL,
    oa = NULL,
    or = NULL,
    x = NULL,
    y = NULL,
    z = NULL,
    formula = NULL
  ),
  public = list(
    class_name = "OfflineBootstrappedReplayBandit",
    randomize = NULL,
    replacement = NULL,
    jitter = NULL,
    arm_multiply = NULL,
    flat_context = NULL,
    initialize   = function(formula,
                            data, k = NULL, d = NULL,
                            unique = NULL, shared = NULL,
                            randomize = TRUE, replacement = TRUE,
                            jitter = TRUE, arm_multiply = TRUE) {

      private$S         <- data           # Logged events

      private$formula   <- Formula::as.Formula(formula)

      if (is.null(k) || is.null(d)) {
        self$k            <- max(Formula::model.part(private$formula, data = private$S,
                                                     lhs = 0, rhs = 1, drop = TRUE))
        self$d            <- length(model.matrix(private$formula, data = private$S[1,], rhs = 2)[,-1])
        self$flat_context <- TRUE
      } else {
        self$k            <- k
        self$d            <- d
        self$flat_context <- FALSE
      }

      self$arm_multiply <- arm_multiply

      if(isTRUE(arm_multiply))  private$S <- do.call("rbind", replicate(self$k, private$S, simplify = FALSE))

      self$randomize    <- randomize      # Randomize logged events within each simulation? (logical)
      self$replacement  <- replacement    # Sample with replacement? (logical)

      self$jitter       <- jitter         # jitter over contextual features (logical)

      self$unique       <- unique         # unique arm ids
      self$shared       <- shared         # shared arm ids

      if(!"context" %in% colnames(private$S)) private$S$context = list(1)
      private$S[is.null(context[[1]]),`:=`(context = list(1))]

      private$oa       <- "optimal_arm" %in% colnames(data)
      private$or       <- "optimal_reward" %in% colnames(data)
    },
    post_initialization = function() {
      if(isTRUE(self$randomize)) private$S <- private$S[sample(nrow(private$S), replace = self$replacement)]
      private$x <-  model.matrix(private$formula, data = private$S, rhs = 2)
      if(!isTRUE(self$flat_context))  private$x <- private$x[,-1]
      if(isTRUE(self$jitter)) private$x <- apply(private$x, 2, jitter)
      private$y <- Formula::model.part(private$formula, data = private$S, lhs = 1, rhs = 0, drop = TRUE)
      private$z <- Formula::model.part(private$formula, data = private$S, lhs = 0, rhs = 1, drop = TRUE)
    },
    get_context = function(index) {
      context <- list(
        k      = self$k,
        d      = self$d,
        unique = self$unique,
        shared = self$shared,
        X      = if(isTRUE(self$flat_context)) private$x[index,] else matrix(private$x[index,],self$d,self$k)
      )
      context
    },
    get_reward = function(index, context, action) {
      if (private$z[[index]] == action$choice) {
        list(
          reward         = as.double(private$y[[index]]),
          optimal_reward = ifelse(private$or, as.double(private$S$optimal_reward[[index]]), NA),
          optimal_arm    = ifelse(private$oa, as.double(private$S$optimal_arm[[index]]), NA)
        )
      } else {
        NULL
      }
    }
  )
)

#' Bandit: Offline Bootstrapped Replay
#'
#' Policy for the evaluation of policies with offline data.
#'
#' The key assumption of the method is that that the original logging policy chose
#' i.i.d. arms uniformly at random.
#'
#' Take care: if the original logging policy does not change over trials, data may be
#' used more efficiently via propensity scoring (Langford et al., 2008; Strehl et al., 2011)
#' and related techniques like doubly robust estimation (Dudik et al., 2011).
#'
#' @name OfflineBootstrappedReplayBandit
#'
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- OfflineBootstrappedReplayBandit(offline_data, k, d, unique = NULL, shared = NULL, randomize = TRUE)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{offline_data}}{
#'     data.table; offline data source (required)
#'   }
#'   \item{\code{k}}{
#'     integer; number of arms (required)
#'   }
#'   \item{\code{d}}{
#'     integer; number of contextual features (optional, default: 0)
#'   }
#'   \item{\code{randomize}}{
#'     logical; randomize rows of data stream per simulation (optional, default: TRUE)
#'   }
#'   \item{\code{unique}}{
#'     integer vector; index of disjoint features (optional)
#'   }
#'   \item{\code{shared}}{
#'     integer vector; index of shared features (optional)
#'   }
#'
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(offline_data, k, d, unique = NULL, shared = NULL, randomize = TRUE)}}{ generates
#'    and instantializes a new \code{OfflineBootstrappedReplayBandit} instance. }
#'
#'   \item{\code{get_context(t)}}{
#'      argument:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'      }
#'      returns a named \code{list}
#'      containing the current \code{d x k} dimensional matrix \code{context$X},
#'      the number of arms \code{context$k} and the number of features \code{context$d}.
#'  }
#'
#'   \item{\code{get_reward(t, context, action)}}{
#'      arguments:
#'      \itemize{
#'          \item \code{t}: integer, time step \code{t}.
#'          \item \code{context}: list, containing the current \code{context$X} (d x k context matrix),
#'          \code{context$k} (number of arms) and \code{context$d} (number of context features)
#'          (as set by \code{bandit}).
#'          \item \code{action}:  list, containing \code{action$choice} (as set by \code{policy}).
#'      }
#'      returns a named \code{list} containing \code{reward$reward} and, where computable,
#'         \code{reward$optimal} (used by "oracle" policies and to calculate regret).
#'  }
#'
#'   \item{\code{post_initialization()}}{
#'      Randomize offline data by shuffling the offline data.table before the start of each
#'      individual simulation when self$randomize is TRUE (default)
#'   }
#' }
#'
#' @references
#'
#' Agrawal, R. (1995). The continuum-armed bandit problem. SIAM journal on control and optimization,
#' 33(6), 1926-1951.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},
#' \code{\link{OfflineBootstrappedReplayBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualLinTSPolicy}}
#'
#' @examples
#' \dontrun{
#'
#' ## generate random policy log and save it
#'
#' context_weights    <- matrix(  c( 0.9, 0.1, 0.1,
#'                                   0.1, 0.9, 0.1,
#'                                   0.1, 0.1, 0.9 ), nrow = 3, ncol = 3, byrow = TRUE)
#' horizon     <- 2000L
#' simulations <- 1L
#' bandit      <- ContextualBinaryBandit$new(weights = context_weights)
#'
#' # For the generation of random data choose a random policy,
#' # otherwise rejection sampling will produce biased results.
#'
#' policy      <- RandomPolicy$new()
#'
#' agent       <- Agent$new(policy, bandit)
#'
#' simulation  <-
#'   Simulator$new(
#'     agent,
#'     horizon = horizon,
#'     simulations = simulations,
#'     save_context = TRUE
#'   )
#'
#' random_offline_data <- simulation$run()
#' random_offline_data$save("log.RData")
#'
#' ## use saved log to evaluate policies with OfflineBootstrappedReplayBandit
#'
#' history <- History$new()
#' history$load("log.RData")
#' log_S <- history$get_data_table()
#'
#' bandit <- OfflineBootstrappedReplayBandit$new(offline_data = log_S, k = 3, d = 3)
#'
#' agents <-
#'   list(
#'     Agent$new(EpsilonGreedyPolicy$new(0.01), bandit),
#'     Agent$new(LinUCBDisjointPolicy$new(0.6), bandit)
#'   )
#'
#' simulation <-
#'   Simulator$new(
#'     agents,
#'     horizon = horizon,
#'     simulations = simulations,
#'     t_over_sims = TRUE,
#'     do_parallel = FALSE
#'   )
#'
#' li_bandit_history <- simulation$run()
#'
#' plot(after, regret = FALSE, type = "cumulative", rate = TRUE)
#'
#' if (file.exists("log.RData")) file.remove("log.RData")
#'
#' }
NULL
