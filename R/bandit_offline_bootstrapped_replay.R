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
    rows = NULL,
    formula = NULL
  ),
  public = list(
    class_name = "OfflineBootstrappedReplayBandit",
    randomize = NULL,
    replacement = NULL,
    jitter = NULL,
    arm_multiply = NULL,
    flat_context = NULL,
    context_free = NULL,
    initialize   = function(formula,
                            data, k = NULL, d = NULL,
                            unique = NULL, shared = NULL,
                            randomize = TRUE, replacement = TRUE,
                            jitter = TRUE, arm_multiply = TRUE,
                            multiplier = 1) {

      private$S         <- data_table_factors_to_numeric(data)

      private$formula   <- Formula::as.Formula(formula)

      if (is.null(k) || is.null(d)) {
        self$k            <- max(Formula::model.part(private$formula, data = private$S,
                                                     lhs = 0, rhs = 1, drop = TRUE))
        self$d            <- suppressWarnings(
                                 length(model.matrix(private$formula, data = private$S[1,], rhs = 2)[,-1])
                             )
        self$flat_context <- TRUE
      } else {
        self$k            <- k
        self$d            <- d
        self$flat_context <- FALSE
      }
      if(self$d == 0) {
        self$d <- 1
        self$context_free <- TRUE
      } else {
        self$context_free <- FALSE
      }
      self$arm_multiply <- arm_multiply   # bootstrapped bandit needs a horizon of at least k arms times the
                                          # horizon by setting this arm_multipy flag, the Simulator knows to
                                          # continue running for (k * horizon) steps.

      if(isTRUE(arm_multiply))
        private$S <- do.call("rbind", replicate(self$k * multiplier, private$S, simplify = FALSE))
      else
        private$S <- do.call("rbind", replicate(multiplier, private$S, simplify = FALSE))

      self$randomize    <- randomize      # Randomize logged events within each simulation? (logical)
      self$replacement  <- replacement    # Sample with replacement? (logical)

      self$jitter       <- jitter         # jitter over contextual features (logical)

      self$unique       <- unique         # unique arm ids
      self$shared       <- shared         # shared arm ids

      private$oa       <- "optimal_arm" %in% colnames(data)
      private$or       <- "optimal_reward" %in% colnames(data)
    },
    post_initialization = function() {
      if(isTRUE(self$randomize)) private$S <- private$S[sample(nrow(private$S), replace = self$replacement)]
      if (self$context_free) {
        private$x <- matrix(1,nrow(private$S))
      } else {
        private$x <- model.matrix(private$formula, data = private$S, rhs = 2)
        if(!isTRUE(self$flat_context))  private$x <- private$x[,-1]
        if(isTRUE(self$jitter)) private$x <- apply(private$x, 2, jitter)
      }
      private$y <- Formula::model.part(private$formula, data = private$S, lhs = 1, rhs = 0, drop = TRUE)
      private$z <- Formula::model.part(private$formula, data = private$S, lhs = 0, rhs = 1, drop = TRUE)
      private$rows <- nrow(private$S)
    },
    get_context = function(index) {
      if(index > private$rows) return(NULL)
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
#' Policy for the evaluation of policies with offline data through replay with bootstrapping.
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
#' @section Usage:
#' \preformatted{
#'   bandit <- OfflineBootstrappedReplayBandit(formula,
#'                                             data, k = NULL, d = NULL,
#'                                             unique = NULL, shared = NULL,
#'                                             randomize = TRUE, replacement = TRUE,
#'                                             jitter = TRUE, arm_multiply = TRUE)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{formula}}{
#'     formula (required). Format: \code{y.context ~ z.choice | x1.context + x2.xontext + ...}
#'     By default,  adds an intercept to the context model. Exclude the intercept, by adding "0" or "-1" to
#'     the list of contextual features, as in: \code{y.context ~ z.choice | x1.context + x2.xontext -1}
#'   }
#'   \item{\code{data}}{
#'     data.table or data.frame; offline data source (required)
#'   }
#'   \item{\code{k}}{
#'     integer; number of arms (optional). Optionally used to reformat the formula defined x.context vector
#'     as a \code{k x d} matrix. When making use of such matrix formatted contexts, you need to define custom
#'     intercept(s) when and where needed in data.table or data.frame.
#'   }
#'   \item{\code{d}}{
#'     integer; number of contextual features (optional) Optionally used to reformat the formula defined
#'     x.context vector as a \code{k x d} matrix. When making use of such matrix formatted contexts, you need
#'     to define custom intercept(s) when and where needed in data.table or data.frame.
#'   }
#'   \item{\code{randomize}}{
#'     logical; randomize rows of data stream per simulation (optional, default: TRUE)
#'   }
#'   \item{\code{replacement}}{
#'     logical; sample with replacement (optional, default: TRUE)
#'   }
#'   \item{\code{jitter}}{
#'     logical; add jitter to contextual features (optional, default: TRUE)
#'   }
#'   \item{\code{arm_multiply}}{
#'     logical; multiply the horizon by the number of arms (optional, default: TRUE)
#'   }
#'   \item{\code{multiplier}}{
#'     integer; replicate the dataset \code{multiplier} times before randomization. When
#'     \code{arm_multiply} has been set to TRUE, the number of replications is the number of arms times
#'     this integer. Can be used when Simulator's policy_time_loop has been set to TRUE, otherwise a
#'     simulation might run out of pre-indexed data.
#'   }
#'
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
#'   \item{\code{new(formula, data, k = NULL, d = NULL, unique = NULL, shared = NULL, randomize = TRUE,
#'                   replacement = TRUE, jitter = TRUE, arm_multiply = TRUE)}}{ generates
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
#' Mary, J., Preux, P., & Nicol, O. (2014, January). Improving offline evaluation of contextual bandit
#' algorithms via bootstrapping techniques. In International Conference on Machine Learning (pp. 172-180).
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
#' library(contextual)
#' library(data.table)
#'
#' # Import personalization data-set
#'
#' url         <- "http://d1ie9wlkzugsxr.cloudfront.net/data_cmab_basic/dataset.txt"
#' datafile    <- fread(url)
#'
#' simulations <- 1
#' horizon     <- nrow(datafile)
#'
#' bandit      <- OfflineReplayEvaluatorBandit$new(formula = V2 ~ V1 | . - V1, data = datafile)
#'
#' # Define agents.
#' agents      <- list(Agent$new(LinUCBDisjointOptimizedPolicy$new(0.01), bandit, "alpha = 0.01"),
#'                     Agent$new(LinUCBDisjointOptimizedPolicy$new(0.05), bandit, "alpha = 0.05"),
#'                     Agent$new(LinUCBDisjointOptimizedPolicy$new(0.1),  bandit, "alpha = 0.1"),
#'                     Agent$new(LinUCBDisjointOptimizedPolicy$new(1.0),  bandit, "alpha = 1.0"))
#'
#' # Initialize the simulation.
#'
#' simulation  <- Simulator$new(agents = agents, simulations = simulations, horizon = horizon,
#'                              do_parallel = FALSE, save_context = TRUE)
#'
#' # Run the simulation.
#' sim  <- simulation$run()
#'
#' # plot the results
#' plot(sim, type = "cumulative", regret = FALSE, rate = TRUE,
#'      legend_position = "bottomright", ylim = c(0,1))
#'
#'
#' }
NULL
