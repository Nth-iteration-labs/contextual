#' @export
OfflineDoublyRobustBandit <- R6::R6Class(
  inherit = OfflineBootstrappedReplayBandit,
  class = FALSE,
  private = list(
    r = NULL,
    p = NULL,
    p_hat = NULL,
    n = NULL
  ),
  public = list(
    inverted = NULL,
    threshold = NULL,
    class_name = "OfflineDoublyRobustBandit",
    initialize   = function(formula,
                            data, k = NULL, d = NULL,
                            unique = NULL, shared = NULL,
                            inverted = FALSE, threshold = 0,
                            randomize = TRUE) {
      self$inverted  <- inverted
      self$threshold <- threshold
      super$initialize(formula,
                       data, k, d,
                       unique, shared,
                       randomize, replacement = FALSE,
                       jitter = FALSE, arm_multiply = FALSE)
    },
    post_initialization = function() {
      super$post_initialization()
      # modeled reward for each arm at each t
      private$r <-  model.matrix(private$formula, data = private$S, rhs = 3)[,-1]
      private$p <-  Formula::model.part(private$formula, data = private$S, lhs = 0, rhs = 4, drop = TRUE)
      if (length(private$p) == 0 || is.null(private$p)) {
        marginal_prob <- table(private$z)/length(private$z)
        private$p <- marginal_prob[private$z]
      }
      private$n     <- 0
      private$p_hat <- 0
    },
    get_reward = function(index, context, action) {
      choice            <- action$choice
      data_reward       <- private$y[index]
      model_reward      <- private$r[index,choice]
      p                 <- private$p[index]
      indicator         <- ind(private$z[index] == choice)
      if (indicator) {
        p   <- private$p[index]
        if (self$inverted) p <- 1 / p
        if (self$threshold > 0) {
          if (isTRUE(self$inverted)) p <- 1 / p
          p <- 1 / max(p,self$threshold)
        } else {
          if (isFALSE(self$inverted)) p <- 1 / p
        }
        inc(private$n)     <- 1
        inc(private$p_hat) <- (p - private$p_hat) / private$n
        prop_reward        <- ((data_reward - model_reward) * p) / private$p_hat
      } else {
        prop_reward       <- 0
      }
      robust_reward <-  prop_reward + model_reward
      list(
        reward         = as.double(robust_reward),
        optimal_reward = ifelse(private$or, as.double(private$S$optimal_reward[[index]]), NA),
        optimal_arm    = ifelse(private$oa, as.double(private$S$optimal_arm[[index]]), NA)
      )
    }
  )
)

#' Bandit: Offline Doubly Robust
#'
#' Bandit for the doubly robust evaluation of policies with offline data.
#'
#' @name OfflineDoublyRobustBandit
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- OfflineDoublyRobustBandit(formula,
#'                                       data, k = NULL, d = NULL,
#'                                       unique = NULL, shared = NULL,
#'                                       inverted = FALSE, randomize = TRUE)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{formula}}{
#'     formula (required).
#'     Format:
#'     \code{y.context ~ z.choice | x1.context + x2.xontext + ... | r1.reward + r2.reward ... | p.propensity}
#'     Here, r1.reward to rk.reward represent regression based precalculated rewards per arm.
#'     When leaving out p.propensity, Doubly Robust Bandit uses marginal prob per arm for propensities:
#      table(private$z)/length(private$z).
#'     Adds an intercept to the context model by default. Exclude the intercept, by adding "0" or "-1" to
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
#'     logical; sample with replacement (optional, default: FALSE)
#'   }
#'   \item{\code{jitter}}{
#'     logical; add jitter to contextual features (optional, default: FALSE)
#'   }
#'   \item{\code{unique}}{
#'     integer vector; index of disjoint features (optional)
#'   }
#'   \item{\code{shared}}{
#'     integer vector; index of shared features (optional)
#'   }
#'   \item{\code{inverted}}{
#'     logical; have the propensities been inverted (1/p) or not (p)?
#'   }
#'   \item{\code{threshold}}{
#'     float [0,1]; Lower threshold or Tau on propensity score values. Smaller Tau makes for less biased
#'     estimates with more variance, and vice versa. For more information, see paper by Strehl at all (2010).
#'     Values between 0.01 and 0.05 are known to work well.
#'   }
#'
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{new(formula, data, k = NULL, d = NULL, unique = NULL, shared = NULL, randomize = TRUE)}}{
#'   generates and instantializes a new \code{OfflineDoublyRobustBandit} instance. }
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
#' Dudík, Miroslav, John Langford, and Lihong Li. "Doubly robust policy evaluation and learning."
#' arXiv preprint arXiv:1103.4601 (2011).
#'
#' Agarwal, Alekh, et al. "Taming the monster: A fast and simple algorithm for contextual bandits."
#' International Conference on Machine Learning. 2014.
#'
#' Strehl, Alex, et al. "Learning from logged implicit exploration data." Advances in Neural Information
#' Processing Systems. 2010.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},
#' \code{\link{OfflineDoublyRobustBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualLinTSPolicy}}
#'
#' @examples
#' \dontrun{
#'
#' library(contextual)
#' ibrary(data.table)
#'
#' # Import myocardial infection dataset
#'
#' url  <- "http://d1ie9wlkzugsxr.cloudfront.net/data_propensity/myocardial_propensity.csv"
#' data            <- fread(url)
#'
#' simulations     <- 300
#' horizon         <- nrow(data)
#'
#' # arms always start at 1
#' data$trt        <- data$trt + 1
#'
#' # turn death into alive, making it a reward
#' data$alive      <- abs(data$death - 1)
#'
#' # Run regression per arm, predict outcomes, and save results, a column per arm
#'
#' f                <- alive ~ age + risk + severity
#'
#' model_f          <- function(arm) glm(f, data=data[trt==arm],
#'                                       family=binomial(link="logit"),
#'                                       y=FALSE, model=FALSE)
#' arms             <- sort(unique(data$trt))
#' model_arms       <- lapply(arms, FUN = model_f)
#'
#' predict_arm      <- function(model) predict(model, data, type = "response")
#' r_data           <- lapply(model_arms, FUN = predict_arm)
#' r_data           <- do.call(cbind, r_data)
#' colnames(r_data) <- paste0("r", (1:max(arms)))
#'
#' # Bind data and model predictions
#'
#' data             <- cbind(data,r_data)
#'
#' m      <- glm(I(trt-1) ~ age + risk + severity, data=data, family=binomial(link="logit"))
#' data$p <-predict(m, type = "response")
#'
#' f          <- alive ~ trt | age + risk + severity | r1 + r2 | p
#'
#' bandit     <- OfflineDoublyRobustBandit$new(formula = f, data = data)
#'
#' # Define agents.
#' agents      <- list(Agent$new(LinUCBDisjointOptimizedPolicy$new(0.2), bandit, "LinUCB"),
#'                     Agent$new(FixedPolicy$new(1), bandit, "Arm1"),
#'                     Agent$new(FixedPolicy$new(2), bandit, "Arm2"))
#'
#' # Initialize the simulation.
#'
#' simulation  <- Simulator$new(agents = agents, simulations = simulations, horizon = horizon)
#'
#' # Run the simulation.
#' sim  <- simulation$run()
#'
#' # plot the results
#' plot(sim, type = "cumulative", regret = FALSE, rate = TRUE, legend_position = "bottomright")
#'
#' plot(sim, type = "arms", limit_agents = "LinUCB")
#'
#' }
NULL
