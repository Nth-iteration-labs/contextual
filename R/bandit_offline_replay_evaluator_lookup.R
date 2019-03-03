#' @export
OfflineLookupReplayEvaluatorBandit <- R6::R6Class(
  inherit = Bandit,
  class = FALSE,
  private = list(
    S = NULL,
    oa = NULL,
    or = NULL,
    shared_lookup = NULL,
    unique_lookup = NULL,
    unique_col = NULL,
    rows = NULL
  ),
  public = list(
    class_name = "OfflineLookupReplayEvaluatorBandit",
    randomize = NULL,
    initialize   = function(offline_data, k, shared_lookup = NULL, unique_lookup = NULL, unique_col = NULL,
                            unique = NULL, shared = NULL, randomize = TRUE) {


      self$k                   <- k
      self$randomize           <- randomize
      private$S                <- data_table_factors_to_numeric(offline_data)

      if(!is.null(unique_lookup)) {
        dim_u                  <- dim(unique_lookup)[2]-1
        self$unique            <- c(1:dim_u)
        private$unique_lookup  <- as.matrix(unique_lookup[,-1])
        private$unique_col     <- unique_col
      } else {
        dim_u                  <- 0
        self$unique            <- 0
        private$shared_lookup  <- NULL
      }

      if(!is.null(shared_lookup)) {
        dim_s                  <- dim(shared_lookup)[2]-1
        self$shared            <- c((dim_u+1):(dim_s + dim_u))
        private$shared_lookup  <- t(as.matrix(shared_lookup[,-1]))
      } else {
        dim_s                  <- 0
        self$shared            <- 0
        private$shared_lookup  <- NULL
      }

      self$d                   <- dim_s + dim_u

      private$oa               <- "optimal_arm" %in% colnames(offline_data)
      private$or               <- "optimal_reward" %in% colnames(offline_data)

    },
    post_initialization = function() {
      if(isTRUE(self$randomize)) private$S <- private$S[sample(nrow(private$S))]
      private$rows <- nrow(private$S)
    },
    get_context = function(index) {
      if(index > private$rows) return(NULL)
      if (self$unique!=0) {
        ulookup            <- private$unique_lookup[private$S[[private$unique_col]][[index]],]
        unique_matrix      <- matrix(ulookup, ncol = self$k, nrow = length(ulookup))
      } else {
        unique_matrix      <- NULL
      }
      all_matrix         <- rbind(unique_matrix, private$shared_lookup)

      context <- list(
        k      = self$k,
        d      = self$d,
        unique = self$unique,
        shared = self$shared,
        X      = all_matrix
      )
      context
    },
    get_reward = function(index, context, action) {
      if (private$S$choice[[index]] == action$choice) {
        list(
          reward = as.double(private$S$reward[[index]]),

          optimal_reward = ifelse(private$or,
                                  as.double(private$S$optimal_reward[[index]]),
                                  NA),

          optimal_arm    = ifelse(private$oa,
                                  as.double(private$S$optimal_arm[[index]]),
                                  NA)
        )
      } else {
        NULL
      }
    }
  )
)

#' Bandit: Offline Replay with lookup tables
#'
#' Alternative interface for replay style bandit.
#'
#' TODO: Needs to be documented more fully.
#'
#' @name OfflineLookupReplayEvaluatorBandit
#'
#' @section Usage:
#' \preformatted{
#'   bandit <- OfflineLookupReplayEvaluatorBandit(offline_data, k, shared_lookup = NULL, unique_lookup = NULL,
#'    unique_col = NULL, unique = NULL, shared = NULL, randomize = TRUE)
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
#'     integer; number of contextual features (required)
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
#'   \item{\code{new(offline_data, k, shared_lookup = NULL, unique_lookup = NULL,
#'    unique_col = NULL, unique = NULL, shared = NULL, randomize = TRUE)}}{ generates
#'    and instantializes a new \code{OfflineLookupReplayEvaluatorBandit} instance. }
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
#' Agrawal, R. (1995). The continuum-armed bandit problem. SIAM journal on control and optimization, 33(6),
#' 1926-1951.
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflineLookupReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualLinTSPolicy}}
#'
#' @examples
#' \dontrun{
#'
#' library(contextual)
#' library(data.table)
#' library(splitstackshape)
#' library(RCurl)
#'
#' # Import MovieLens ml-10M
#'
#' # Info: https://d1ie9wlkzugsxr.cloudfront.net/data_movielens/ml-10M/README.html
#'
#' movies_dat      <- "http://d1ie9wlkzugsxr.cloudfront.net/data_movielens/ml-10M/movies.dat"
#' ratings_dat     <- "http://d1ie9wlkzugsxr.cloudfront.net/data_movielens/ml-10M/ratings.dat"
#'
#' movies_dat      <- readLines(movies_dat)
#' movies_dat      <- gsub( "::", "~", movies_dat )
#' movies_dat      <- paste0(movies_dat, collapse = "\n")
#' movies_dat      <- fread(movies_dat, sep = "~", quote="")
#' setnames(movies_dat, c("V1", "V2", "V3"), c("MovieID", "Name", "Type"))
#' movies_dat      <- splitstackshape::cSplit_e(movies_dat, "Type", sep = "|", type = "character",
#'                                              fill = 0, drop = TRUE)
#' movies_dat[[3]] <- NULL
#'
#' ratings_dat     <- RCurl::getURL(ratings_dat)
#' ratings_dat     <- readLines(textConnection(ratings_dat))
#' ratings_dat     <- gsub( "::", "~", ratings_dat )
#' ratings_dat     <- paste0(ratings_dat, collapse = "\n")
#' ratings_dat     <- fread(ratings_dat, sep = "~", quote="")
#' setnames(ratings_dat, c("V1", "V2", "V3", "V4"), c("UserID", "MovieID", "Rating", "Timestamp"))
#'
#' all_movies      <- ratings_dat[movies_dat, on=c(MovieID = "MovieID")]
#'
#' all_movies      <- na.omit(all_movies,cols=c("MovieID", "UserID"))
#'
#' rm(movies_dat,ratings_dat)
#'
#' all_movies[, UserID   := as.numeric(as.factor(UserID))]
#'
#' count_movies    <- all_movies[,.(MovieCount = .N), by = MovieID]
#' top_50          <- as.vector(count_movies[order(-MovieCount)][1:50]$MovieID)
#' not_50          <- as.vector(count_movies[order(-MovieCount)][51:nrow(count_movies)]$MovieID)
#'
#' top_50_movies   <- all_movies[MovieID %in% top_50]
#'
#' # Create feature lookup tables - to speed up, MovieID and UserID are
#' # ordered and lined up with the (dt/matrix) default index.
#'
#' # Arm features
#'
#' # MovieID of top 50 ordered from 1 to N:
#' top_50_movies[, MovieID   := as.numeric(as.factor(MovieID))]
#' arm_features    <- top_50_movies[,head(.SD, 1),by = MovieID][,c(1,6:24)]
#' setorder(arm_features,MovieID)
#'
#' # User features
#'
#' # Count of categories for non-top-50 movies normalized per user
#' user_features   <- all_movies[MovieID %in% not_50]
#' user_features[, c("MovieID", "Rating", "Timestamp", "Name"):=NULL]
#' user_features   <- user_features[, lapply(.SD, sum, na.rm=TRUE), by=UserID ]
#' user_features[, total  := rowSums(.SD, na.rm = TRUE), .SDcols = 2:20]
#' user_features[, 2:20 := lapply(.SD, function(x) x/total), .SDcols = 2:20]
#' user_features$total <- NULL
#'
#' # Add users that were not in the set of non-top-50 movies (4 in 10m dataset)
#' all_users <- as.data.table(unique(all_movies$UserID))
#' user_features <- user_features[all_users, on=c(UserID = "V1")]
#' user_features[is.na(user_features)] <- 0
#'
#' setorder(user_features,UserID)
#'
#' rm(all_movies, not_50, top_50, count_movies)
#'
#' # Contextual format
#'
#' top_50_movies[, t := .I]
#' top_50_movies[, sim := 1]
#' top_50_movies[, agent := "Offline"]
#' top_50_movies[, choice := MovieID]
#' top_50_movies[, reward := ifelse(Rating <= 4, 0, 1)]
#'
#' setorder(top_50_movies,Timestamp,Name)
#'
#'
#' # Run simulation
#'
#' simulations <- 1
#' horizon     <- nrow(top_50_movies)
#'
#' bandit      <- OfflineLookupReplayEvaluatorBandit$new(top_50_movies,
#'                                                       k             = 50,
#'                                                       unique_col    = "UserID",
#'                                                       shared_lookup = arm_features,
#'                                                       unique_lookup = user_features)
#' agents      <-
#'   list(Agent$new(ThompsonSamplingPolicy$new(), bandit, "Thompson"),
#'        Agent$new(UCB1Policy$new(), bandit, "UCB1"),
#'        Agent$new(RandomPolicy$new(), bandit, "Random"),
#'        Agent$new(LinUCBHybridOptimizedPolicy$new(0.9), bandit, "LinUCB Hyb 0.9"),
#'        Agent$new(LinUCBDisjointOptimizedPolicy$new(2.1), bandit, "LinUCB Dis 2.1"))
#'
#' simulation  <-
#'   Simulator$new(
#'     agents           = agents,
#'     simulations      = simulations,
#'     horizon          = horizon
#'   )
#'
#' results  <- simulation$run()
#'
#' plot(results, type = "cumulative", regret = FALSE,
#'      rate = TRUE, legend_position = "topleft")
#'
#'
#' }
NULL
