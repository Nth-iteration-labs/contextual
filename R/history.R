#' @importFrom data.table data.table set
#' @export
History <- R6::R6Class(
  "History",
  portable = FALSE,
  class = TRUE,
  public = list(
    n = NULL,
    save_theta = NULL,
    save_context = NULL,
    initialize = function(n = 1, save_context = FALSE, save_theta = FALSE) {
      self$n                          <- n
      self$save_context               <- save_context
      self$save_theta                 <- save_theta
      self$reset()
    },
    reset = function() {
      self$clear_data_table()
      private$.data <- data.table::data.table(
        t                             = rep(0L,      n),
        sim                           = rep(0L,      n),
        choice                        = rep(0.0,     n),
        reward                        = rep(0.0,     n),
        choice_is_optimal             = rep(0L,      n),
        optimal_reward_value          = rep(0.0,     n),
        propensity                    = rep(0.0,     n),
        agent                         = rep("",      n)
      )
      if (self$save_context) private$.data$context <- rep(list(),  n)
      if (self$save_theta)   private$.data$theta   <- rep(list(),  n)
      invisible(self)
    },
    save = function(   index,
                       t,
                       action,
                       reward,
                       policy_name,
                       simulation_index,
                       context_value = NA,
                       theta_value = NA
    ) {

      index <- as.integer(index)

      if (!is.null(action$propensity)) propensity <- action$propensity
      else propensity <- NA

      if (!is.null(reward$optimal_reward_value)) optimal_reward_value <- reward$optimal_reward_value
      else  optimal_reward_value <- NA

      data.table::set(
        data,
        index,
        1L:8L,
        list(
          t,
          simulation_index,
          action$choice,
          reward$reward,
          as.integer(reward$reward == optimal_reward_value),
          optimal_reward_value,
          propensity,
          policy_name
        )
      )
      if (self$save_context | self$save_theta) {
        if (!self$save_theta) {
          data.table::set(data, index, 9L , list(list(context_value)))
        } else if (!self$save_context) {
          data.table::set(data, index, 9L, list(list(theta_value)))
          ## if split over mult col
          #data[index, (paste0("X.", seq_along(context))) := context]
        } else {
          data.table::set(data, index, 9L, list(list(context_value)))
          data.table::set(data, index, 10L , list(list(theta_value)))
        }
      }
      invisible(self)
    },
    cumulative = function(final = TRUE, regret = TRUE, rate = FALSE) {
      if (regret) {
        private$.data$temp <- private$.data$optimal_reward_value - private$.data$reward
      } else {
        private$.data$temp <- private$.data$reward
      }
      if (rate) {
        private$.data$cum_reward_rate <- private$.data[, cumsum(temp)/t, by = list(agent, sim)]$V1
        cum_rewards <- private$.data[, list(cum_reward_rate_var = var(cum_reward_rate),
                                            cum_reward_rate = mean(cum_reward_rate)), by = list(t, agent)]
      } else {
        private$.data$cum_reward <- private$.data[, cumsum(temp), by = list(agent, sim)]$V1
        cum_rewards <- private$.data[, list(cum_reward_var = var(cum_reward),
                                            cum_reward = mean(cum_reward)), by = list(t, agent)]
      }
      private$.data$temp <- NULL
      if (final) {
        agent_levels <- levels(as.factor(cum_rewards$agent))
        final_cum_rewards <- list()
        for (agent_name in agent_levels) {
          final_cum_rewards[[agent_name]] <- tail(cum_rewards[cum_rewards$agent == agent_name], n = 1)[[4]]
        }
        final_cum_rewards
      } else {
        cum_rewards
      }

    },
    save_data = function(filename = NA) {
      if (is.na(filename))
        filename <- paste("contextual_data_",
                          format(Sys.time(), "%y%m%d_%H%M%S"),
                          ".RData",
                          sep = "")
      saveRDS(private$.data, file = filename, compress = TRUE)
      invisible(self)
    },
    load_data = function(filename, nth_rows = 0) {
      if (nrow(private$.data) > 1 && private$.data$agent[[1]] != "") {
        temp_data     <- readRDS(filename)
        if (nth_rows > 0) temp_data <- temp_data[t %% nth_rows == 0]
        private$.data <- rbind(private$.data, temp_data)
        temp_data     <- NULL
      } else {
        private$.data <- readRDS(filename)
        if (nth_rows > 0) private$.data <- private$.data[t %% nth_rows == 0]
      }
      if ( "opimal" %in% colnames(private$.data)) setnames(private$.data, old = "opimal", new = "optimal_reward_value")
      invisible(self)
    },
    leave_nth = function(nth_rows = 0) {
      private$.data <- private$.data[t %% nth_rows == 0]
      reindex_t()
    },
    get_data_frame = function() {
      as.data.frame(private$.data)
    },
    set_data_frame = function(df) {
      private$.data <- as.data.table(df)
      invisible(self)
    },
    get_data_table = function() {
      private$.data
    },
    set_data_table = function(dt) {
      private$.data <- dt
      invisible(self)
    },
    clear_data_table = function() {
      private$.data <- private$.data[0,]
      invisible(self)
    },
    delete_empty_rows = function() {
      private$.data <- private$.data[sim > 0 & t > 0]
      private$.data <- private$.data[, t := seq_len(.N), by = c("agent", "sim")]
      #private$.data[ , max(t), by = c("agent","sim")][,min(V1), by = c("agent")][,V1]
      invisible(self)
    },
    reindex_t = function(truncate = TRUE) {
      private$.data <- private$.data[, t := seq_len(.N), by = c("agent", "sim")]
      if (truncate) {
        min_t_anywhere <- min(private$.data[, .(count = uniqueN(t)), by = c("agent","sim")]$count)
        private$.data <- private$.data[t <= min_t_anywhere]
      }
      invisible(self)
    },
    print_data = function() {
      str(private$.data)
    },
    finalize = function() {
      self$clear_data_table()
    }
  ),
  active = list(
    data = function(value) {
      if (missing(value)) {
        private$.data
      } else {
        warning("## History$data is read only", call. = FALSE)
      }
    }
  ),
  private = list(
    .data = NULL
  )
)

#' History
#'
#' The R6 class \code{History} keeps a log of all \code{Simulator} interactions
#' in its internal \code{data.table}. It also provides basic data summaries,
#' and can save or load simulation log data files.
#'
#' @name History
#' @aliases print_data reindex_t delete_empty_rows clear_data_table set_data_table get_data_table set_data_frame get_data_frame leave_nth load_data cumulative save
#'
#' @section Usage:
#' \preformatted{
#' History <- History$new(n = 1, save_context = FALSE, save_theta = FALSE)
#' }
#'
#' @section Arguments:
#'
#' \describe{
#'   \item{\code{n}}{
#'      \code{integer}. The number of rows, to be preallocated during initialization.
#'   }
#'   \item{\code{save_context}}{
#'     \code{logical}. Save context vectors \code{X} when writing simulation data?
#'   }
#'   \item{\code{save_theta}}{
#'     \code{logical}. Save parameter lists \code{theta} when writing simulation data?
#'   }
#'
#'
#' }
#'
#' @section Methods:
#'
#' \describe{
#'
#'   \item{\code{reset()}}{
#'      Resets a \code{History} instance to its original initialisation values.
#'   }
#'   \item{\code{save(index,
#'                    t,
#'                    action,
#'                    reward,
#'                    policy_name,
#'                    simulation_index,
#'                    context_value = NA,
#'                    theta_value = NA)}}{
#'      Saves one row of simulation data. Is generally not called directly, but from a {Simulator} instance.
#'   }
#'   \item{\code{save_data(filename = NA)}}{
#'      Writes the \code{History} log file in its default data.table format,
#'      with \code{filename} as the name of the file which the data is to be written to.
#'   }
#'   \item{\code{load_data = function(filename, nth_rows = 0)}}{
#'      Reads a \code{History} log file in its default \code{data.table} format,
#'      with \code{filename} as the name of the file which the data are to be read from.
#'      If \code{nth_rows} is larger than 0, every \code{nth_rows} of data is read instead of the
#'      full data file. This can be of use with (a first) analysis of very large data files.
#'   }
#'   \item{\code{get_data_frame()}}{
#'      Returns the \code{History} log as a \code{data.frame}.
#'   }
#'   \item{\code{get_data_table()}}{
#'      Returns the \code{History} log as a \code{data.table}.
#'   }
#'   \item{\code{set_data_table(dt)}}{
#'      Sets the \code{History} log with the data in \code{data.table} \code{dt}.
#'   }
#'   \item{\code{clear_data_table()}}{
#'      Clears the \code{History} log.
#'   }
#'   \item{\code{delete_empty_rows()}}{
#'      Deletes all empty rows in the \code{History} log and re-indexes the \code{t} column grouped
#'      by agent and simulation.
#'   }
#'   \item{\code{reindex_t(truncate = TRUE)}}{
#'      Removes empty rows from the \code{History} log, reindexes the \code{t} column, and,
#'      if \code{truncate} is \code{TRUE}, truncates the resulting data to the shortest simulation
#'      grouped by agent and simulation.
#'   }
#'   \item{\code{print_data()}}{
#'      Prints a summary of the \code{History} log.
#'   }
#'   \item{\code{cumulative(final = TRUE, regret = TRUE, rate = FALSE)}}{
#'      Returns cumulative reward (when \code{regret} is \code{FALSE}) or regret. When \code{final} is \code{TRUE},
#'      it only returns the final value. When \code{final} is FALSE, it returns a \code{data.table} containing all
#'      cumulative reward or regret values from 1 to T.
#'      When \code{rate} is \code{TRUE}, cumulative reward or regret are divided by column \code{t} before any values
#'      are returned.
#'   }
#'  }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBandit}}, \code{\link{ContextualBandit}},  \code{\link{LiSamplingOfflineBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
NULL
