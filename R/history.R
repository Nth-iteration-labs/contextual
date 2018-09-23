#' @importFrom data.table data.table set setorder setkeyv copy uniqueN setcolorder
#' @import checkmate
#' @export
History <- R6::R6Class(
  "History",
  portable = FALSE,
  cloneable = FALSE,
  class = TRUE,

  public = list(
    n            = NULL,
    save_theta   = NULL,
    save_context = NULL,
    initialize = function(n = 1, save_context = FALSE, save_theta = FALSE) {
      self$n            <- n
      self$save_context <- save_context
      self$save_theta   <- save_theta
      self$reset()
    },
    reset = function() {
      gc(reset = FALSE, full = TRUE)
      self$clear_data_table()
      private$initialize_data_tables()
      invisible(self)
    },
    update_statistics = function() {
      calculate_cum_stats()
    },
    insert = function(index,
                      t,
                      action,
                      reward,
                      agent_name,
                      simulation_index,
                      context_value     = NA,
                      theta_value       = NA) {

      if (is.null(action[["propensity"]])) {
        propensity <- NA
      } else {
        propensity <- action[["propensity"]]
      }

      if (is.null(reward[["optimal_reward"]])) {
        optimal_reward <- NA
      } else {
        optimal_reward <- reward[["optimal_reward"]]
      }

      if (is.null(reward[["optimal_arm"]])) {
        optimal_arm <- NA
      } else {
        optimal_arm <- reward[["optimal_arm"]]
      }

      data.table::set(
        data,
        index,
        1L:9L,
        list(
          t,
          simulation_index,
          action[["choice"]],
          reward[["reward"]],
          as.integer(reward$reward == optimal_reward),
          as.integer(optimal_arm),
          optimal_reward,
          propensity,
          agent_name
        )
      )


      if (save_context || save_theta) {
        if (!save_theta) {
          data.table::set(data, index, 10L, list(list(context_value)))
        } else if (!self$save_context) {
          data.table::set(data, index, 10L, list(list(theta_value)))
        } else {
          data.table::set(data, index, 10L, list(list(context_value)))
          data.table::set(data, index, 11L, list(list(theta_value)))
        }
      }
      invisible(self)
    },
    get_agent_list = function() {
      levels(as.factor(private$.data$agent))
    },
    get_agent_count = function() {
      length(self$get_agent_list())
    },
    get_simulation_count = function() {
      length(levels(as.factor(private$.data$sim)))
    },
    get_meta_data = function() {
        private$.meta
    },
    set_meta_data = function(key, value, group = "sim", agent_name = NULL) {
      upsert <- list()
      upsert[[key]] <- value
      if(!is.null(agent_name)) {
        agent <- list()
        private$.meta[[group]][[key]][[agent_name]] <- NULL
        agent[[agent_name]]    <- append(agent[[agent_name]], upsert)
        private$.meta[[group]] <- append(private$.meta[[group]],agent)
      } else {
        private$.meta[[group]][[key]] <- NULL
        private$.meta[[group]] <- append(private$.meta[[group]],upsert)
      }
    },
    get_cumulative_data = function(limit_agents = NULL, limit_cols = NULL, interval = 1) {
      if (is.null(limit_agents)) {
        if (is.null(limit_cols)) {
          private$.cum_stats[t %% interval == 0 | t == 1]
        } else {
          private$.cum_stats[t %% interval == 0 | t == 1, mget(limit_cols)]
        }
      } else {
        if (is.null(limit_cols)) {
          private$.cum_stats[agent %in% limit_agents][t %% interval == 0 | t == 1]
        } else {
          private$.cum_stats[agent %in% limit_agents][t %% interval == 0 | t == 1, mget(limit_cols)]
        }
      }
    },
    get_cumulative_result = function(limit_agents = NULL, as_list = TRUE, limit_cols = NULL, t = NULL) {
      if (is.null(t)) {
        idx <- private$.cum_stats[, .(idx = .I[.N]),   by=agent]$idx
      } else {
        t_int <- as.integer(t)
        idx <- private$.cum_stats[, .(idx = .I[t==t_int]), by=agent]$idx
      }
      cum_results <- private$.cum_stats[idx]
      if (is.null(limit_cols)) {
        if (is.null(limit_agents)) {
          if (as_list) {
            private$data_table_to_named_nested_list(cum_results, transpose = FALSE)
          } else {
            cum_results
          }
        } else {
          if (as_list) {
            private$data_table_to_named_nested_list(cum_results[agent %in% limit_agents], transpose = FALSE)
          } else {
            cum_results(cum_results[agent %in% limit_agents])
          }
        }
      } else {
        if (is.null(limit_agents)) {
          if (as_list) {
            private$data_table_to_named_nested_list(cum_results[, mget(limit_cols)], transpose = FALSE)
          } else {
            cum_results[, mget(limit_cols)]
          }
        } else {
          if (as_list) {
            private$data_table_to_named_nested_list(cum_results[, mget(limit_cols)]
                                            [agent %in% limit_agents], transpose = FALSE)
          } else {
            cum_results(cum_results[, mget(limit_cols)][agent %in% limit_agents])
          }
        }
      }
    },
    save = function(filename = NA) {
      if (is.na(filename)) {
        filename <- paste("contextual_data_",
          format(Sys.time(), "%y%m%d_%H%M%S"),
          ".RData",
          sep = ""
        )
      }
      attr(private$.data, "meta") <- private$.meta
      saveRDS(private$.data, file = filename, compress = TRUE)
      invisible(self)
    },
    load = function(filename, interval = 0, auto_stats = TRUE, bind_to_existing = FALSE) {
      if (isTRUE(bind_to_existing) && nrow(private$.data) > 1 && private$.data$agent[[1]] != "") {
        temp_data <- readRDS(filename)
        if (interval > 0) temp_data <- temp_data[t %% interval == 0]
        private$.data <- rbind(private$.data, temp_data)
        temp_data <- NULL
      } else {
        private$.data <- readRDS(filename)
        if (interval > 0) private$.data <- private$.data[t %% interval == 0]
      }
      private$.meta <- attributes(private$.data)$meta
      if ("opimal" %in% colnames(private$.data))
        setnames(private$.data, old = "opimal", new = "optimal_reward")
      if (isTRUE(auto_stats)) private$calculate_cum_stats()
      invisible(self)
    },
    save_csv = function(filename = NA, context_to_columns = FALSE) {
      if (is.na(filename)) {
        filename <- paste("contextual_data_",
                          format(Sys.time(), "%y%m%d_%H%M%S"),
                          ".csv",
                          sep = ""
        )
      }
      if (context_to_columns) {
        context_cols <- c(paste0("X.", seq_along(unlist(private$.data[1,]$context))))
        dt_to_save <- copy(private$.data)
        dt_to_save[, context_string := lapply(.SD, function(x) paste( unlist(x), collapse=',') ),
                                    by=1:nrow(private$.data), .SDcols = c("context")]
        one_context <- private$.data[1,]$context[[1]]
        if (is.vector(one_context)) one_context <- matrix(one_context, nrow = 1L)
        dt_to_save$k <- dim(one_context)[2]
        dt_to_save$d <- dim(one_context)[1]
        dt_to_save[, (context_cols) := tstrsplit(context_string, ",", fixed=TRUE)]
        dt_to_save$context <- NULL
        if ("theta" %in% names(dt_to_save)) dt_to_save$theta <- NULL
        dt_to_save$context_string <- NULL
        fwrite(dt_to_save[,which(dt_to_save[,colSums(is.na(dt_to_save))<nrow(dt_to_save)]), with = F],
               file = filename)
        rm(dt_to_save)
        gc()
      } else {
        if ("theta" %in% names(private$.data)) {
          fwrite(private$.data[,which(private$.data[,colSums(is.na(private$.data))<nrow(private$.data)]),
                             with =FALSE][, !"theta", with=FALSE], file = filename)
        } else {
          fwrite(private$.data[,which(private$.data[,colSums(is.na(private$.data))<nrow(private$.data)]),
                               with =FALSE], file = filename)
        }
      }
      invisible(self)
    },
    get_data_frame = function(context_to_columns = FALSE) {
        as.data.frame(private$.data)
    },
    set_data_frame = function(df, auto_stats = TRUE) {
      private$.data <- as.data.table(df)
      if (isTRUE(auto_stats)) private$calculate_cum_stats()
      invisible(self)
    },
    get_data_table = function(limit_agents = NULL, limit_cols = NULL, interval = 1, no_zero_sim = FALSE) {
      if (is.null(limit_agents)) {
        if (is.null(limit_cols)) {
          private$.data[t %% interval == 0 | t == 1][sim != 0]
        } else {
          private$.data[t %% interval == 0 | t == 1, mget(limit_cols)][sim != 0]
        }
      } else {
        if (is.null(limit_cols)) {
          private$.data[agent %in% limit_agents][t %% interval == 0 | t == 1][sim != 0]
        } else {
          private$.data[agent %in% limit_agents][t %% interval == 0 | t == 1, mget(limit_cols)][sim != 0]
        }
      }
    },
    set_data_table = function(dt, auto_stats = TRUE) {
      private$.data <- dt
      if (isTRUE(auto_stats)) private$calculate_cum_stats()
      invisible(self)
    },
    clear_data_table = function() {
      private$.data <- private$.data[0, ]
      invisible(self)
    },
    delete_empty_rows = function() {
      private$.data <- private$.data[sim > 0 & t > 0]
      private$.data <- private$.data[, t := seq_len(.N), by = c("agent", "sim")]
      # private$.data[ , max(t), by = c("agent","sim")][,min(V1), by = c("agent")][,V1]
      invisible(self)
    },
    reindex = function(truncate = TRUE) {
      private$.data <- private$.data[, t := seq_len(.N), by = c("agent", "sim")]
      if (truncate) {
        min_t_anywhere <- min(private$.data[, .(count = data.table::uniqueN(t)), by = c("agent", "sim")]$count)
        private$.data <- private$.data[t <= min_t_anywhere]
      }
      invisible(self)
    },
    finalize = function() {
      self$clear_data_table()
    }
  ),
  private = list(
    .data            = NULL,
    .meta            = NULL,
    .cum_stats       = NULL,

    initialize_data_tables = function() {
      private$.data <- data.table::data.table(
        t = rep(0L, self$n),
        sim = rep(0L, self$n),
        choice = rep(0.0, self$n),
        reward = rep(0.0, self$n),
        choice_is_optimal = rep(0L, self$n),
        optimal_arm = rep(0L, self$n),
        optimal_reward = rep(0.0, self$n),
        propensity = rep(0.0, self$n),
        agent = rep("", self$n)
      )
      if (self$save_context) private$.data$context <- rep(list(), self$n)
      if (self$save_theta) private$.data$theta <- rep(list(), self$n)

      # meta data
      private$.meta <- list()

      # cumulative data
      private$.cum_stats <- data.table::data.table()
    },

    calculate_cum_stats = function() {


      self$set_meta_data("min_t",min(private$.data[, .(count = data.table::uniqueN(t)), by = c("agent", "sim")]$count))
      self$set_meta_data("max_t",max(private$.data[, .(count = data.table::uniqueN(t)), by = c("agent", "sim")]$count))

      self$set_meta_data("agents",min(private$.data[, .(count = data.table::uniqueN(agent))]$count))
      self$set_meta_data("simulations",min(private$.data[, .(count = data.table::uniqueN(sim))]$count))

      if ("optimal_reward" %in% colnames(private$.data))
        private$.data[, regret:= optimal_reward - reward]
      else
        private$.data[, optimal_reward:= NA]

      private$.data[, cum_reward:= cumsum(reward), by = list(agent, sim)]
      private$.data[, cum_reward_rate := cum_reward / t]

      private$.data[, cum_regret := cumsum(regret), by = list(agent, sim)]
      private$.data[, cum_regret_rate := cum_regret / t]

      data.table::setkeyv(private$.data,c("t","agent"))

      private$.cum_stats <- private$.data[, list(

        regret_var          = var(regret),
        regret_sd           = sd(regret),
        regret              = mean(regret),

        reward_var          = var(reward),
        reward_sd           = sd(reward),
        reward              = mean(reward),

        cum_regret_var      = var(cum_regret),
        cum_regret_sd       = sd(cum_regret),
        cum_regret          = mean(cum_regret),

        cum_reward_var      = var(cum_reward),
        cum_reward_sd       = sd(cum_reward),
        cum_reward          = mean(cum_reward) ), by = list(t, agent)]


      private$.cum_stats[, cum_reward_rate_var := cum_reward_var / t]
      private$.cum_stats[, cum_reward_rate_sd := cum_reward_sd / t]
      private$.cum_stats[, cum_reward_rate := cum_reward / t]

      private$.cum_stats[, cum_regret_rate_var := cum_regret_var / t]
      private$.cum_stats[, cum_regret_rate_sd := cum_regret_sd / t]
      private$.cum_stats[, cum_regret_rate := cum_regret / t]

      qn       <- qnorm(0.975)
      sqrt_sim <- sqrt(self$get_simulation_count())

      private$.cum_stats[, cum_regret_ci      := cum_regret_sd / sqrt_sim * qn]
      private$.cum_stats[, cum_reward_ci      := cum_reward_sd / sqrt_sim * qn]
      private$.cum_stats[, cum_regret_rate_ci := cum_regret_rate_sd / sqrt_sim * qn]
      private$.cum_stats[, cum_reward_rate_ci := cum_reward_rate_sd / sqrt_sim * qn]
      private$.cum_stats[, regret_ci          := regret_sd / sqrt_sim * qn]
      private$.cum_stats[, reward_ci          := reward_sd / sqrt_sim * qn]

      # move agent column to front
      data.table::setcolorder(private$.cum_stats, c("agent", setdiff(names(private$.cum_stats), "agent")))

    },
    data_table_to_named_nested_list = function(dt, transpose = FALSE) {
      df_m <- as.data.frame(dt)
      rownames(df_m) <- df_m[, 1]
      df_m[, 1] <- NULL
      if (!isTRUE(transpose)) {
        apply((df_m), 1, as.list)
      } else {
        apply(t(df_m), 1, as.list)
      }
    }
  ),
  active = list(
    data = function(value) {
      if (missing(value)) {
        private$.data
      } else {
        warning("## history$data is read only", call. = FALSE)
      }
    },
    cumulative = function(value) {
      if (missing(value)) {
        self$get_cumulative_result()
      } else {
        warning("## history$cumulative is read only", call. = FALSE)
      }
    },
    meta = function(value) {
      if (missing(value)) {
        self$get_meta_data()
      } else {
        warning("## history$meta is read only", call. = FALSE)
      }
    }
  )
)

#' History
#'
#' The R6 class \code{History} keeps a log of all \code{Simulator} interactions
#' in its internal \code{data.table}. It also provides basic data summaries,
#' and can save or load simulation log data files.
#'
#' @name History
#' @aliases print_data reindex delete_empty_rows clear_data_table set_data_table get_data_table set_data_frame get_data_frame load cumulative save
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
#'     \code{logical}. Save context matrix \code{X} when writing simulation data?
#'   }
#'   \item{\code{save_theta}}{
#'     \code{logical}. Save parameter lists \code{theta} when writing simulation data?
#'   }
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
#'   \item{\code{insert(index,
#'                      t,
#'                      action,
#'                      reward,
#'                      agent_name,
#'                      simulation_index,
#'                      context_value = NA,
#'                      theta_value = NA)}}{
#'      Saves one row of simulation data. Is generally not called directly, but from a {Simulator} instance.
#'   }
#'   \item{\code{save(filename = NA)}}{
#'      Writes the \code{History} log file in its default data.table format,
#'      with \code{filename} as the name of the file which the data is to be written to.
#'   }
#'   \item{\code{load = function(filename, interval = 0)}}{
#'      Reads a \code{History} log file in its default \code{data.table} format,
#'      with \code{filename} as the name of the file which the data are to be read from.
#'      If \code{interval} is larger than 0, every \code{interval} of data is read instead of the
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
#'   \item{\code{reindex(truncate = TRUE)}}{
#'      Removes empty rows from the \code{History} log, reindexes the \code{t} column, and,
#'      if \code{truncate} is \code{TRUE}, truncates the resulting data to the shortest simulation
#'      grouped by agent and simulation.
#'   }
#'   \item{\code{print_data()}}{
#'      Prints a summary of the \code{History} log.
#'   }
#'  }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},  \code{\link{OfflinePolicyEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
NULL
