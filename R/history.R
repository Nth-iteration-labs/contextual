#' @importFrom data.table data.table as.data.table set setorder setkeyv copy uniqueN setcolorder tstrsplit
#' @import checkmate
#' @export
History <- R6::R6Class(
  "History",
  portable = FALSE,
  public = list(
    n            = NULL,
    save_theta   = NULL,
    save_context = NULL,
    context_multiple_columns = NULL,
    context_columns_initialized = NULL,
    initialize = function(n = 1, save_context = FALSE, context_multiple_columns = FALSE, save_theta = FALSE) {
      self$n                           <- n
      self$context_multiple_columns    <- context_multiple_columns
      self$save_context                <- save_context
      self$save_theta                  <- save_theta
      self$reset()
    },
    reset = function() {
      gc()
      self$context_columns_initialized <- FALSE
      self$clear_data_table()
      private$initialize_data_tables()
      invisible(self)
    },
    update_statistics = function() {
      private$calculate_cum_stats()
    },
    insert = function(index,
                      t,
                      k,
                      d,
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

      if (save_context || save_theta) {
        if (!isTRUE(self$save_theta)) {
          if(isTRUE(self$context_multiple_columns)) {
            if(!isTRUE(self$context_columns_initialized)) {
              private$initialize_data_tables(length(context_value))
              self$context_columns_initialized <- TRUE
            }
            data.table::set(private$.data, index, (14L:(13L+length(context_value))),
                            as.list(as.vector(context_value)))
          } else {
            data.table::set(private$.data, index, 14L, list(list(context_value)))
          }
        } else if (!isTRUE(self$save_context)) {
          data.table::set(private$.data, index, 14L, list(list(theta_value)))
        } else {
          data.table::set(private$.data, index, 14L, list(list(context_value)))
          data.table::set(private$.data, index, 15L, list(list(theta_value)))
        }
      }

      data.table::set(
        private$.data,
        index,
        1L:13L,
        list(
          t,
          k,
          d,
          simulation_index,
          action[["choice"]],
          reward[["reward"]],
          as.integer(optimal_arm),
          optimal_reward,
          propensity,
          agent_name,
          reward[["regret"]],
          reward[["cum_reward"]],
          reward[["cum_regret"]]
        )
      )
      invisible(self)
    },
    get_agent_list = function() {
      levels(private$.data$agent)
    },
    get_agent_count = function() {
      length(self$get_agent_list())
    },
    get_simulation_count = function() {
      length(levels(as.factor(private$.data$sim)))
    },
    get_arm_choice_percentage = function(limit_agents) {
      private$.data[agent %in% limit_agents][sim != 0][order(choice),
                                                       .(choice = unique(choice),
                                                         percentage = tabulate(choice)/.N)]
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
            cum_results[agent %in% limit_agents]
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
            cum_results[, mget(limit_cols)][agent %in% limit_agents]
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
        dt_to_save <- data.table::copy(private$.data)
        dt_to_save[, context_string := lapply(.SD, function(x) paste( unlist(x), collapse=',') ),
                   by=1:private$.data[, .N], .SDcols = c("context")]
        one_context <- private$.data[1,]$context[[1]]
        if (is.vector(one_context)) one_context <- matrix(one_context, nrow = 1L)
        dt_to_save$k <- ncol(one_context)
        dt_to_save$d <- nrow(one_context)
        dt_to_save[, (context_cols) := data.table::tstrsplit(context_string, ",", fixed=TRUE)]
        dt_to_save$context <- NULL
        if ("theta" %in% names(dt_to_save)) dt_to_save$theta <- NULL
        dt_to_save$context_string <- NULL
        fwrite(dt_to_save[,which(dt_to_save[,colSums(is.na(dt_to_save))<nrow(dt_to_save)]), with = FALSE],
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
      private$.data <- data.table::as.data.table(df)
      if (isTRUE(auto_stats)) private$calculate_cum_stats()
      invisible(self)
    },
    get_data_table = function(limit_agents = NULL, limit_cols = NULL, limit_context = NULL,
                              interval = 1, no_zero_sim = FALSE) {
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
    truncate = function() {
      min_t_sim <- min(private$.data[,max(t), by = c("agent","sim")]$V1)
      private$.data <- private$.data[t<=min_t_sim]
    },
    context_to_columns = function(delete_original_column = FALSE) {
      # create d and k columns
      one_context <- private$.data[1,]$context[[1]]
      if (is.vector(one_context)) one_context <- matrix(one_context, nrow = 1L)
      private$.data$k <- ncol(one_context)
      private$.data$d <- nrow(one_context)
      # create context columns
      context_cols <- c(paste0("X.", seq_along(unlist(private$.data[1,]$context))))
      # create temporary context_string column
      private$.data[, context_string := lapply(.SD, function(x) paste( unlist(x), collapse=',') ),
                    by=1:private$.data[, .N], .SDcols = c("context")]
      # extract context data to context columns
      private$.data[, (context_cols) := data.table::tstrsplit(context_string, ",", fixed=TRUE)]
      # delete temporary context_string column
      private$.data$context_string <- NULL
      # if not temporary, delete original context column
      if (isTRUE(delete_original_column)) private$.data$context <- NULL
    },
    extract_theta = function(limit_agents, parameter, arm, tail = NULL){
      if(!is.null(tail)){
        unlist(sapply(sapply(tail(private$.data[agent %in% limit_agents],tail)$theta,
                             `[`, parameter), `[[`, arm), FALSE, FALSE)
      } else {
        unlist(sapply(sapply(private$.data[agent %in% limit_agents]$theta,
                             `[`, parameter), `[[`, arm), FALSE, FALSE)
      }
    },
    finalize = function() {
      self$clear_data_table()
    }
  ),
  private = list(
    .data            = NULL,
    .meta            = NULL,
    .cum_stats       = NULL,

    initialize_data_tables = function(context_cols = NULL) {
      private$.data <- data.table::data.table(
        t = rep(0L, self$n),
        k = rep(0L, self$n),
        d = rep(0L, self$n),
        sim = rep(0L, self$n),
        choice = rep(0.0, self$n),
        reward = rep(0.0, self$n),
        optimal_arm = rep(0L, self$n),
        optimal_reward = rep(0.0, self$n),
        propensity = rep(0.0, self$n),
        agent = rep("", self$n),
        regret = rep(0.0, self$n),
        cum_reward = rep(0.0, self$n),
        cum_regret = rep(0.0, self$n),
        stringsAsFactors = TRUE
      )
      if (isTRUE(self$save_context)) {
        if (isTRUE(self$context_multiple_columns && !is.null(context_cols))) {
          context_cols <- c(paste0("X.", seq_along(1:context_cols)))
          private$.data[, (context_cols) := 0.0]
        } else {
          private$.data$context <- rep(list(), self$n)
        }
      }
      if (isTRUE(self$save_theta)) private$.data$theta <- rep(list(), self$n)

      # meta data
      private$.meta <- list()

      # cumulative data
      private$.cum_stats <- data.table::data.table()
    },

    calculate_cum_stats = function() {


      self$set_meta_data("min_t",min(private$.data[, .(count = data.table::uniqueN(t)),
                                                   by = c("agent", "sim")]$count))
      self$set_meta_data("max_t",max(private$.data[, .(count = data.table::uniqueN(t)),
                                                   by = c("agent", "sim")]$count))

      self$set_meta_data("agents",min(private$.data[, .(count = data.table::uniqueN(agent))]$count))
      self$set_meta_data("simulations",min(private$.data[, .(count = data.table::uniqueN(sim))]$count))

      if (!"optimal_reward" %in% colnames(private$.data)) #### TODO: why is this? backward compat?
        private$.data[, optimal_reward:= NA]

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

      private$.data[, cum_reward_rate := cum_reward / t]
      private$.data[, cum_regret_rate := cum_regret / t]

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
#' @aliases print_data clear_data_table set_data_table get_data_table
#' set_data_frame get_data_frame load cumulative save
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
#'   \item{\code{save_csv(filename = NA, context_to_columns = FALSE)}}{
#'      Saves History data to csv file. When context_to_columns is TRUE, the "context" column will
#'      be split over multiple columns X1 to X...
#'   }
#'   \item{\code{extract_theta(limit_agents, parameter, arm, tail = NULL)}}{
#'      Extract theta parameter from theta list for \code{limit_agents},
#'      where \code{parameter} sets the to be retrieved parameter or vector of parameters in theta,
#'      \code{arm} is the relevant integer index of the arm or vector of arms of interest, and the
#'      optional \code{tail} selects the last elements in the list.
#'      Returns a vector, matrix or array with the selected theta values.
#'   }
#'   \item{\code{print_data()}}{
#'      Prints a summary of the \code{History} log.
#'   }
#'   \item{\code{data}}{
#'      Active binding, read access to History's internal data.table.
#'   }
#'   \item{\code{cumulative}}{
#'      Active binding, read access to cumulative data by name through $ accessor.
#'   }
#'   \item{\code{meta}}{
#'      Active binding, read access to meta data by name through $ accessor.
#'   }
#'  }
#'
#' @seealso
#'
#' Core contextual classes: \code{\link{Bandit}}, \code{\link{Policy}}, \code{\link{Simulator}},
#' \code{\link{Agent}}, \code{\link{History}}, \code{\link{Plot}}
#'
#' Bandit subclass examples: \code{\link{BasicBernoulliBandit}}, \code{\link{ContextualLogitBandit}},
#' \code{\link{OfflineReplayEvaluatorBandit}}
#'
#' Policy subclass examples: \code{\link{EpsilonGreedyPolicy}}, \code{\link{ContextualThompsonSamplingPolicy}}
#'
#' @examples
#' \dontrun{
#'
#'   policy    <- EpsilonGreedyPolicy$new(epsilon = 0.1)
#'   bandit    <- BasicBernoulliBandit$new(weights = c(0.6, 0.1, 0.1))
#'
#'   agent     <- Agent$new(policy, bandit, name = "E.G.", sparse = 0.5)
#'
#'   history   <- Simulator$new(agents = agent,
#'                              horizon = 10,
#'                              simulations = 10)$run()
#'
#'   summary(history)
#'
#'   plot(history)
#'
#'   dt <- history$get_data_table()
#'
#'   df <- history$get_data_frame()
#'
#'   print(history$cumulative$E.G.$cum_regret_sd)
#'
#'   print(history$cumulative$E.G.$cum_regret)
#'
#' }
#'
NULL
