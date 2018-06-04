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
      private$initialize_data()
      private$initialize_stats_data()
      private$initialize_meta_data()
      invisible(self)
    },
    save = function(   index,   ### set ipv save?
                       t,
                       action,
                       reward,
                       agent_name,
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
          agent_name
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
    add_agent_data = function(row_name, agent_data_list) {
      private$.meta_agent <- rbind( private$.meta_agent, c(id=row_name,agent_data_list))
    },
    add_meta_data = function(row_name, agent_data) {
      private$.meta <- rbind(private$.meta, list(id=row_name,data=toString(agent_data)))
    },
    data_table_to_named_nested_list = function(dt, transpose = FALSE) {
      df_m <- as.data.frame(dt)
      rownames(df_m) <- df_m[,1]
      df_m[,1] = NULL
      if(transpose == FALSE) {
        apply((df_m),1,as.list)
      } else {
        apply(t(df_m),1,as.list)
      }
    },
    get_meta_agent = function(as_list=TRUE) {
      if(as_list) {
        data_table_to_named_nested_list(private$.meta_agent, transpose = TRUE)
      } else {
        private$.meta_agent
      }
    },
    get_meta_data = function(as_list=TRUE) {
      if(as_list) {
        split(private$.meta$data, private$.meta$id)
      } else {
        private$.meta
      }
    },
    get_agent_list = function() {
      levels(as.factor(private$.data$agent))
    },
    number_of_agents = function() {
      length(get_agent_list())
    },
    number_of_simulations = function() {
      length(levels(as.factor(private$.data$sim)))
    },
    get_cumulative_data = function(limit_agents = NULL) {
        if (is.null(limit_agents)) {
          private$.cum_stats
        } else {
          private$.cum_stats[agent %in% limit_agents]
        }
    },
    get_cumulative_final = function(limit_agents = NULL, as_list = TRUE) {
      if (is.null(limit_agents)) {
        if(as_list) {
          data_table_to_named_nested_list(private$.cum_stats_final, transpose = FALSE)
        } else {
          private$.cum_stats_final
        }
      } else {
        if(as_list) {
          data_table_to_named_nested_list(private$.cum_stats_final[agent %in% limit_agents], transpose = FALSE)
        } else {
          private$.cum_stats_final(private$.cum_stats_final[agent %in% limit_agents])
        }
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
      cat("## Overview of data in History object\n\n")
      str(private$.data, max.level = 1)
      cat("\n")
    },
    calculate_stats = function() {
      calculate_cum_stats()
    },
    initialize_meta_agent = function() {
      mdims <- matrix(ncol = number_of_agents()+1, nrow = 0)
      storage.mode(mdims) <- "character"
      private$.meta_agent <- data.table::data.table(mdims, stringsAsFactors = FALSE )
      colnames(private$.meta_agent) <- c("id",get_agent_list())
    },
    finalize = function() {
      self$clear_data_table()
    }
  ),
  private = list(
    .data = NULL,
    .meta = NULL,
    .meta_agent = NULL,
    .cum_stats = NULL,
    .cum_stats_final = NULL,

    initialize_data = function() {
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
    },
    initialize_meta_data = function() {
      private$.meta <- data.table::data.table()
      mdims <- matrix(ncol = 2, nrow = 0)
      storage.mode(mdims) <- "character"
      private$.meta<- data.table::data.table(mdims, stringsAsFactors = FALSE )
      colnames(private$.meta) <- c("id","data")
    },
    initialize_stats_data = function() {
      private$.cum_stats <- data.table::data.table()
      private$.cum_stats_final <- data.table::data.table(stringsAsFactors = FALSE)
    },
    calculate_cum_stats = function() {

      calculate_regret_reward()

      private$.cum_stats <- data.table::data.table()
      private$.cum_stats <- private$.data[, list(cum_regret_var  = var(cum_regret, na.rm = TRUE),
                                                 cum_regret_sd   = sd(cum_regret, na.rm = TRUE),
                                                 cum_regret = mean(cum_regret, na.rm = TRUE),

                                                 cum_regret_rate_var  = var(cum_regret_rate, na.rm = TRUE),
                                                 cum_regret_rate_sd   = sd(cum_regret_rate, na.rm = TRUE),
                                                 cum_regret_rate = mean(cum_regret_rate, na.rm = TRUE),

                                                 cum_reward_var  = var(cum_reward, na.rm = TRUE),
                                                 cum_reward_sd   = sd(cum_reward, na.rm = TRUE),
                                                 cum_reward = mean(cum_reward, na.rm = TRUE),

                                                 cum_reward_rate_var  = var(cum_reward_rate, na.rm = TRUE),
                                                 cum_reward_rate_sd   = sd(cum_reward_rate, na.rm = TRUE),
                                                 cum_reward_rate = mean(cum_reward_rate, na.rm = TRUE)), by = list(t, agent)]

      private$.cum_stats$cum_regret_ci      <- private$.cum_stats$cum_regret_sd/sqrt(number_of_simulations())*qnorm(0.975)
      private$.cum_stats$cum_reward_ci      <- private$.cum_stats$cum_reward_sd/sqrt(number_of_simulations())*qnorm(0.975)
      private$.cum_stats$cum_regret_rate_ci <- private$.cum_stats$cum_regret_rate_sd/sqrt(number_of_simulations())*qnorm(0.975)
      private$.cum_stats$cum_reward_rate_ci <- private$.cum_stats$cum_reward_rate_sd/sqrt(number_of_simulations())*qnorm(0.975)

      # move agent column to front
      setcolorder(private$.cum_stats, c("agent", setdiff(names(private$.cum_stats), "agent")))

      # final cumulative stats
      private$.cum_stats_final <- data.table::data.table(stringsAsFactors = FALSE)
      for (agent_name in get_agent_list()) {
        private$.cum_stats_final <- rbind(private$.cum_stats_final,
             tail(private$.cum_stats[private$.cum_stats$agent == agent_name], n = 1))
      }

    },
    calculate_regret_reward = function() {

      calculate_regret()
      calculate_cumulative_reward()
      calculate_cumulative_regret()

    },
    calculate_regret = function() {
      private$.data$regret <- private$.data$optimal_reward_value - private$.data$reward
    },
    calculate_cumulative_reward = function(rate = TRUE) {
      private$.data$cum_reward <- private$.data[, cumsum(reward), by = list(agent, sim)]$V1
      if (rate) private$.data$cum_reward_rate <- private$.data$cum_reward/private$.data$t
    },
    calculate_cumulative_regret = function(rate = TRUE) {
      if(!"regret" %in% colnames(private$.data)) self$calculate_regret()
      private$.data$cum_regret <- private$.data[, cumsum(regret), by = list(agent, sim)]$V1
      if (rate) private$.data$cum_regret_rate <- private$.data$cum_regret/private$.data$t
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
        get_cumulative_final()
      } else {
        warning("## history$cumulative is read only", call. = FALSE)
      }
    },
    meta = function(value) {
      if (missing(value)) {
        get_meta_data()
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
#'     \code{logical}. Save context matrix \code{X} when writing simulation data?
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
#'                    agent_name,
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
