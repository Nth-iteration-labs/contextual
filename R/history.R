#' @import data.table
#' @export
History <- R6::R6Class(
  "History",
  portable = FALSE,
  class = TRUE,
  public = list(
    n = 1000,
    save_theta = NULL,
    save_context = NULL,
    initialize = function(n = 1, save_context = FALSE, save_theta = FALSE) {
      self$n <- n
      self$save_context <- save_context
      self$save_theta   <- save_theta
      self$reset()
    },
    reset = function() {
      self$clear_data_table()
      private$.data <- data.table::data.table(
        t               = rep(0L,      n),
        sim             = rep(0L,      n),
        choice          = rep(0.0,     n),
        reward          = rep(0.0,     n),
        is_optimal      = rep(0L,      n),
        opimal          = rep(0.0,     n),
        propensity      = rep(0.0,     n),
        agent           = rep("",      n)
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
                       s,
                       context_value = NA,
                       theta_value = NA
    ) {

      index <- as.integer(index)

      if (!is.null(action$propensity)) propensity <- action$propensity
      else propensity <- NA

      if (!is.null(reward$opimal)) opimal <- reward$opimal
      else  opimal <- NA

      data.table::set(
        data,
        index,
        1L:8L,
        list(
          t,
          s,
          action$choice,
          reward$reward,
          as.integer(reward$reward == opimal),
          opimal,
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
        private$.data$temp <- private$.data$opimal - private$.data$reward
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
      # check on if table info not NULL too?
      if (nrow(private$.data) > 1) {
        temp_data     <- readRDS(filename)
        if (nth_rows > 0) temp_data <- temp_data[t %% nth_rows == 0]
        private$.data <- rbind(private$.data, temp_data)
        temp_data     <- NULL
      } else {
        private$.data <- readRDS(filename)
        if (nth_rows > 0) private$.data <- private$.data[t %% nth_rows == 0]
      }
      invisible(self)
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

#' External History
#'
#' History intro
#'
#' @section Usage:
#' \preformatted{b <- History$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{History} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new History, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name History
#' @examples
#'\dontrun{}
#'
NULL
