#' @import R.devices
#' @export
Plot <- R6::R6Class(
  "Plot",
  portable = FALSE,
  inherit = Contextual,
  class = FALSE,
  public = list(
    bandit_matrix = NULL,
    initialize = function() {
    },
    grid = function(history) {
      if (!is.data.table(history)) history = history$get_data_table()
      history <- history[t <= history[ , max(t), by = c("sim")][,min(V1)]]
      dev.hold()
      self$bandit_matrix <- layout(matrix(c(1, 3, 2, 4), 2, 2, byrow = TRUE))

      par(mar = c(3, 5, 1, 1))#b,l,t,r
      self$cummulative(history, grid = TRUE, legend = TRUE, regret = TRUE)

      par(mar = c(3, 5, 1, 1))
      self$optimal(history, grid = TRUE, legend = FALSE)

      par(mar = c(3, 5, 1, 2))
      self$cummulative(history, grid = TRUE, legend = FALSE, regret = FALSE)


      par(mar = c(3, 5, 1, 2))
      self$average(history, grid = TRUE, legend = FALSE)

      #if (history[agent == "TSampling",.N] > 0) {                               ## this needs to be more generic!!!
      #  self$ts(history, grid = TRUE, legend = TRUE)
      #} else {
      #  self$average(history, grid = TRUE, legend = FALSE, regret = TRUE)
      #}

      dev.flush()
      invisible(self)
    },
    cummulative = function(history,
                           grid = FALSE,
                           xlim = NULL ,
                           legend = TRUE,
                           regret = FALSE) {
      if (!is.data.table(history)) history = history$get_data_table()
      if (grid == FALSE)
        dev.hold()

      if (regret) {
        cs <- history[, list(mean = mean(oracle - reward)), by = list(t, agent)]
      } else {
        cs <- history[, list(mean = mean(reward)), by = list(t, agent)]
      }

      agent_list <- unique(cs$agent)
      agents <- length(agent_list)
      setorder(cs, agent, t)
      agent_list <- unique(cs$agent)
      ms <- matrix(unlist(cs[, cumsum(mean), by = list(agent)][, 2], FALSE, FALSE),
                  ncol = agents)

      if (regret) {
        ylab_title = "Cummulative regret"
      } else {
        ylab_title = "Cummulative reward"
      }

      matplot(
        ms,
        type = "l",
        xlim = xlim,
        lwd = 1,
        lty = 1,
        xlab = "Time Step",
        ylab = ylab_title
      )
      if (legend)
        legend(
          "topleft",
          NULL,
          agent_list,
          col = 1:agents,
          lwd = 1,
          lty = 1,
          bg = "white"
        )
      if (grid == FALSE)
        dev.flush()
      invisible(self)
    },
    average = function(history,
                       grid = FALSE,
                       xlim = NULL,
                       legend = TRUE,
                       regret = FALSE) {
      if (!is.data.table(history)) history = history$get_data_table()
      if (grid == FALSE) dev.hold()
      if (regret) {
        cs <- history[, list(mean = mean(oracle - reward)), by = list(t, agent)]
      } else {
        cs <- history[, list(mean = mean(reward)), by = list(t, agent)]
      }
      agent_list <- unique(cs$agent)
      agents <- length(agent_list)
      setorder(cs, agent, t)
      agent_list <- unique(cs$agent)
      ms <- matrix(unlist(cs[, 3], FALSE, FALSE), ncol = agents)
      if (regret) {
        ylab_title = "Average regret"
      } else {
        ylab_title = "Average reward"
      }


      matplot(
        ms,
        type = "l",
        xlim = xlim,
        lwd = 1,
        lty = 1,
        xlab = "Time Step",
        ylab = ylab_title
      )
      if (legend)
        legend(
          "topleft",
          NULL,
          agent_list,
          col = 1:agents,
          lwd = 1,
          lty = 1,
          bg = "white"
        )
      if (grid == FALSE) dev.flush()
      invisible(self)
    },
    optimal = function(history,
                       grid = FALSE,
                       xlim = NULL,
                       legend = TRUE) {
      if (!is.data.table(history)) history = history$get_data_table()
      if (grid == FALSE)
        dev.hold()
      cs <- history[, list(mean = mean(is_optimal)), by = list(t, agent)]
      agent_list <- unique(cs$agent)
      agents <- length(agent_list)
      setorder(cs, agent, t)
      agent_list <- unique(cs$agent)
      ms <- matrix(unlist(cs[, 3], FALSE, FALSE), ncol = agents)
      matplot(
        ms * 100,
        type = "l",
        xlim = xlim,
        lwd = 1,
        lty = 1,
        xlab = "Time Step",
        ylab = "Optimal arm",
        ylim = c(0, 100)
      )
      if (legend)
        legend(
          "topleft",
          NULL,
          agent_list,
          col = 1:agents,
          lwd = 1,
          lty = 1,
          bg = "white"
        )
      if (grid == FALSE)
        dev.flush()
      invisible(self)
    },
    is_rstudio = function() {
      .Platform$GUI == "RStudio"
    },
    ts = function(history,
                  grid = FALSE,
                  xlim = NULL,
                  legend = TRUE) {
      if (!is.data.table(history)) history = history$get_data_table()
      if (grid == FALSE)
        dev.hold()

      last_t   = history[,max(t)]
      theta_list = history[agent == "TSampling" & t == last_t & sim == 1,theta]
      mu = lname_to_vector(theta_list[[1]], "mu")
      succes_n = lname_to_vector(theta_list[[1]], "succes")
      chosen_n = lname_to_vector(theta_list[[1]], "chosen")

      alpha = 1; beta = 1
      alpha_per_arm = alpha + succes_n
      beta_per_arm = beta + chosen_n - succes_n
      arm_n = length(alpha_per_arm)

      xv = seq(0, 1, by = 0.01); yv_all = c()
      for (i in 1:arm_n) {
        yv_all = as.vector(rbind(yv_all,dbeta(xv,alpha_per_arm[i],beta_per_arm[i])))
      }
      #ymax = max(yv_all)

      curve(dbeta(x,alpha_per_arm[1],
                  beta_per_arm[1]),
                  col = 4,
                  xlab = "X",
                  ylab = "PDF",
                  ylim = c(0, (10 + 0.6)))
                  #ylim = c(0, (ymax + 0.6)))
      for (i in 2:arm_n) {
        curve(dbeta(x,alpha_per_arm[i],beta_per_arm[i]),add = TRUE,col = (i + 3))
      }
      abline(v = mu, col = c(4:(arm_n + 4)))
      #points(max(mu), 0, type = "p")

      if (legend)
        legend(
          "topleft",
          NULL,
          paste("Arm ", 1:arm_n, sep = ""),
          col = c(4:(arm_n + 4)),
          lwd = 1,
          lty = 1,
          bg = "white"
        )
      if (grid == FALSE)
        dev.flush()
      invisible(self)
    },
    set_external = function(ext = TRUE,
                            width = 10,
                            height = 6) {
      if (self$is_rstudio()) {
        if (isTRUE(ext)) {
          sysname <- tolower(Sys.info()["sysname"])
          device.name <- "x11"
          switch(sysname,
                 darwin = {
                   device.name = "quartz"
                 },
                 windows = {
                   device.name = "windows"
                 })
          options("device" = device.name)
          R.devices::devOptions(sysname, width = width, height = height)
        } else{
          options("device" = "RStudioGD")
        }
        graphics.off()
      }
      invisible(self)
    }
  )
)

#' External Plot
#'
#' Plot intro
#'
#' @section Usage:
#' \preformatted{b <- Plot$new()
#'
#' b$reset()
#'
#' print(b)
#' }
#'
#' @section Arguments:
#' \describe{
#'   \item{b}{A \code{Plot} object.}
#' }
#'
#' @section Details:
#' \code{$new()} starts a new Plot, it uses \code{\link[base]{pipe}}.
#' R does \emph{not} wait for the process to finish, but returns
#' immediately.
#'
#' @importFrom R6 R6Class
#' @name Plot
#' @examples
#'\dontrun{}
#'
NULL
