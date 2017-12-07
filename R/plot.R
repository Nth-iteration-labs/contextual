#' @import R.devices
#' @export
Plot <- R6::R6Class(
  "Plot",
  inherit = Contextual,
  portable = FALSE,
  class = FALSE,
  cloneable = FALSE,
  public = list(
    initialize = function() {

    },
    plot_grid = function(history) {
      dev.hold()
      layout(matrix(c(1, 3, 2, 4), 2, 2, byrow = TRUE))
      par(mar = c(3, 5, 1, 1))#b,l,t,r
      self$cummulative(history, grid = TRUE, legend = TRUE)
      par(mar = c(3, 5, 1, 1))
      self$optimal(history, grid = TRUE, legend = FALSE)
      par(mar = c(3, 5, 1, 2))
      self$average(history, grid = TRUE, legend = FALSE)
      dev.flush()
    },
    cummulative = function(history,
                           grid = FALSE,
                           xlim = NULL ,
                           legend = TRUE) {
      if (grid == FALSE)
        dev.hold()
      cs = history[, list(mean = mean(reward)), by = list(t, agent)]
      agent_list = unique(cs$agent)
      agents = length(agent_list)
      setorder(cs, agent, t)
      agent_list = unique(cs$agent)
      ms = matrix(unlist(cs[, cumsum(mean), by = list(agent)][, 2]),
                  ncol = agents)
      matplot(
        ms,
        type = "l",
        xlim = xlim,
        lwd = 1,
        lty = 1,
        xlab = "Time Step",
        ylab = "Cummulative reward"
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
    },
    is_rstudio = function() {
      .Platform$GUI == "RStudio"
    },
    optimal = function(history,
                       grid = FALSE,
                       xlim = NULL,
                       legend = TRUE) {
      if (grid == FALSE)
        dev.hold()
      cs = history[, list(mean = mean(optimal)), by = list(t, agent)]
      agent_list = unique(cs$agent)
      agents = length(agent_list)
      setorder(cs, agent, t)
      agent_list = unique(cs$agent)
      ms = matrix(unlist(cs[, 3]), ncol = agents)
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
    },
    average = function(history,
                       grid = FALSE,
                       xlim = NULL,
                       legend = TRUE) {
      if (grid == FALSE) dev.hold()
      cs = history[, list(mean = mean(reward)), by = list(t, agent)]
      agent_list = unique(cs$agent)
      agents = length(agent_list)
      setorder(cs, agent, t)
      agent_list = unique(cs$agent)
      ms = matrix(unlist(cs[, 3]), ncol = agents)
      matplot(
        ms,
        type = "l",
        xlim = xlim,
        lwd = 1,
        lty = 1,
        xlab = "Time Step",
        ylab = "Average reward"
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
    },
    set_external = function(ext = TRUE,
                            width = 10,
                            height = 6) {
      if (self$is_rstudio()) {
        if (isTRUE(ext)) {
          sysname = tolower(Sys.info()["sysname"])
          device.name = "x11"
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
