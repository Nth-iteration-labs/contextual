library(R6)
library(data.table)
#' @export
Plot <- R6Class(
  "Plot",
  portable = FALSE, class = FALSE, cloneable = FALSE,
  public = list(
    initialize = function() {
    },
    grid = function(history) {
      dev.hold()

      layout(matrix(c(1,3,2,4), 2, 2, byrow = TRUE))

      #b,l,t,r
      par(mar = c(3,5,1,1))
      self$cummulative(history, grid = TRUE, legend = TRUE)

      par(mar = c(3,5,1,1))
      self$optimal(history, grid = TRUE, legend = FALSE)

      par(mar = c(3,5,1,2))
      self$average(history, grid = TRUE, legend = FALSE)

      dev.flush()
    },
    cummulative = function(history, grid = FALSE,  xlim = NULL , legend = TRUE) {
      if (grid == FALSE) dev.hold()

      cs = history[,list(mean = mean(reward)),by = list(t,agent)]
      agent_list = unique(cs$agent)
      agents = length(agent_list)
      setorder(cs, agent, t)
      agent_list = unique(cs$agent)
      ms = matrix(unlist(cs[,cumsum(mean), by = list(agent)][,2]), ncol = agents)
      matplot(ms, type = "l", xlim = xlim, lwd = 1, lty = 1, xlab = "Time Step", ylab = "Cummulative reward")
      if (legend) legend( "topleft", NULL, agent_list, col = 1:agents, lwd = 1, lty = 1, bg = "white")

      if (grid == FALSE) dev.flush()
    },

    optimal = function(history, grid = FALSE, xlim = NULL, legend = TRUE ) {
      if (grid == FALSE) dev.hold()

      cs = history[,list(mean = mean(optimal)),by = list(t,agent)]
      agent_list = unique(cs$agent)
      agents = length(agent_list)
      setorder(cs, agent, t)
      agent_list = unique(cs$agent)
      ms = matrix(unlist(cs[,3]), ncol = agents)
      matplot(ms*100, type = "l", xlim = xlim, lwd = 1, lty = 1, xlab = "Time Step", ylab = "Optimal arm",  ylim = c(0, 100))
      if (legend) legend( "topleft", NULL, agent_list, col = 1:agents, lwd = 1, lty = 1, bg = "white")

      if (grid == FALSE) dev.flush()
    },
    average = function(history, grid = FALSE,  xlim = NULL, legend = TRUE ) {
      if (grid == FALSE) dev.hold()

      cs = history[,list(mean = mean(reward)),by = list(t,agent)]
      agent_list = unique(cs$agent)
      agents = length(agent_list)
      setorder(cs, agent, t)
      agent_list = unique(cs$agent)
      ms = matrix(unlist(cs[,3]), ncol = agents)
      matplot(ms, type = "l", xlim = xlim, lwd = 1, lty = 1, xlab = "Time Step", ylab = "Average reward")
      if (legend) legend( "topleft", NULL, agent_list, col = 1:agents, lwd = 1, lty = 1, bg = "white")

      if (grid == FALSE) dev.flush()
    },
    set_external = function(ext = TRUE, width = 10, height = 6){
      if ( is.rstudio() ) {
        if ( isTRUE(ext) ) {

          sysname = tolower(Sys.info()["sysname"])

          device_name = "x11"
          switch(sysname,
                 darwin = {
                   device_name = "quartz"
                 },
                 windows = {
                   device_name = "windows"
                 }
          )
          options("device" = device_name)
          devOptions(sysname, width = width, height = height )
        } else{
          options("device" = "RStudioGD")
        }
        graphics.off()
      }
    }
  )
)
