library(R.devices)
library(dplyr)

index_of_max <- function(x)
{
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L) sample(y, 1L) else y
}

moving_average <- function(arr, n=15){
  res = arr
  for (i in n:length(arr)) {
    res[i] = mean(arr[(i - n):i])
  }
  res
}

is.rstudio = function(){
  .Platform$GUI == "RStudio"
}

#' @title Change Default Graphing Device from RStudio
#' @description
#' Checks to see if the user is in RStudio. If so, then it changes the device to a popup window. You can set width and height of the device window.
#' @param ext A \code{logical} indicating whether the graph should be done externally or internally in RStudio.
#' @details
#' Depending on the operating system, the default drivers attempted to be used are:
#'
#' OS X and Linux: quartz()
#'
#' Windows: windows()
#'
#' Note, this setting is not permanent. Thus, the behavioral change will last until the end of the session.
#'
#' Also, the active graphing environment will be killed. As a result, any graphs that are open will be deleted. You will have to regraph them.
#' @author Robin van Emden
#' @examples
#' \dontrun{
#' # Turn on external graphs
#' external_graphs()
#'
#' # Turn off external graphs
#' external_graphs(F)
#' }
#
external_graphs = function(ext = TRUE, width = 10, height = 6){
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

    # Kill open graphic devices
    graphics.off()
  }
}

plot_results = function(results, time_step = NA, b = NA) {

  as.data.frame(results)
  dev.hold()
  layout(matrix(c(1,3,2,4), 2, 2, byrow = TRUE))

  ######################

  par(mar = c(3,5,1,1)) #b,l,t,r

  aggr = results %>%
    group_by(t,agent) %>%
    arrange(t,agent) %>%
    summarize(average_reward = mean(reward)) %>%
    group_by(agent)  %>%
    mutate(cummulative_reward = cumsum(average_reward))

  if (is.na(b)) b = length(unique(aggr$agent))

  matrix <- matrix(aggr$cummulative_reward, ncol = b, byrow = T)
  matplot(matrix, type = "l", lwd = 1, lty = 1,
          xlab = "Time Step", ylab = "Cummulative reward")

  legend( "topleft", NULL, unique(aggr$agent), col = c(1,2,3,4,5,6), lwd = 1, lty = 1)
  ####################

  par(mar = c(3,5,1,1)) #b,l,t,r

  aggr = results %>%
    group_by(t,agent) %>%
    arrange(t,agent) %>%
    summarize(optimal_action = mean(optimal)*100)

  matrix <- matrix(aggr$optimal_action, ncol = b, byrow = T)
  matplot(matrix, type = "l", lwd = 1, lty = 1,
          xlab = NULL, ylab = "Optimal arm",  ylim = c(0,100))

  ######################

  par(mar = c(3,5,1,2)) #b,l,t,r
  aggr = results %>%
    group_by(t,agent) %>%
    arrange(t,agent) %>%
    summarize(average_reward = mean(reward))

  matrix <- matrix(aggr$average_reward, ncol = b, byrow = T)
  matplot(matrix, type = "l", lwd = 1, lty = 1,
                  xlab = "Time Step", ylab = "Average reward")


  dev.flush()
}
