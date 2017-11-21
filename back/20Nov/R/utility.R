library(R.devices)

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

plot_results = function(results, time_step = NA) {
  if (is.na(time_step)) time_step = length(results$reward)


  layout(matrix(c(1,3,2,4), 2, 2, byrow = TRUE))



  dev.hold()

  par(mar = c(1,5,3,1)) #b,l,t,r
  matplot(cumsum(results$reward)[1:time_step] * 100 ,
          type = "l", lwd = 1, lty = 1,
          xlab = NULL, ylab = "Cumulative Reward",
          main = "Live Bandit Plots",
          xlim = c(0,time_step))

  par(mar = c(3,5,2,1)) #b,l,t,r
  barplot( colMeans(colSums(results$arm)) / time_step ,
           #col = rainbow(20),
           ylab = "Proportion arm chosen",
           names.arg=1:length(colMeans(colSums(results$arm))),
           ylim = c(0,1))

  par(mar = c(1,5,3,1)) #b,l,t,r
  matplot(results$optimal[1:time_step] * 100 ,
          xaxt = 'n', type = "l", lwd = 1, lty = 1,
          xlab = NULL, ylab = "Optimal Action",  ylim = c(0,100),
          xlim = c(0,horizon))

  par(mar = c(3,5,1,1)) #b,l,t,r
  matplot(results$reward[1:time_step],
          type = "l", lwd = 1, lty = 1,
          xlab = "Time Step", ylab = "Chosen arm",
          ylim = c(-0.5,  2), xlim = c(0,horizon) )


  results$cummulative <- cumsum(results$reward)
  dev.flush()
}

