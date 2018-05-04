##################### Imports ####################

setwd("~/GitHub/contextual/demo/dependent_observations")
# library(contextual)
source("../dev.R")

##################### Settings ###################

horizon        <- 10000
simulations    <- 100
subjects       <- list(50,100,500,1000)
betas          <- list(c(1.5, 1.5),c(5, 5))
do_poisson     <- list(FALSE,TRUE)

policies       <- list("EG","UCB","Thompson")
subpolicies    <- list("Partial", "Pooled", "Unpooled" )

plot_log       <- History$new()

data_dir       <- "D:/data/"

do_legend      <- FALSE

##################### Simulate ###################
# total number of simulations: 9 x 6 x 2 x 2 = 216

ptm            <- proc.time()

for (dp in do_poisson) {
  for (beta in betas)    {
    dev.hold()
    par(mfrow = c(length(subjects),length(policies)))
    par(oma = c(0.1,0.1,2,0.1))      # outer margins
    par(mar = c(0, 0, 0, 0))         # inner margins
    par(xaxt = "n")                  # no x axis label
    par(yaxt = "n")                  # no y axis label
    i <- 1
    j <- 1
    for (sn in subjects)   {
      for (pol in policies)   {

        plot_log$clear_data_table()
        for (subpol in subpolicies)   {
          sim_str <- paste0(subpol,pol,"_b",beta[1],"_s",sn,"_p",as.numeric(dp),
                                "_r",simulations,".RData")
          message(paste0("Plotting: ",sim_str))
          plot_log$load_data(paste0(data_dir,sim_str))
        }
        if (i == 1) { do_legend <- TRUE } else {do_legend <- FALSE }
        plot(plot_log,
             grid = TRUE,
             type = "cumulative",
             rate = FALSE,
             legend = do_legend,
             legend_labels = subpolicies,
             legend_border = FALSE,
             ylim = c(1,1650))
        title(outer = FALSE, main = pol, adj = 0.96, line = -1.3, font.main = 1, cex.main = 1.0, col.main = "black")
        if (i > 1) title(outer = FALSE, main = sn, adj = 0.04, line = -1.3, font.main = 1, cex.main = 1.0, col.main = "black" )
        i <- i + 1
      }

    }
    title(paste0("Betas: ", beta[1], " Poisson: ", dp), outer = TRUE)
    dev.flush()
  }
}


print(proc.time() - ptm)

#local_plot <- plot(plot_log, type = "cumulative", rate = TRUE, legend = FALSE)
