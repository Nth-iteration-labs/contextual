### Imports

setwd("~/GitHub/contextual/demo/dependent_observations")
# library(contextual)
library(ggplot2)
source("../dev.R")


### Settings

horizon         <- 10000
simulations     <- 100
subjects        <- list(50,100,500,1000)
betas           <- list(c(1.5, 1.5),c(5, 5))
do_poisson      <- list(FALSE, TRUE)

policies        <- list("EG","UCB","Thompson")
subpolicies     <- list("Partial", "Unpooled", "Pooled" )

do_line_charts  <- FALSE

data_dir        <- "D:/data/"

history         <- History$new()

regret_df <- data.frame(matrix(ncol = 8, nrow = 0))
cols <- c("horizon", "simulations", "subjects", "betas", "poisson", "policies", "subpolicies", "regret")
colnames(regret_df) <- cols


### Load and manipulate data

ptm            <- proc.time()

for (dp in do_poisson) {
  for (beta in betas)    {
    dev.hold()
    par(mfrow = c(length(subjects),length(policies)))
    par(oma = c(0.1,0.1,2,0.1))
    par(mar = c(0, 0, 0, 0))
    par(xaxt = "n")
    par(yaxt = "n")
    i <- 1
    j <- 1
    for (sn in subjects)   {
      for (pol in policies)   {

        history$clear_data_table()
        for (subpol in subpolicies)   {
          sim_str <- paste0(subpol,pol,"_b",beta[1],"_s",sn,"_p",as.numeric(dp),
                                "_r",simulations,".RData")
          message(paste0("Plotting: ",sim_str))
          history$load_data(paste0(data_dir,sim_str))

        }
        cum_regret <- history$cumulative(final = TRUE, rate = FALSE, regret = TRUE)
        for (i in seq_along(subpolicies))   {
          regret_df[(nrow(regret_df) + 1), ] <- c(horizon, simulations, sn, beta[1], dp, pol, subpolicies[[i]], cum_regret[[i]])
        }
        if (do_line_charts) {
          if (i == 1) { do_legend <- TRUE } else {do_legend <- FALSE }
          plot(history,
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
    }
    title(paste0("Betas: ", beta[1], " Poisson: ", dp), outer = TRUE)
    dev.flush()
  }
}


### Plot and export summary

regret_df[c(1, 2, 3, 4, 8)] <- lapply(regret_df[c(1, 2, 3, 4, 8)], as.numeric)
regret_df$poisson = ifelse(regret_df$poisson == TRUE, "Poisson", "Uniform")
regret_df[c(1:7)] <- lapply(regret_df[c(1:7)], as.factor)

ggplot(regret_df, aes(subjects, regret, fill = as.factor(subpolicies))) +
  geom_bar(position = "dodge", stat = "identity") +
  facet_grid(poisson + betas ~ policies)

write.table(regret_df,
            file = "end_regret_table.csv",
            sep = ",",
            row.names = FALSE)


