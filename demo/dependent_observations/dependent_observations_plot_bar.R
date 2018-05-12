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
subpolicies     <- list("Partial", "Pooled", "Unpooled" )

data_dir        <- "D:/data/"

history         <- History$new()

regret_df <- data.frame(matrix(ncol = 8, nrow = 0))
cols <- c("horizon", "simulations", "subjects", "betas", "poisson", "policies", "subpolicies", "regret")
colnames(regret_df) <- cols

# Could, of course, also have used R's expand.grid() function:
# combinations        <- expand.grid(subjects,betas,do_poisson,policies,subpolicies )
# names(combinations) <- c("subjects","betas","do_poisson","policies","subpolicies")
# ... but nested loops seemed more comprehensive here.

### Load and manipulate data, plot grids of line charts if do_line_charts TRUE

for (dp in do_poisson) {
  for (beta in betas)    {
    for (sn in subjects)   {
      for (pol in policies)   {
        for (subpol in subpolicies)   {
          history$clear_data_table()
          sim_str <- paste0(subpol,pol,"_b",beta[1],"_s",sn,"_p",as.numeric(dp),"_r",simulations,".RData")
          message(paste0("Plotting: ",sim_str))
          history$load_data(paste0(data_dir,sim_str))
          cum_regret <- history$cumulative(final = TRUE, rate = FALSE, regret = TRUE)
          regret_df[(nrow(regret_df) + 1), ] <- c(horizon, simulations, sn, beta[1], dp, pol, subpol, cum_regret[[1]])
        }
      }
    }
  }
}

### Plot and export regret end value data.frame

# Some plot specific adaptations:
regret_df_plot <- regret_df
regret_df_plot[c(1, 2, 3, 8)] <- lapply(regret_df_plot[c(1, 2, 3, 8)], as.numeric)
regret_df_plot$poisson = ifelse(regret_df_plot$poisson == TRUE, "Poisson", "Uniform")
regret_df_plot$betas = ifelse(regret_df_plot$betas == "1.5", "Beta(1.5,1.5)", "Beta(5,5)")
regret_df_plot[c(1:7)] <- lapply(regret_df_plot[c(1:7)], as.factor)
regret_df_plot$subpolicies <- factor(regret_df_plot$subpolicies, levels = list("Partial","Unpooled", "Pooled"))

p <- ggplot(regret_df_plot, aes(subjects, regret, fill = as.factor(subpolicies))) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  facet_grid(poisson + betas ~ policies) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom") +
  xlab("Users") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) +
  ylab("Cumulative regret")

print(p)

write.table(regret_df_plot,
            file = "end_regret_table.csv",
            sep = ",",
            row.names = FALSE)



