library(data.table)
library(contextual)
library(ggplot2)
library(here)
setwd(here("demo","replication_kruijswijk_2018"))

### Settings

horizon         <- 10000
simulations     <- 100

subjects        <- list(50,100,500,1000)
betas           <- list(c(1.5, 1.5),c(5, 5),c(2.5, 1.5))
do_poisson      <- list(FALSE) #, TRUE)
policies        <- list("EG","UCB","Thompson")
subpolicies     <- list("PartialBB","Partial", "Pooled", "Unpooled" )

data_dir        <- "D:/_plotme_/"

history         <- History$new()

regret_df <- data.frame(matrix(ncol = 9, nrow = 0))
cols <- c("horizon", "simulations", "subjects", "betas", "poisson", "policies", "subpolicies", "regret", "ci")
colnames(regret_df) <- cols

### Load and manipulate data, plot grids of line charts if do_line_charts TRUE

for (dp in do_poisson) {
  for (beta in betas)    {
    for (sn in subjects)   {
      for (pol in policies)   {
        for (subpol in subpolicies)   {
          history$clear_data_table()
          sim_str <- paste0(subpol,pol,"_b",beta[1],"_s",sn,"_p",as.numeric(dp),"_r",simulations,".RData")
          message(paste0("Plotting: ",sim_str))
          if(subpol=="PartialBB" && pol == "Thompson") {
            cum_regret <- list()
            cum_regret[[1]] <- list()
            cum_regret[[1]]$cum_regret <- 0
            cum_regret[[1]]$cum_regret_ci <- 0
          } else {
            history$load(paste0(data_dir,sim_str))
            cum_regret <- history$get_cumulative_result(t=10000)
          }
          regret_df[(nrow(regret_df) + 1), ] <- c(horizon, simulations, sn, beta[1], dp, pol, subpol,
                                                  cum_regret[[1]]$cum_regret, cum_regret[[1]]$cum_regret_ci)
        }
      }
    }
  }
}

### Plot and export regret end value data.frame

regret_df_plot <- regret_df

regret_df_plot[c(1, 2, 3, 8, 9)] <- lapply(regret_df_plot[c(1, 2, 3, 8, 9)], as.numeric)
regret_df_plot$poisson = ifelse(isTRUE(regret_df_plot$poisson), "Poisson", "Uniform")

regret_df_plot$betas[regret_df_plot$betas == "1.5"] = "Beta(1.5,1.5)"
regret_df_plot$betas[regret_df_plot$betas == "2.5"] = "Beta(2.5,1.5)/Beta(1.5,2.5)"
regret_df_plot$betas[regret_df_plot$betas == "5"] = "Beta(5,5)"

regret_df_plot[c(1:7)] <- lapply(regret_df_plot[c(1:7)], as.factor)
regret_df_plot$subpolicies <- factor(regret_df_plot$subpolicies,
                                     levels=c("Pooled","Unpooled","PartialBB","Partial" ))
levels(regret_df_plot$subpolicies) <-       c("Complete","No","PartialBB","Partial")

regret_df_plot$policies_r = factor(regret_df_plot$policies, levels=c("EG","UCB","Thompson"))
regret_df_plot$betas_r = factor(regret_df_plot$betas, levels=c("Beta(1.5,1.5)",
                                                               "Beta(5,5)",
                                                               "Beta(2.5,1.5)/Beta(1.5,2.5)"))

p <- ggplot(regret_df_plot, aes(subjects, regret, fill = subpolicies)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal(base_size = 16) +
  geom_errorbar(aes(ymin=(regret-ci), ymax=(regret+ci)), width=.4, size =0.3,
                position=position_dodge(.9)) +
  facet_grid(betas_r ~ policies_r) +
  # when poisson results comment above and uncomment below:
  # facet_grid(poisson + betas_r ~ policies_r) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom", legend.spacing.x = unit(0.25, 'cm')) +

  xlab("Users") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.5)) +
  ylab("Cumulative regret")

print(p)

write.table(regret_df_plot,
            file = "end_regret_table.csv",
            sep = ",",
            row.names = FALSE)

