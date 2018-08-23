library(contextual)


load_file_names         <- list("D:/results/Yahoo_T_4681992/Yahoo_T_4681992_sparse_0.RData",
                                "D:/results/Yahoo_T_4681992/Yahoo_T_4681992_sparse_0.7.RData",
                                "D:/results/Yahoo_T_4681992/Yahoo_T_4681992_sparse_0.8.RData",
                                "D:/results/Yahoo_T_4681992/Yahoo_T_4681992_sparse_0.9.RData",
                                "D:/results/Yahoo_T_4681992/Yahoo_T_4681992_sparse_0.95.RData",
                                "D:/results/Yahoo_T_4681992/Yahoo_T_4681992_sparse_0.99.RData")

history  <- History$new()

ctr_list        <- list()

for(i in seq_along(load_file_names)) {
  history$load(load_file_names[[i]])
  first_day_n     <- floor(history$meta$sim$max_t)
  first_day_data  <- history$get_cumulative_result(as_list = FALSE, t = first_day_n)
  ctr             <- first_day_data$cum_reward_rate
  agents          <- first_day_data$agent
  ctr_relative    <- ctr / ctr[match("Random",agents)]
  ctr_relative    <- ctr_relative[!ctr_relative==1]

  ctr_list[[i]]   <- c(ctr_relative)
}

agents_relative <- agents[!agents=="Random"]

all_ctr <- data.frame("100%" = ctr_list[[1]],   "30%" = ctr_list[[2]],
                      "20%"  = ctr_list[[3]],   "10%" = ctr_list[[4]],
                      "5%"   = ctr_list[[5]],   "1%"  = ctr_list[[6]],  check.names = FALSE)

omniscient <- 1.615
barplot(as.matrix(all_ctr),  xpd = FALSE, beside=TRUE, legend = FALSE,
        ylab="ctr", las=1, xlab="data size", ylim = c(1,1.8))
abline(h=omniscient, col="gray", lwd=1, lty=2)
barplot(as.matrix(all_ctr),  xpd = FALSE,col=gray.colors(6), beside=TRUE,
        legend = agents_relative, args.legend = list(x = 'topright'),
        ylab="ctr", las=1, xlab="data size", ylim = c(1,1.8),add=TRUE)
box(lwd=3)

message("Plot completed")
