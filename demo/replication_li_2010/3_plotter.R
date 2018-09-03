library(contextual)

# Config -----------------------------------------------------------------------------------------------------

load_file_name          <- "D:/results/Yahoo_T_37450000/Yahoo_T_37450000_sparse_0.RData"

# Setup ------------------------------------------------------------------------------------------------------

history  <- History$new()

# Take a look at the results ---------------------------------------------------------------------------------

history$load(load_file_name)

message("Data imported")

plot(history, regret = FALSE, rate = TRUE, type = "cumulative", legend_position = "bottomright",
     smooth = FALSE, do_par = FALSE, ylim = c(0.018,0.073))
abline(h=350000, col="black") #needs to handdrawn

message("Plot completed")

history$get_cumulative_result(as_list = FALSE)$cum_reward_rate
