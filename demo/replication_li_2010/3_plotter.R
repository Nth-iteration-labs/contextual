library(contextual)

# Config -----------------------------------------------------------------------------------------------------

load_file_name          <- "D:/results/Yahoo_T_37450000_sparse_0.00.RData"

# Setup ------------------------------------------------------------------------------------------------------

history  <- History$new()

# Take a look at the results ---------------------------------------------------------------------------------

history$load_data_table(load_file_name)

message("Data imported")

plot(history, regret = FALSE, rate = TRUE, type = "cumulative", legend_position = "bottomright", smooth = TRUE)

message("Plot completed")

history$get_cumulative_result(as_list = FALSE)$cum_reward_rate
