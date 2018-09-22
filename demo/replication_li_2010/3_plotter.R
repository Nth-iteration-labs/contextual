library(contextual)
library(lobstr)

# Config -----------------------------------------------------------------------------------------------------

load_file_name          <- "Yahoo_T_2e+06_sparse_0.99.RData"

# Setup ------------------------------------------------------------------------------------------------------

history  <- History$new()

# Take a look at the results ---------------------------------------------------------------------------------

history$load_data_table(load_file_name)

plot(history, regret = FALSE, rate = TRUE, type = "cumulative", legend_position = "bottomright", interval = 1000)
