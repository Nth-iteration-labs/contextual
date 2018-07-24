library(contextual)
library(lobstr)

# Config -----------------------------------------------------------------------------------------------------

load_file_name          <- "D:/Yahoo_T30M_Sparse0.RData"
load_file_name          <- "D:/Yahoo_T30M_Sparse0.99.RData"

# Setup ------------------------------------------------------------------------------------------------------

history  <- History$new()

# Take a look at the results ---------------------------------------------------------------------------------

history$load_data(load_file_name)

plot(history, regret = FALSE, rate = TRUE, interval = 10000,
     type = "cumulative", legend_position = "bottomright")
