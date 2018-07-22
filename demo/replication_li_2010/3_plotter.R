library(contextual)
library(lobstr)

# Config -----------------------------------------------------------------------------------------------------

load_file_name          <- "D:/Yahoo_T30M_Sparse0.RData"
load_file_name          <- "D:/Yahoo_T30M_Sparse0.99.RData"

# Setup ------------------------------------------------------------------------------------------------------

history  <- History$new()

# Take a look at the results ---------------------------------------------------------------------------------

history$load_data(load_file_name, auto_stats = TRUE) # TODO: save whole history object? that works
                                                     # better with interval in load.

                                                     # Then, make it an option to save the data to csv
                                                     # both the results, and the contexts together.

                                                     # also, make stats faster and cleaner!

lobstr::obj_size(history)
lobstr::mem_used()

plot(history, regret = FALSE, rate = TRUE, interval = 10000,
     type = "cumulative", legend_position = "bottomright")
