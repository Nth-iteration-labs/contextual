library(contextual)

# Config -----------------------------------------------------------------------------------------------------

load_file_name          <- "Yahoo_T_37450000_sparse_0.99.RData"

# Setup ------------------------------------------------------------------------------------------------------

history  <- History$new()

# Take a look at the results ---------------------------------------------------------------------------------

history$load(load_file_name)

par(mfrow = c(1, 1), mar = c(4, 4, 0.1, 0.1), cex=1.25)
plot(history, regret = FALSE, rate = TRUE, type = "cumulative",
     legend_position = "bottomright",
     legend_border = FALSE,
     no_par = TRUE,
     ylim = c(0.025,0.053),
     interval = 18000)
