library(contextual)
library(lobstr)

# Config -----------------------------------------------------------------------------------------------------

load_file_name          <- "D:/results/Yahoo_T_37450000_sparse_0.95.RData"

# Setup ------------------------------------------------------------------------------------------------------

history  <- History$new()

# Take a look at the results ---------------------------------------------------------------------------------

history$load(load_file_name)

message("Data imported")

first_day_n     <- floor(as.integer(history$meta$max_t)/8)
first_day_data  <- history$get_cumulative_result(as_list = FALSE, t = first_day_n)
ctr             <- first_day_data$cum_reward_rate
agents          <- first_day_ctr$agent

barplot(ctr,  xpd = FALSE,col=gray.colors(6),
        #legend = agents, args.legend = list(x = 'topright'),
        ylim = c(0.03,0.055))

message("Plot completed")
