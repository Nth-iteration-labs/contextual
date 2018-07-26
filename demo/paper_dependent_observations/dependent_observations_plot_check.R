library(contextual)
library(here)
setwd(here("demo","paper_dependent_observations"))

source("../dev.R")

data_dir        <- "D:/data/"
history         <- History$new()
history$load_data_table(paste0(data_dir,"UnpooledEG_b1.5_s50_p0_r100.RData"))
cum_regret <- history$cumulative(final = TRUE, rate = FALSE, regret = TRUE)
print(cum_regret)

history$clear_data_table()
history$load_data_table(paste0(data_dir,"PooledEG_b1.5_s50_p0_r100.RData"))
cum_regret <- history$cumulative(final = TRUE, rate = FALSE, regret = TRUE)
print(cum_regret)

history$clear_data_table()
history$load_data_table(paste0(data_dir,"PartialEG_b1.5_s50_p0_r100.RData"))
cum_regret <- history$cumulative(final = TRUE, rate = FALSE, regret = TRUE)
print(cum_regret)
