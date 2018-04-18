##################### Imports #########################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

##################### Plot saved data #################

data_dir <- "D:/do_data/"

# create new history log object, load and append data
plot_log <- History$new()
#plot_log$load_data(paste0(data_dir,"PartialEG_1.5_5.RData"))
#plot_log$load_data(paste0(data_dir,"PartialEG_1.5_10.RData"))
#plot_log$load_data(paste0(data_dir,"PartialEG_1.5_50.RData"))
#plot_log$load_data(paste0(data_dir,"PartialEG_1.5_100.RData"))
plot_log$load_data(paste0(data_dir,"PartialEG_1.5_500.RData"))

#plot_log$load_data(paste0(data_dir,"UnpooledEG_1.5_5.RData"))
#plot_log$load_data(paste0(data_dir,"UnpooledEG_1.5_10.RData"))
#plot_log$load_data(paste0(data_dir,"UnpooledEG_1.5_50.RData"))
#plot_log$load_data(paste0(data_dir,"UnpooledEG_1.5_100.RData"))
plot_log$load_data(paste0(data_dir,"UnpooledEG_1.5_500.RData"))

#plot_log$load_data(paste0(data_dir,"PooledEG_1.5_5.RData"))
#plot_log$load_data(paste0(data_dir,"PooledEG_1.5_10.RData"))
#plot_log$load_data(paste0(data_dir,"PooledEG_1.5_50.RData"))
#plot_log$load_data(paste0(data_dir,"PooledEG_1.5_100.RData"))
plot_log$load_data(paste0(data_dir,"PooledEG_1.5_500.RData"))

plot(plot_log, type = "cumulative", rate = TRUE, legend = FALSE)
plot(plot_log, type = "cumulative", rate = FALSE, ylim = c(1,1500), legend_labels = c("Partial","Pooled","Unpooled"))
