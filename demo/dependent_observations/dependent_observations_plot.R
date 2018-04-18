##################### Imports #########################

#library(contextual)
setwd("~/GitHub/contextual/demo")
source("dev.R")

##################### Plot saved data #################

# create new history log object, load and append data
plot_log <- History$new()
#plot_log$load_data("D:/data/PartialEG_1.5_5.RData")
#plot_log$load_data("D:/do_data/PartialEG_1.5_10.RData")
#plot_log$load_data("D:/do_data/PartialEG_1.5_50.RData")
#plot_log$load_data("D:/do_data/PartialEG_1.5_100.RData")
plot_log$load_data("D:/do_data/PartialEG_1.5_500.RData")

#plot_log$load_data("D:/do_data/UnpooledEG_1.5_5.RData")
#plot_log$load_data("D:/do_data/UnpooledEG_1.5_10.RData")
#plot_log$load_data("D:/do_data/UnpooledEG_1.5_50.RData")
#plot_log$load_data("D:/do_data/UnpooledEG_1.5_100.RData")
plot_log$load_data("D:/do_data/UnpooledEG_1.5_500.RData")

#plot_log$load_data("D:/do_data/PooledEG_1.5_5.RData")
#plot_log$load_data("D:/do_data/PooledEG_1.5_10.RData")
#plot_log$load_data("D:/do_data/PooledEG_1.5_50.RData")
#plot_log$load_data("D:/do_data/PooledEG_1.5_100.RData")
plot_log$load_data("D:/do_data/PooledEG_1.5_500.RData")

plot(plot_log, type = "cumulative", rate = TRUE, legend = FALSE)
plot(plot_log, type = "cumulative", rate = FALSE, ylim = c(1,500), legend_labels = c("Partial","Pooled","Unpooled"))
