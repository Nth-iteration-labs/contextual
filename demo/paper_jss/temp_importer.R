library(data.table)
library(contextual)
library(here)
setwd(here("demo","paper_jss"))

website_data   <- setDT(read.csv("C:/Users/robin/Desktop/werkmap/dependent_observations/data/persuasion_inc_time.csv"))

to_daypart <- function(x) {
    #c("night","day","night")[findInterval(as.POSIXlt(x,origin="1970-01-01")$hour,c(0,6,18,24))]
    c('night','morning','before_afternoon','afternoon','twilight','evening')[findInterval(as.POSIXlt(x,origin="1970-01-01")$hour,c(0,6,9,15,18,20))]
}
setnames(website_data, old = c("sid","scs"), new = c("choice", "reward"))

website_data[, t := .I]
website_data[, choice := choice + 1]

website_data$daypart   <- to_daypart(website_data$stamp)
website_data$daypart   <- as.numeric(factor(website_data$daypart, levels = unique(website_data$daypart)))

website_data[, daypart := daypart - 1]

write.csv(website_data, file = "persuasion_api_daypart.csv", row.names = FALSE)
