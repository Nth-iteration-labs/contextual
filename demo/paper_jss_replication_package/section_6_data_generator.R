library(data.table)
library(contextual)
library(here)
setwd(here("demo","paper_jss"))

website_data   <- setDT(read.csv("C:/Users/robin/Desktop/werkmap/dependent_observations/
                                     data/persuasion_detail_pages_only.csv"))

to_daypart <- function(x) {
    c("night","day","night")[findInterval(as.POSIXlt(x,origin="1970-01-01")$hour,c(0,6,18,24));
}

website_data$day_part   <- to_daypart(website_data$stamp)
website_data$day_part   <- as.numeric(factor(website_data$daypart, levels = unique(website_data$daypart)))

setnames(website_data, old = c("sid","uid","scs"), new = c("choice", "user", "reward"))
website_data[, setdiff(names(website_data), c("choice", "user", "reward", "day_part")) := NULL]


website_data[, t := .I]
website_data[, choice := choice + 1]
website_data$user_nr       <- as.numeric(factor(website_data$user, levels = unique(website_data$user)))
website_data[, return_visitor := if(.N == 1) 0 else 1, by=user]
website_data[, multiple_sales := if(.N == 1) 0 else as.numeric(sum(reward == 1)), by=user]

website_data[, daypart := daypart - 1]

write.csv(website_data, file = "persuasion_api_daypart-alt.csv", row.names = FALSE)
