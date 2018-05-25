library(RCurl)
library(foreign)

url <- "https://raw.githubusercontent.com/Nth-iteration-labs/"
url <- paste0(url,"contextual_data/master/PersuasionAPI/persuasion.csv")

persuasion_data     <- getURL(url)
persuasion_data     <- read.csv(textConnection(persuasion_data))
