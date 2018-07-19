# ----------------------------------------------------------------------
# Description
# ----------------------------------------------------------------------

# Script for Chapelle Li 2011 article, produces table 1 from the article.


# ----------------------------------------------------------------------
# Loading the data
# ----------------------------------------------------------------------

# set the directory if using the script interactively, 
# below you see example of my path to the folder containintg the script
# setwd("/home/hstojic/Research/replications/gap_Chapelle_Li_2011/code")

# house keeping
rm(list = ls())

# load libraries and auxiliary functions
source("utils.R")

# load the dataset
load(file = "../data/banditDelay.RData")

# outDir
outDir <- "../article/"
saveFileName <- paste0(outDir, "table1")


# ----------------------------------------------------------------------
# Preparing the data
# ----------------------------------------------------------------------

# add labels to the data
noTrials <- max(resultsAvg$trial)
filtResults <- resultsAvg %>%   
    filter(trial %in% c(noTrials)) %>%   
    mutate(
    	batch = sapply(strsplit(algo, "_"), "[", 2),
    	algo = sapply(strsplit(algo, "_"), "[", 1)
    )

# extract the relevant numbers
ucb <- filter(filtResults, algo == "UCB")$reg_mean
thompson <- filter(filtResults, algo == "Thompson")$reg_mean
ratio <- ucb/thompson

# create a table
table1 <- round(rbind(ucb, thompson, ratio), 2)
colnames(table1) <- filter(filtResults, algo == "Thompson")$batch
rownames(table1) <- c("UCB", "TS", "Ratio")


# ----------------------------------------------------------------------
# Saving the results
# ----------------------------------------------------------------------

sink(saveFileName)
print(table1)
xtable::xtable(table1)
sink()
