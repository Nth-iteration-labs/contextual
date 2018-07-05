# ----------------------------------------------------------------------
# Description
# ----------------------------------------------------------------------

# Script for Chapelle Li 2011 article, produces figure 2 from the article.


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

# outDir
outDir <- "../article/"


# ----------------------------------------------------------------------
# Reshaping the data
# ----------------------------------------------------------------------


# combining the datasets
load(file = "../data/banditBasic.RData")
resultsBasic <- filter(resultsAvg, algo == "Thompson")
load(file = "../data/banditOptimistic.RData")
resultsAvg <- rbind(resultsAvg, resultsBasic)

# add labels to the data
noTrials <- max(resultsAvg$trial)
resultsSub <- resultsAvg %>%
    mutate(
        lineLabel = ifelse(trial == 10^6, as.character(algo), NA))


# ----------------------------------------------------------------------
# Plotting
# ----------------------------------------------------------------------

# plotting function
genPlot <- function(plotData, yMax, yStep) {
    ggplot(data = plotData, aes(x = trial, y = reg_mean)) + 
    geom_line(aes(group = algo, color = algo, linetype = algo)) + 
    facet_wrap(epsilon ~ noArms, 
        labeller = label_bquote(
            cols = K ~ "=" ~ .(noArms) ~ "," ~ epsilon ~ "=" ~ .(epsilon))
        ) +
    geom_label_repel( 
        aes(x = trial, y = reg_mean, label = lineLabel),
        colour = "black", segment.alpha = 1,
        segment.colour = "black", segment.size = 0.5, force = 10,
        min.segment.length = unit(0.3, "lines"),
        size = fontSize,
        inherit.aes = FALSE) + 
    scale_x_continuous("Time (in log units)",
        limits = c(10^2, 10^7),
        trans = 'log10',
        breaks = 10^c(2,3,4,5,6,7),
        labels = trans_format('log10', math_format(10^.x))) +
    scale_y_continuous("Cumulative regret", 
        limits = c(0,yMax),
        breaks = seq(0, yMax, yStep)) +
    scale_color_manual("", values = cbbPalette[c(2,6)]) +
    scale_linetype_manual("", values = c(1, 2)) +
    pdftheme + 
    theme(legend.position = "none") 
}

# generating plots for each experiment condition
epsilon01_K10 <- genPlot(
    plotData = filter(resultsSub, epsilon == "0.1", noArms == "10"),
    yMax = 500, yStep = 100
    )

epsilon002_K10 <- genPlot(
    plotData = filter(resultsSub, epsilon == "0.02", noArms == "10"),
    yMax = 1800, yStep = 200
    )

epsilon01_K100 <- genPlot(
    plotData = filter(resultsSub, epsilon == "0.1", noArms == "100"),
    yMax = 5000, yStep = 1000
    )

epsilon002_K100 <- genPlot(
    plotData = filter(resultsSub, epsilon == "0.02", noArms == "100"),
    yMax = 20000, yStep = 4000
    )


# ----------------------------------------------------------------------
# Saving figures
# ----------------------------------------------------------------------

filename <- paste0(outDir, "figure2_epsilon002_K10.pdf")
cairo_pdf(filename, height = 3, width = 3.5, onefile = TRUE)
print(epsilon002_K10)
dev.off()
