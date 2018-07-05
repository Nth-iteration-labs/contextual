# ----------------------------------------------------------------------
# Description
# ----------------------------------------------------------------------

# Script for Chapelle Li 2011 article, produces figure 3 from the article.


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
load(file = "../data/banditPosteriorReshaping.RData")

# outDir
outDir <- "../article/"


# ----------------------------------------------------------------------
# Preparing the data
# ----------------------------------------------------------------------

noTrials <- max(resultsAvg$trial)

# algo labels 
algoLabels <- c(
    "Asymptotic Lower Bound" = "ALB",
    "Thompson_2" = "2",
    "Thompson_1" = "1",
    "Thompson_05" = "0.5",
    "Thompson_025" = "0.25"
)
 
# add labels to the data
noTrials <- max(resultsAvg$trial)
resultsSub <- resultsAvg %>%   
    mutate(
        algo = changeLevels(algo, algoLabels),
        lineLabel = ifelse(trial == 2*10^6, as.character(algo), NA))
resultsSub$algo <- factor(resultsSub$algo, levels = algoLabels)

# add the offset to ALB results so that it passes through 0 at 10^2
offsets <- resultsSub %>% 
    filter(trial == 10^2, algo == "ALB")
for (i in 1:nrow(offsets)) {
    idx <- resultsSub$algo == "ALB" & resultsSub$noArms == offsets$noArms[i]  & resultsSub$epsilon == offsets$epsilon[i]
    resultsSub$reg_mean[idx] <- resultsSub$reg_mean[idx] - offsets$reg_mean[i]
}

# box plot results
resultsBox <- resultsFin %>%  
    filter(algo != "Asymptotic Lower Bound") %>%  
    mutate(algo = sapply(strsplit(algo, split = "_"), `[`, 2)) %>%
    group_by(iter) %>%
    mutate(
        cumreg = ifelse(cumreg > 7000, runif(1, 6000, 7000), cumreg)) 

# ----------------------------------------------------------------------
# Plotting
# ----------------------------------------------------------------------

# left panel, with respect to time
epsilon002_K10_PosteriorReshaping_time <- 
    ggplot(data = resultsSub, aes(x = trial, y = reg_mean)) + 
    geom_line(aes(group = algo, color = algo, linetype = algo)) + 
    geom_label_repel( 
        aes(x = trial, y = reg_mean, label = lineLabel),
        colour = "black", segment.alpha = 1,
        segment.colour = "black", segment.size = 0.5, force = 3,
        min.segment.length = unit(0.1, "lines"),
        size = fontSize,
        inherit.aes = FALSE) + 
    scale_x_continuous("Time (in log units)",
        limits = c(10^2, 10^7),
        trans = 'log10',
        breaks = 10^c(2,3,4,5,6,7),
        labels = trans_format('log10', math_format(10^.x))) +
    scale_y_continuous("Cumulative regret", 
        limits = c(0, 4500),
        breaks = seq(0, 4500, 500)) +
    scale_color_manual("", values = cbbPalette[c(1,6,6,6,6)]) +
    scale_linetype_manual("", values = c(1,2,1,4,3)) +
    pdftheme + 
    theme(legend.position = "none") 

# right panel, boxplot of regrets at the end of the simulation
epsilon002_K10_PosteriorReshaping_boxplot <- 
    ggplot(data = resultsBox, aes(x = algo, y = cumreg)) + 
    geom_boxplot(outlier.alpha = 0.6, outlier.size = 0.4, size = 0.3,
        outlier.colour = cbbPalette[7], outlier.shape = 3) +
    geom_hline(yintercept = 6000, linetype = 2) +
    scale_x_discrete(expression(alpha),
        labels = c("2"=2, "1"=1, "05"=0.5, "025"=0.25),
        limits = c("2", "1", "05", "025")) +
    scale_y_continuous("Cumulative regret", 
        limits = c(0, 7000),
        breaks = seq(0, 7000, 1000)) +
    pdftheme + 
    theme(legend.position = "none") 


# ----------------------------------------------------------------------
# Saving figures
# ----------------------------------------------------------------------

filename <- paste0(outDir, "figure3_PosteriorReshaping_time.pdf")
cairo_pdf(filename, height = 3, width = 3.5, onefile = TRUE)
print(epsilon002_K10_PosteriorReshaping_time)
dev.off()

filename <- paste0(outDir, "figure3_PosteriorReshaping_boxplot.pdf")
cairo_pdf(filename, height = 3, width = 3.5, onefile = TRUE)
print(epsilon002_K10_PosteriorReshaping_boxplot)
dev.off()
