# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# this script contains ggplot theme and color specifications 
# used in plotting figures, as well as some useful functions for plotting


# ----------------------------------------------------------------------
# Loading packages
# ----------------------------------------------------------------------

# list of needed packages
packages <- c('ggplot2', 'dplyr', 'reshape2', 
              'ggrepel', 'doParallel', 
              'foreach', 'doRNG', "scales", "here")


ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}


if ("devtools" %in% rownames(installed.packages()) == FALSE) {
    install.packages("devtools")
}
library(devtools)

ipak(packages)

# loading the packages 
lapply(names(packages), library, character.only = TRUE)


# ----------------------------------------------------------------------
# Fonts and ggplot themes
# ----------------------------------------------------------------------

# font setup
fontSetup <- "Helvetica"
fontSize <- 2.3 
pointSize <- 1.5
themeFontSize <- 9

# theme with font sizes adjusted for plotting figures with tikz device
pdftheme <- 
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.x = element_line(lineend = 4, linetype = 1),
        axis.ticks.y = element_line(lineend = 4, linetype = 1),
        axis.ticks = element_line (colour = "black", size = 0.3), 
        axis.text = element_text(size = themeFontSize, colour = "black"),
        axis.text.x = element_text(vjust = 0.5),
        axis.title = element_text(size = themeFontSize + 2),
        axis.title.y = element_text(vjust = 1.8),
        axis.title.x = element_text(vjust = -.8),
        legend.title = element_blank(),
        legend.justification = c(1,0),
        legend.position = c(1,0),
        legend.text = element_text(size = themeFontSize),
        legend.key = element_rect(fill = "#FFFFFF"),
        legend.key.height = unit(0.8,"line"),
        strip.text = element_text(size = themeFontSize + 2),
        strip.background = element_rect(fill = "#FFFFFF"),
        text = element_text(family = fontSetup),
        validate = TRUE
    )


# ----------------------------------------------------------------------
# Color palettes
# ----------------------------------------------------------------------

# Color-blind friendly color combinations

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



# ----------------------------------------------------------------------
# Functions
# ----------------------------------------------------------------------

# small function for recoding levels of a categorical variable,
# requires a named vector, where names are original values and 
# values are values to be recoded to
changeLevels <- function(variable, labels, exp2char = TRUE) {
    variable <- as.character(variable)
    for (i in names(labels)) {
        lab <- if (exp2char) as.character(labels[i]) else labels[i] 
        variable <- ifelse(variable == i, lab, variable)
    }
    return(variable)
}
