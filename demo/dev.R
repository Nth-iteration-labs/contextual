# Clean and reset the local development environment ----------------------------

# Clear environment of all objects before running, to make sure no interactions.
rm(list = ls(all = TRUE))

# Garbage collection to free up memory, cleaning up after rm.
gc()

# Detach and unload packages: we want to run the local code base when developing.

if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils",
                      "package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

# Check if testthat and devtools  installed, if not, install
if (!require(devtools)| !require(testthat)) {
  install.packages("devtools")
  devtools::install_github("r-lib/testthat")
}
# Install package dependencies if needed...
devtools::install_deps()

# And load package dependencies, since do not load the package itself.
devtools::load_all()

# Set path to GhostScript command in Windows necessary for local as-cran check.
if (grepl('w|W', .Platform$OS.type)) {
  # Windows
  Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.22\\bin\\gswin64c.exe")
}

#goodpractice
#setwd("~/GitHub")
#gp("contextual")

# PERFORMANCE DATA  ------------------------------------------------------------------------------------------

# on 58  cores:    k3*d3 * 5 policies * 300  * 10000 --> 132 seconds
# on 120 cores:    k3*d3 * 5 policies * 300  * 10000 --> 390 seconds

# on 58  cores:    k3*d3 * 5 policies * 3000 * 10000 --> 930 seconds
# on 120 cores:    k3*d3 * 5 policies * 3000 * 10000 --> 691 seconds

# devtools::use_appveyor()

# data.table::set(data, index, 1L, t)
# data.table::set(data, index, 2L, s)
# data.table::set(data, index, 3L, action$choice)
# data.table::set(data, index, 4L, reward[[1]])
# data.table::set(data, index, 5L, choice_is_optimal)
# data.table::set(data, index, 6L, reward$oracle)
# data.table::set(data, index, 7L, policy_name)

# at end of oracle_generate can do
# if (FALSE) {
# sineChange <- function(x,y,M){
#  M[x,y] + 4 * sin((x - y*6.4)/60)  # Y would be t ..
# }
# vecSineChange <- Vectorize(sineChange,vectorize.args = c('x','y'))
# private$O <- outer(1:nrow(private$O),
#                    1:ncol(private$O),
#                    vecSineChange,
#                    private$O )
# }

# history --> no_par
# if (history[agent == "TSampling",.N] > 0) {       ## this needs to be more generic!!!
#   self$ts(history, no_par = TRUE, legend = TRUE)
# } else {
#   self$average(history, no_par = TRUE, legend = FALSE, regret = TRUE)
# }

# ggplot(data = cs, aes(
#    x = t,
#    y = mean,
#    ymin = ci_lower,
#    ymax = ci_upper
#)) +
#  geom_line(aes(color = agent)) + geom_ribbon(aes(fill = agent), alpha = 0.3)
##geom_smooth(method = "lm",
## formula = y ~ splines::bs(x, 6),
## aes(fill = agent, color = agent),
## alpha = 0.5, se = FALSE)
#gp

# think about rmax if not boolean!!! Rmax.reward!!!?
# To make sure that all arms receive at least one pull, arms
# ' that have not yet been chosen are given an estimate of Rmax.
