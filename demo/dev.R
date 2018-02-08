# Clean and reset the local development environment ----------------------------

# Detach and unload package: we want to run the local code base when developing.
if ("package:contextual" %in% search()) detach("package:contextual", unload = T)
# Clear environment of all objects before running, to make sure no interactions.
rm(list = ls())
# Garbage collection to free up memory, cleaning up after rm.
gc()
# Install package dependencies if needed...
devtools::install_deps()
# And load package dependencies, since do not load the package itself.
devtools::load_all()
# Set path to GhostScript command, necessary for local as-cran check.
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.22\\bin\\gswin64c.exe")


# TODO LIST --------------------------------------------------------------------

# check when NA saved to history (ie in the case of oracle/optimal not known etc)
# Comments should explain the why, not the what.

# only data as weight set mechanism, not setweights?

# REFACTOR and DRY plot .. check how usually done in R
# make sure output/summary is nicely formatted and collected
# random when not set, "choose" randomly prgrammaticly.
# TODO: document, write, document, then write some more
# TODO: propensity, and correct names for all vars
# TODO: visualisation(s) from thesis
# TODO: MODEL type input
# make sure enough checks on inputs..
# PROGRESS - through sockets?
# check if without precalc faster on 128 cores.
# TODO: make possible more simple save reward and such (reward = 1 vsreward$reward = ..)
# TODO: seperate context and theta results tables afterall? mem use for nothing.
# TODO: confidence interval
# summary -- these agents, ..
# met continue ding minder duidelijk
# regret ... tool van analyse...
# TODO: fix external plot device on linux etc
# TODO: add tests, full test coverage with testthat
# TODO: theta, cleanup naming
# TODO: output some info how far along, particularly if not chart -> progressbar
# TODO: time calculation, benchmarking per policy, how?
# TODO: double check make sure no object sees/accesses too much
# TODO: UCB1, exp3, exp4, epoch.. ?? Which other policies, particularly cMAB
# TODO: Implement more interesting policies, Maurits and others
# TODO: Ability to add/remove arms
# TODO: Changes over time
# TODO: Adversarial
# TODO: blog about all this, do JS versions


# PERFORMANCE DATA  ------------------------------------------------------------

# on 58  cores:    k3*d3 * 5 policies * 300  * 10000 --> 132 seconds
# on 120 cores:    k3*d3 * 5 policies * 300  * 10000 --> 390 seconds

# on 58  cores:    k3*d3 * 5 policies * 3000 * 10000 --> 930 seconds
# on 120 cores:    k3*d3 * 5 policies * 3000 * 10000 --> 691 seconds

# devtools::use_appveyor()



#data.table::set(data, index, 1L, t)
#data.table::set(data, index, 2L, s)
#data.table::set(data, index, 3L, action$choice)
#data.table::set(data, index, 4L, reward[[1]])
#data.table::set(data, index, 5L, is_optimal)
#data.table::set(data, index, 6L, reward$oracle)
#data.table::set(data, index, 7L, policy_name)

# at end of oracle_generate can do
# if (FALSE) {
# sineChange <- function(x,y,M){
#  M[x,y] + 4 * sin((x - y*6.4)/60)  # Y would be t ..
#}
#vecSineChange <- Vectorize(sineChange,vectorize.args = c('x','y'))
#private$O <- outer(1:nrow(private$O),
#                   1:ncol(private$O),
#                   vecSineChange,
#                   private$O )
#}

# history --> grid
#if (history[agent == "TSampling",.N] > 0) {                               ## this needs to be more generic!!!
#  self$ts(history, grid = TRUE, legend = TRUE)
#} else {
#  self$average(history, grid = TRUE, legend = FALSE, regret = TRUE)
#}

#ggplot(data = cs, aes(
#  x = t,
#  y = mean,
#  ymin = ci_lower,
#  ymax = ci_upper
#)) +
#  geom_line(aes(color = agent)) + geom_ribbon(aes(fill = agent), alpha = 0.3)
##geom_smooth(method = "lm",
## formula = y ~ splines::bs(x, 6),
## aes(fill = agent, color = agent),
## alpha = 0.5, se = FALSE)
#gp

# options(WordPressLogin = c(admin = 'd323mb3r!ap'), WordPressURL = 'https://pavlov.tech/xmlrpc.php')
# knit2wp('posts.Rmd', title = 'Blogging directly from R using RWordPress', publish = FALSE)
