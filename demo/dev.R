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
# More than 80 chars: encapsulate in other function.

# true / false? version nr

# only data as weight set mechanism, not setweights?

# ## precahce on/off makes difference in seed
# REFACTOR and DRY plot .. check how usually done in R
# make sure output/summary is nicely formatted and collected
# random when not set, "choose" randomly prgrammaticly.
# and check what if not precache, seed-wize .. must be same as not precache?
# check all <- and =
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

