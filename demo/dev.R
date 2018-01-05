
## Clean and reset local development environment -------------------------------

# Detach and unload package, we want to use local code base.
if ("package:contextual" %in% search()) {
  detach("package:contextual", unload = TRUE)
}

# Clear environment of all objects.
rm(list = ls())

# Garbage collection, just to be sure.
gc()

# Install package dependencies if needed.
devtools::install_deps()
devtools::load_all()

# Set environment path to GhostScript command, for CRAN check
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.22\\bin\\gswin64c.exe")

## TODO LIST -------------------------------------------------------------------

# TODO: document, write, document, then write some more
# TODO: propensity, and correct names for all vars
# TODO: visualisation(s) from thesis
# TODO: MODEL type input
# make sure enough checks on inputs..

# PROGRESS - through sockets?

# check if not precalc faster on 128 cores.

# TODO: confidence interval
# summary -- these agents, ..
# met continue ding minder duidelijk
# regret ... tool van analyse...

# TODO: fix external plot device on linux etc
# TODO: add tests, full test coverage with testthat
# TODO: theta, cleanup naming
# TODO: output some info how far along, particularly if not chart --> progressbar
# TODO: time calculation, benchmarking per policy, how?
# TODO: double check make sure no object sees/accesses too much
# TODO: UCB1, exp3, exp4, epoch.. ?? Which other policies, particularly cMAB
# TODO: Implement more interesting policies, Maurits and others
# TODO: Ability to add/remove arms
# TODO: Changes over time
# TODO: Adversarial

# TODO: blog about all this, do JS versions









