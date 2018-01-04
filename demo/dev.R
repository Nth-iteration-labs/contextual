rm(list = ls())
gc()
if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)
devtools::load_all()

# Set environment path to GhostScript command, for check
Sys.setenv(R_GSCMD = "C:\\Program Files\\gs\\gs9.22\\bin\\gswin64c.exe")

########### Start TODO LIST ######################

# TODO: document, write, document, then write some more
# TODO: confidence interval
# summary -- these agents, ..
# met continue ding minder duidelijk
# regret ... tool van analyse...

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

########### End TODO LIST #######################
