rm(list = ls())
gc()
if ("package:contextual" %in% search()) detach("package:contextual", unload = TRUE)
devtools::load_all()

########### Start TODO LIST ######################

# TODO: Get feedback and potentially change architecture and class/function names
# TODO: document, write, document, then write some more

# TODO: add tests, full test coverage with testthat
# TODO: theta, cleanup: whats theta, whats "just logging"
# TODO: output some info how far along, particularly if not chart --> progressbar
# TODO: add option to do in batches
# TODO: time calculation, benchmarking per policy, how?
# TODO: make sure no object sees/accesses too much!
# TODO: UCB1, exp3, exp4, epoch.. ?? Which other policies
# TODO: create context from preexisting data
# TODO: online and offline generators
# TODO: Implement more interesting policies, from Maurits and others
# TODO: Ability to add/remove arms
# TODO: Generate changes over time

# TODO: blog about all this, do JS versions

########### End TODO LIST #######################


########### Start COMMENT LIST ##################

## MAURITS KAPTEIN, 12-12-2017 ##################
## Need to be able to make use of a custom data generating function ..
## Seems to imply an AbstractBandit class, user gen Bandit classes inherit.
## Which is what Robin already was planning to do, together with an OfflineBandit
## or somesuch class that uses preexisting data to generate..
## But.. maybe its better to change this altogether, context from a context class?

## Badge: install.packages("badger"); library("badger"); badge_devel("robinvanemden/contextual", "blue")

########### End COMMENT LIST ####################
