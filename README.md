Contextual: Multi-Armed Bandits in R
==========

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://tidyverse.org/lifecycle/#maturing)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Nth-iteration-labs/contextual?branch=master&svg=true)](https://ci.appveyor.com/project/robinvanemden/contextual) [![Build Status](https://travis-ci.org/Nth-iteration-labs/contextual.svg?branch=master)](https://travis-ci.org/Nth-iteration-labs/contextual) [![codecov](https://codecov.io/gh/Nth-iteration-labs/contextual/branch/master/graph/badge.svg)](https://codecov.io/gh/Nth-iteration-labs/contextual) [![develVersion](https://img.shields.io/badge/devel%20version-0.9.0-green.svg?style=flat)](https://github.com/Nth-iteration-labs/contextual) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0) [![DOI](https://zenodo.org/badge/114037654.svg)](https://zenodo.org/badge/latestdoi/114037654) <!-- [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/contextual)](https://cran.r-project.org/package=contextual) -->

Overview
--------

[R6](https://github.com/r-lib/R6) based R package facilitating the simulation and evaluation of context-free and contextual Multi-Armed Bandit policies or algorithms.

Installation
------------

```r
# Install development version from GitHub:
# install.packages("devtools")
devtools::install_github("Nth-iteration-labs/contextual")
```

Documentation
-------------

-   [Online reference](https://nth-iteration-labs.github.io/contextual/reference/index.html)
<!---   [Blog at Pavlov](https://pavlov.tech/category/contextual/)-->


Core class diagram
------------------

![Contextual's core class diagram](https://raw.githubusercontent.com/Nth-iteration-labs/contextual/master/man/figures/cmab_all_large.jpg)

Basic example
-------------

```r
horizon       <- 500L
simulations   <- 500L

bandit        <- ContextualLogitBandit$new(k = 5, d = 10, intercept = TRUE)

agents        <- list(Agent$new(ContextualThompsonSamplingPolicy$new(delta=0.5, R=0.01, epsilon=0.5), bandit),
                      Agent$new(EpsilonGreedyPolicy$new(0.1), bandit),
                      Agent$new(LinUCBGeneralPolicy$new(0.6), bandit),
                      Agent$new(ContextualEpochGreedyPolicy$new(8), bandit))

simulation     <- Simulator$new(agents, horizon, simulations)

history        <- simulation$run()

plot(history, type = "cumulative", rate = TRUE, regret = FALSE, legend_position = "bottomright")
```

Maintainers
-----------

Robin van Emden: author, maintainer
Maurits Kaptein: supervisor

If you encounter a clear bug, please file a minimal reproducible example on [GitHub](https://github.com/Nth-iteration-labs/contextual/issues).
