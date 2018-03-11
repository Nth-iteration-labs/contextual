Contextual: Multi-Armed Bandits in R
==========

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://tidyverse.org/lifecycle/#experimental)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Nth-iteration-labs/contextual?branch=master&svg=true)](https://ci.appveyor.com/project/Nth-iteration-labs/contextual) [![Build Status](https://travis-ci.org/Nth-iteration-labs/contextual.svg?branch=master)](https://travis-ci.org/Nth-iteration-labs/contextual) [![codecov](https://codecov.io/gh/Nth-iteration-labs/contextual/branch/master/graph/badge.svg)](https://codecov.io/gh/Nth-iteration-labs/contextual) [![develVersion](https://img.shields.io/badge/devel%20version-0.0.0.9012-blue.svg?style=flat)](https://github.com/Nth-iteration-labs/contextual) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0) [![DOI](https://zenodo.org/badge/114037654.svg)](https://zenodo.org/badge/latestdoi/114037654) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/contextual)](https://cran.r-project.org/package=contextual)

Overview
--------

Contextual is an R package to facilitate the simulation and analysis of Contextual Multi-Armed Bandit (CMAB) policies and algorithms.

Documentation
-------------

-   [Online reference](https://nth-iteration-labs.github.io/contextual/reference/index.html)
-   [Blog at Pavlov](https://pavlov.tech/category/contextual/)

Installation
------------

``` r
# Install development version from GitHub:
# install.packages("devtools")
devtools::install_github("Nth-iteration-labs/contextual")
```

If you encounter a clear bug, please file a minimal reproducible example on [GitHub](https://github.com/Nth-iteration-labs/contextual/issues).

Usage
-----

Running a simulation to compare basic Multi-Armed Bandit policies:

``` r
library(contextual)

horizon            <- 100L
simulations        <- 1000L

weight_per_arm     <- c(0.9, 0.1, 0.1)

bandit             <- SyntheticBandit$new(data = weight_per_arm)

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(RandomPolicy$new("Random"), bandit),
                            Agent$new(OraclePolicy$new("Oracle"), bandit),
                            Agent$new(ThompsonSamplingPolicy$new(1.0, 1.0, "TS"), bandit),
                            Agent$new(Exp3Policy$new(0.1, "Exp3"), bandit),
                            Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations)

history            <- simulation$run()

plot(history, type = "grid")
```

<img src="https://raw.githubusercontent.com/Nth-iteration-labs/contextual/master/tools/readme/basic_multi_plot.png" width="100%" />

Running a simulation to compare a contextual (LinUCB) and some non-contextual Multi-Armed Bandit policies:

``` r
library(contextual)

horizon            <- 100L
simulations        <- 5000L
                                  #k1  #k2  #k3   -> k armed bandit
weights            <- matrix(  c( 0.9, 0.3, 0.2,                        #d1
                                  0.5, 0.6, 0.2,                        #d2
                                  0.2, 0.1, 0.5),  nrow = 3, ncol = 3)  #d3  -> d features in context

bandit             <- SyntheticBandit$new(data = weights )

agents             <- list( Agent$new(EpsilonGreedyPolicy$new(0.1, "\U190-greedy"), bandit),
                            Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit),
                            Agent$new(OraclePolicy$new("Oracle"), bandit),
                            Agent$new(RandomPolicy$new("Random"), bandit),
                            Agent$new(ThompsonSamplingPolicy(1, 1, "TSampling"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations)

history            <- simulation$run()

plot(history, type = "grid")
```

<img src="https://raw.githubusercontent.com/Nth-iteration-labs/contextual/master/tools/readme/basic_contextual_plot.png" width="100%" />
