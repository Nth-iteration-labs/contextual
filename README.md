# Contextual

[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Nth-iteration-labs/contextual?branch=master&svg=true)](https://ci.appveyor.com/project/Nth-iteration-labs/contextual)
[![Build Status](https://travis-ci.org/Nth-iteration-labs/contextual.svg?branch=master)](https://travis-ci.org/Nth-iteration-labs/contextual) [![codecov](https://codecov.io/gh/Nth-iteration-labs/contextual/branch/master/graph/badge.svg)](https://codecov.io/gh/Nth-iteration-labs/contextual) [![develVersion](https://img.shields.io/badge/devel%20version-0.0.0.9012-blue.svg?style=flat)](https://github.com/Nth-iteration-labs/contextual)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/114037654.svg)](https://zenodo.org/badge/latestdoi/114037654)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/contextual)](https://cran.r-project.org/package=contextual)

Overview
--------

Contextual is an R package that facilitates the simulation and analysis of Contextual Multi-Armed Bandit (CMAB) problems.

Documentation
-------------

* [Online package documentation](https://nth-iteration-labs.github.io/contextual/)
* [Blog at Pavlov](https://pavlov.tech/category/contextual/)

Installation
------------

``` r
# Install development version from GitHub:
# install.packages("devtools")
devtools::install_github("Nth-iteration-labs/contextual")
```

If you encounter a clear bug, please file a minimal reproducible example on [github](https://github.com/Nth-iteration-labs/contextual/issues).

Usage
-----

Running a basic Multi-Armed Bandit policy comparison simulation:

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
                            Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit)
                          )

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "grid")

```

![MAB plot](tools/readme/basic_multi_plot.png?raw=true "MAB plot")

Running a Contextual Multi-Armed Bandit policy comparison simulation:

``` r
library(contextual)

horizon            <- 100L
simulations        <- 2000L
weights            <- matrix(  c( 0.9, 0.1, 0.1,
                                  0.1, 0.9, 0.1,
                                  0.1, 0.1, 0.9), nrow = 3, ncol = 3)

bandit             <- SyntheticBandit$new(data = weights )
agents             <- list(
                            Agent$new(OraclePolicy$new("Oracle"), bandit),
                            Agent$new(ThompsonSamplingPolicy(1, 1, "TS"), bandit),
                            Agent$new(LinUCBPolicy$new(1.0, "LinUCB"), bandit) )

simulation         <- Simulator$new(agents, horizon, simulations)
history            <- simulation$run()

plot(history, type = "grid")

```

![CMAB plot](tools/readme/contextual_plot.png?raw=true "CMAB plot")
