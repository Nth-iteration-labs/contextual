Contextual: Multi-Armed Bandits in R
====================================

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg?style=flat)](https://tidyverse.org/lifecycle/#maturing) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Nth-iteration-labs/contextual?branch=master&svg=true)](https://ci.appveyor.com/project/robinvanemden/contextual) [![Build Status](https://travis-ci.org/Nth-iteration-labs/contextual.svg?branch=master)](https://travis-ci.org/Nth-iteration-labs/contextual) [![codecov](https://codecov.io/gh/Nth-iteration-labs/contextual/branch/master/graph/badge.svg)](https://codecov.io/gh/Nth-iteration-labs/contextual)[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0) [![DOI](https://zenodo.org/badge/114037654.svg)](https://zenodo.org/badge/latestdoi/114037654) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/contextual)](https://cran.r-project.org/package=contextual) 

<!--
* [AppVeyor: passing](https://ci.appveyor.com/project/robinvanemden/contextual)
* [Travis CI: passing](https://travis-ci.org/Nth-iteration-labs/contextual)
* [Codecov: 96% coverage](https://codecov.io/gh/Nth-iteration-labs/contextual)
-->

Overview
--------

R package facilitating the simulation and evaluation of context-free and contextual Multi-Armed Bandit policies.

The package has been developed to:

* Ease the implementation, evaluation and dissemination of both existing and new contextual Multi-Armed Bandit policies. 
* Introduce a wider audience to contextual bandit policies' advanced sequential decision strategies.

Installation
------------

To install contextual from CRAN:

```R
install.packages('contextual')
```

To install the development version (requires the devtools package):

```R
install.packages("devtools")
devtools::install_github('Nth-iteration-labs/contextual')
```

When working on or extending the package, clone its [GitHub repository](https://github.com/Nth-iteration-labs/contextual), then do:

```R
install.packages("devtools")
devtools::install_deps(dependencies = TRUE)
devtools::build()
devtools::reload()
```

clean and rebuild...

Documentation
-------------

See the [demo directory](https://github.com/Nth-iteration-labs/contextual/tree/master/demo) for practical examples and replications of both synthetic and offline (contextual) bandit policy evaluations. 

How to replicate figures from two introductory context-free Multi-Armed Bandits texts:

* [Replication of figures from Sutton and Barto, "Reinforcement Learning: An Introduction", Chapter 2](https://nth-iteration-labs.github.io/contextual/articles/sutton_barto.html)
* [Replication of figures from "Bandit algorithms for website optimization" by John Miles White](https://nth-iteration-labs.github.io/contextual/articles/website_optimization.html)

Basic, context-free multi-armed bandit examples:

* [Basic MAB Epsilon Greedy evaluation](https://nth-iteration-labs.github.io/contextual/articles/epsilongreedy.html)
* [Synthetic MAB policy comparison](https://nth-iteration-labs.github.io/contextual/articles/mabs.html)
* [Replication Eckles & Kaptein (Bootstrap Thompson Sampling)](https://nth-iteration-labs.github.io/contextual/articles/eckles_kaptein.html)

Examples of both synthetic and offline *contextual* multi-armed bandit evaluations:

* [Synthetic cMAB policy comparison](https://nth-iteration-labs.github.io/contextual/articles/cmabs.html)

* [Offline Evaluation Data Set - Basic Replay Bandit: Ratings](https://nth-iteration-labs.github.io/contextual/articles/cmabsoffline.html)
* [Offline Evaluation Data Set - Bootstrapped Replay Bandit: Carskit DePaul Movies](https://nth-iteration-labs.github.io/contextual/articles/offline_depaul_movies.html)
* [Offline Evaluation Data Set - Lookup Table Replay Bandit: MovieLens 10M](https://nth-iteration-labs.github.io/contextual/articles/ml10m.html) 

* [Offline Bandits and Simpson's Paradox](https://nth-iteration-labs.github.io/contextual/articles/simpsons.html)

Some more extensive vignettes to get you started with the package:

* [Getting started: running simulations](https://nth-iteration-labs.github.io/contextual/articles/introduction.html)
* [Offline evaluation: replication of Li et al (2010)](https://nth-iteration-labs.github.io/contextual/articles/replication.html)
* [Class reference](https://nth-iteration-labs.github.io/contextual/reference/index.html)

Paper offering a general overview of the package's structure & API:

* [An introduction to multi-armed bandit problems and the use of the R package contextual](https://arxiv.org/abs/1811.01926) paper.


<!---
* [Blog at Pavlov](https://pavlov.tech/category/contextual/)
-->

Overview of core classes
------------------------


![](man/figures/cmab_all_medium.jpg)

Policies and Bandits
--------------------

Overview of contextual's growing library of contextual and context-free bandit policies:

| General | Context-free | Contextual | Other |
|---------------|-------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------|------------------------------|
| Random<br>  Oracle<br> Fixed <br> <br> <br><br><br>  | Epsilon-Greedy<br>  Epsilon-First<br>  UCB1, UCB2<br>   Thompson Sampling<br>   BootstrapTS<br>   Softmax<br> Gradient<br>  Gittins | CMAB Naive Epsilon-Greedy <br> Epoch-Greedy<br>   LinUCB (General, Disjoint, Hybrid)<br>Linear Thompson Sampling<br> ProbitTS<br> LogitBTS<br>GLMUCB<br>  <br>  | Lock-in Feedback (LiF)  <br> <br> <br> <br><br> <br><br> <br>  |

Overview of contextual's bandit library:

| Basic Synthetic | Contextual Synthetic | Offline | Continuous |
|------------------------------------------|---------------------------------------------------------------------------------------|------------------------------------------------------------|------------|
| Basic Bernoulli Bandit<br>  Basic Gaussian Bandit<br><br> <br> <br>    | Contextual Bernoulli<br>  Contextual Logit<br>  Contextual Hybrid<br>  Contextual Linear<br>  Contextual Wheel | Replay Evaluator <br> Bootstrap Replay<br>Propensity Weighting<br>Direct Method<br>Doubly Robust<br>   | Continuum <br> <br> <br> <br> <br> |

Alternative parallel backends
-----------------------------

By default, "contextual" uses R's built-in parallel package to facilitate parallel evaluation of multiple agents over repeated simulation. See the demo/alternative_parallel_backends directory for several alternative parallel backends:

* [Microsoft Azure VM's using doAzureParallel.](https://github.com/Nth-iteration-labs/contextual/tree/master/demo/alternative_parallel_backends/azure)
* [Redis using doRedis.](https://github.com/Nth-iteration-labs/contextual/tree/master/demo/alternative_parallel_backends/redis)
* [MPI (Message-Passing Interface) using Rmpi and doMPI.](https://github.com/Nth-iteration-labs/contextual/tree/master/demo/alternative_parallel_backends/rmpi)

Maintainers
-----------

Robin van Emden: author, maintainer*
Maurits Kaptein: supervisor*

\* [Tilburg University](https://www.tilburguniversity.edu/) / [Jheronimus Academy of Data Science](https://www.jads.nl/research.html).

If you encounter a clear bug, please file a minimal reproducible example on [GitHub](https://github.com/Nth-iteration-labs/contextual/issues).
