Contextual: Multi-Armed Bandits in R
==========

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://tidyverse.org/lifecycle/#maturing)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/Nth-iteration-labs/contextual?branch=master&svg=true)](https://ci.appveyor.com/project/robinvanemden/contextual) [![Build Status](https://travis-ci.org/Nth-iteration-labs/contextual.svg?branch=master)](https://travis-ci.org/Nth-iteration-labs/contextual) [![codecov](https://codecov.io/gh/Nth-iteration-labs/contextual/branch/master/graph/badge.svg)](https://codecov.io/gh/Nth-iteration-labs/contextual) [![develVersion](https://img.shields.io/badge/devel%20version-0.9.0-green.svg?style=flat)](https://github.com/Nth-iteration-labs/contextual) [![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](https://www.gnu.org/licenses/gpl-3.0) [![DOI](https://zenodo.org/badge/114037654.svg)](https://zenodo.org/badge/latestdoi/114037654) <!-- [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/contextual)](https://cran.r-project.org/package=contextual) -->

Overview
--------

[R6](https://github.com/r-lib/R6) based R package facilitating the simulation and evaluation of context-free and contextual Multi-Armed Bandit policies or algorithms. 

The package has been developed to: 

* Introduce a wider audience to contextual bandit policies' advanced sequential decision strategies.
* Ease the implementation, evaluation and dissemination of both existing and new contextual Multi-Armed Bandit algorithms and policies. 

Installation
------------

```r
# Install development version from GitHub:
# install.packages("devtools")
devtools::install_github("Nth-iteration-labs/contextual")
```

Documentation
-------------

* [Class reference](https://nth-iteration-labs.github.io/contextual/reference/index.html)
* [Getting started: running simulations](https://nth-iteration-labs.github.io/contextual/articles/contextual.html)

<!---   [Blog at Pavlov](https://pavlov.tech/category/contextual/)-->

Overview of core classes
------------------------

![Contextual's core class diagram](https://raw.githubusercontent.com/Nth-iteration-labs/contextual/master/man/figures/cmab_all_large.jpg)

Policies and Bandits
--------------------

Overview of contextual's growing library of contextual and context-free bandit policies:

| General | Context-free | Contextual |
|---------------|-------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------|
| Random<br>  Oracle<br> <br> <br> <br> <br><br>   | Epsilon-Greedy<br>  Epsilon-First<br>  UCB-tuned<br>   Thompson Sampling<br>   BootstrapTS<br>   Softmax<br>   Gittins | CMAB Epoch-Greedy<br>   LinUCB (General, Disjoint, Hybrid)<br>  COFIBA*<br>   LinTS<br>   LogitBTS<br><br> <br>   |

Overview of contextual's bandit library:

| Context-free  | Contextual | Offline | Continuous |
|------------------------------------------|---------------------------------------------------------------------------------------|------------------------------------------------------------|------------|
| BasicBernoulliBandit<br>  BasicGaussianBandit<br><br> <br> <br>    | ContextualBernoulli<br>  ContextualLogit<br>  ContextualHybrid<br>  ContextualLinear<br>  ContextualWheel | OfflinePolicyEvaluator<br>  PropensityEvaluator*<br>   DoublyRobust*<br> <br> <br>   | Continuum <br> <br> <br> <br> <br> |

\* available soon

Maintainers
-----------

Robin van Emden: author, maintainer*
Maurits Kaptein: supervisor*

\* [Tilburg University](https://www.tilburguniversity.edu/) / [Jheronimus Academy of Data Science](https://www.jads.nl/research.html).

If you encounter a clear bug, please file a minimal reproducible example on [GitHub](https://github.com/Nth-iteration-labs/contextual/issues).
