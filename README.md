
<!-- README.md is generated from README.Rmd. Please edit that file -->

# A Hitchhiker’s Guide to Reproducible Research

<!-- badges: start -->

[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

## How to reproduce this manuscript

To reproduce this project Git, Make, and Docker is required (see [the
installation
guide](https://github.com/aaronpeikert/repro-tutorial/blob/main/install.md)).

Open the terminal, download the repository, and enter the directory:

``` bash
git clone https://github.com/aaronpeikert/repro-tutorial.git
cd repro-tutorial
```

Then build the Docker image, and run Make:

``` r
repro::rerun(cache = TRUE)
```

    ## • To reproduce this project, run the following code in a terminal:

    ##   make docker &&
    ##   make DOCKER=TRUE

This does not rerun the simulation study (unless you make changes to
it), because the computation may take a long time. If you are willing to
reproduce everything use:

``` r
repro::rerun(cache = FALSE)
```

    ## • To reproduce this project, run the following code in a terminal:

    ##   make docker &&
    ##   make -B DOCKER=TRUE

## Abstract

Computational reproducibility is the ability to obtain identical results
from the *same* data with the *same* computer code. It is a building
block for transparent and cumulative science because it enables other
researchers, on other computers and later in time, to reproduce and thus
understand how results came about while, at the same time, it avoids a
variety of errors that may lead to erroneous reporting of statistical
and computational results. In this tutorial, we demonstrate how the R
package `repro` supports researchers in creating fully computationally
reproducible research projects with a focus on current Open Science
practices; starting from sketching the research proposal, over
preregistering an analysis as executable code with simulated data, until
the creation and dissemination of the final scientific report.

![](images/nutshell.svg)<!-- -->

## Code of Conduct

Please note that the repro-tutorial project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
