
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rTwig <img src="man/figures/logo.svg" align="right" height="139"/>

<!-- badges: start -->

[![](https://www.r-pkg.org/badges/version/rTwig)](https://cran.r-project.org/package=rTwig)
[![](https://aidanmorales.r-universe.dev/badges/rTwig)](https://aidanmorales.r-universe.dev)
[![](https://github.com/aidanmorales/rTwig/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aidanmorales/rTwig/actions/workflows/R-CMD-check.yaml)
[![](https://img.shields.io/github/last-commit/aidanmorales/rTwig.svg)](https://github.com/aidanmorales/rTwig/commits/main)
[![](https://cranlogs.r-pkg.org/badges/grand-total/rTwig)](https://cran.r-project.org/package=rTwig)
[![](https://cranlogs.r-pkg.org/badges/last-month/rTwig)](https://cran.r-project.org/package=rTwig)
[![](https://img.shields.io/badge/license-GPL%20(%3E=%203)-orchid.svg)](https://cran.r-project.org/web/licenses/GPL-3)

<!-- badges: end -->

## Description

Real Twig is a method to correct unrealistic cylinders in quantitative
structure models (QSMs), especially overestimated small branch and twig
cylinders. Real Twig is different than traditional allometric or
statistical corrections. Real Twig uses real twig diameter measurements
from corresponding tree species to inform individual branch taper
models. Real Twig dynamically identifies realistic cylinders in a QSM
using network analysis, general additive models, and real twig
measurements, to model and correct unrealistic cylinders with a high
degree of precision and accuracy when vetted against ground truth
reference data.

Real Twig does much more than correct QSM cylinder radii. Real Twig
includes tools for QSM visualization and analysis that are seamlessly
compatible with base R and popular packages such as the Tidyverse. Real
Twig also includes fast, vectorized functions for point cloud fractal
analysis and visualization using the box-dimension metric. Also included
is a novel database of twig diameter measurements for many common North
American and European trees, on both the species and genus level.

<img src="man/figures/Plot_rTwig.png"/>

## Functions

- `run_rtwig()` runs the Real Twig method on a QSM
- `import_qsm()` imports a QSM created by TreeQSM (.mat)
- `import_treegraph()` imports a QSM created by treegraph (.json)
- `standardize_qsm()` standardizes QSM variable names across supported
  software
- `update_cylinders()` updates cylinder relationships and adds new QSM
  variables
- `correct_radii` models QSM paths and corrects cylinder radii
- `tree_metrics()` generates detailed tree metrics and a simulated point
  cloud
- `qsm_summary()` summarizes QSM diameter, height, volume, and surface
  area
- `smooth_qsm()` connects cylinder end-to-end to smooth branch
  visualization
- `plot_qsm()` plots QSMs and optionally their point clouds
- `export_mesh()` exports a QSM as a mesh object (.ply)
- `export_mat()` exports a QSM in a MATLAB format (.mat)
- `box_dimension()` calculates and visualizes the structural complexity
  of a point cloud

## Installation

You can install the released version of `rTwig` from
[CRAN](https://cran.r-project.org/package=rTwig) with:

``` r
install.packages("rTwig")
```

You can install the development version of `rTwig` from
[GitHub](https://github.com/aidanmorales/rTwig) with:

``` r
# install.packages("devtools")
devtools::install_github("aidanmorales/rTwig")
```

### Quick Start: QSM Radii Correction

QSM radii corrections are run as a chain of function that build on one
another. The general processing chain is as follows:

1.  Import a QSM: TreeQSM, SimpleForest and Treegraph are currently
    supported.
2.  Calculate new variables with consistent QSM structure to allow for
    network analysis.
3.  Correct the small branch and twig cylinders using real twig diameter
    measurements.
4.  Summarize and visualize the results.

Below are examples of how to quickly run Real Twig on TreeQSM and
SimpleForest QSMs, using example data from the package. See the
vignettes for more details on a general workflow and best practices.

#### TreeQSM

``` r
 # Load the Real Twig library
 library(rTwig)
 
 # File path to QSM
 file <- system.file("extdata/QSM.mat", package = "rTwig")
 
 # Correct QSM cylinders
 qsm <- run_rtwig(file, twig_radius = 4.23)
 
 # Plot the result
 plot_qsm(qsm$cylinder)
 
 # Calculate detailed tree metrics
 metrics <- tree_metrics(qsm$cylinder)
```

#### SimpleForest

``` r
 # Load the Real Twig library
 library(rTwig)
 
 # File path to QSM
 file <- system.file("extdata/QSM.csv", package = "rTwig")
 
 # Correct QSM cylinders
 qsm <- run_rtwig(file, twig_radius = 4.23)
 
 # Plot the result
 plot_qsm(qsm)
 
 # Calculate detailed tree metrics
 metrics <- tree_metrics(qsm)
```

### Quick Start: Box Dimension

Below is an example of how to quickly calculate box-dimension on a point
cloud using example data from the package. See the vignette for more
details and explanation.

``` r
# Load a point cloud
file <- system.file("extdata/cloud.txt", package = "rTwig")
cloud <- read.table(file)

# Calculate box-dimension
output <- box_dimension(cloud)
output[[2]]$slope
```
