% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct_radii.R
\name{correct_radii}
\alias{correct_radii}
\title{Correct Radii}
\usage{
correct_radii(cylinder, twig_radius, backend = "multisession")
}
\arguments{
\item{cylinder}{QSM cylinder data frame}

\item{twig_radius}{Twig radius in millimeters}

\item{backend}{Parallel backend for multi-core processing. Defaults to "multisession" (all platforms), but can be set to "multicore" (MacOS & Linux), "cluster" (all platforms), or a "package::backend" string.}
}
\value{
Returns a data frame
}
\description{
Corrects cylinder radii
}
\examples{
\donttest{

## TreeQSM Processing Chain
file <- system.file("extdata/QSM.mat", package = "rTwig")
qsm <- import_qsm(file)
cylinder <- qsm$cylinder
cylinder <- update_cylinders(cylinder)
cylinder <- correct_radii(cylinder, twig_radius = 4.23)
str(cylinder)

## SimpleForest Processing Chain
file <- system.file("extdata/QSM.csv", package = "rTwig")
cylinder <- read.csv(file)
cylinder <- update_cylinders(cylinder)
cylinder <- correct_radii(cylinder, twig_radius = 4.23)
str(cylinder)
}

}
