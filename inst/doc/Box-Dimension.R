## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("rTwig")

## ----echo=FALSE, warning=FALSE------------------------------------------------
# Enable RGL Plot
library(rgl)

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("https://github.com/aidanmorales/rTwig")

## ----message=FALSE, warning=FALSE---------------------------------------------
library(rTwig)

## -----------------------------------------------------------------------------
file <- system.file("extdata/cloud.txt", package = "rTwig")
cloud <- read.table(file, header = FALSE)

## -----------------------------------------------------------------------------
# Box Dimension Summary
output <- box_dimension(cloud)
str(output)

# Box Dimension (slope)
output[[2]]$slope

## ----fig.width=7, fig.height=5------------------------------------------------
# 2D Plot
output <- box_dimension(cloud, plot = "2D")

## ----webgl=TRUE, fig.width=7, fig.height=5, fig.align='center', eval=FALSE----
#  # 3D Plot
#  output <- box_dimension(cloud, plot = "3D")

