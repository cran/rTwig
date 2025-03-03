## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("rTwig")

## ----eval=FALSE---------------------------------------------------------------
# devtools::install_github("https://github.com/aidanmorales/rTwig")

## ----message=FALSE, warning=FALSE---------------------------------------------
# Load rTwig
library(rTwig)

# Other useful packages
library(dplyr)

## ----echo=FALSE, message=FALSE------------------------------------------------
# Plotting Packages
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(yardstick)
library(gt)

## ----message=FALSE------------------------------------------------------------
# QSM directory
file <- system.file("extdata/QSM.mat", package = "rTwig")

# Import and save QSM
qsm <- import_treeqsm(file)

## ----message=FALSE------------------------------------------------------------
summary(qsm)

## ----message=FALSE------------------------------------------------------------
# QSM info
qsm$rundata$version
qsm$rundata$start.date

# Number of cylinders
str(qsm$cylinder)

## ----message=FALSE------------------------------------------------------------
# QSM Directory
file <- system.file("extdata/QSM_2.mat", package = "rTwig")

# Import and save QSM
qsm2 <- import_treeqsm(file, version = "2.0")

# QSM Info
summary(qsm2)
str(qsm2$cylinder)

## ----message=FALSE------------------------------------------------------------
# QSM directory
file <- system.file("extdata/QSM.csv", package = "rTwig")

# Import and save QSM cylinder data
cylinder <- read.csv(file)

## ----message=FALSE------------------------------------------------------------
str(cylinder)

## ----message=FALSE------------------------------------------------------------
# Save cylinders to new object
cylinder <- qsm$cylinder

# Update cylinder data
cylinder <- update_cylinders(cylinder)

## ----eval = FALSE-------------------------------------------------------------
# # Load the input point cloud
# file <- system.file("extdata/cloud.txt", package = "rTwig")
# cloud <- read.table(file, header = FALSE)
# 
# # Plot the qsm and point cloud
# plot_qsm(cylinder = cylinder, cloud = cloud, radius = "UnmodRadius")

## -----------------------------------------------------------------------------
# Look at the twigs database
twigs

# Find our species
filter(twigs, scientific_name == "Gymnocladus dioicus")

## ----message = FALSE----------------------------------------------------------
# QSM summary
summarise_qsm(cylinder = cylinder, radius = radius)[[1]]

# QSM summary with Triangulation
summarise_qsm(cylinder = cylinder, radius = radius, triangulation = qsm$triangulation)[[1]]

## ----echo=FALSE, fig.width = 7, fig.height = 4, fig.align='center'------------
# Diagnostic Plot 1
cylinder %>%
  ggplot(aes(x = growthLength, y = radius, color = growthLength)) +
  geom_point() +
  labs(
    title = "Radius vs Growth Length",
    x = "Growth Length (m)",
    y = "Radius (m)",
    color = "Growth Length"
  ) +
  theme_classic() +
  scale_y_log10() +
  scale_x_log10() +
  geom_hline(yintercept = .00423) +
  scale_color_viridis_c()

## ----message = FALSE, results = 'hide'----------------------------------------
# Correct cylinder radii
cylinder <- correct_radii(cylinder, twig_radius = 4.23)

## ----r, message = FALSE-------------------------------------------------------
# Corrected QSM summary
summarise_qsm(cylinder, radius = radius)[[1]]

## ----echo=FALSE, fig.width = 7, fig.height = 4, fig.align='center'------------
# Diagnostic Plot 1
cylinder %>%
  ggplot(aes(x = growthLength, y = radius, color = growthLength)) +
  geom_point() +
  labs(
    title = "Radius vs Growth Length",
    x = "Growth Length (m)",
    y = "Radius (m)",
    color = "Growth Length"
  ) +
  theme_classic() +
  scale_y_log10() +
  scale_x_log10() +
  geom_hline(yintercept = .00423) +
  scale_color_viridis_c()

## ----eval=FALSE---------------------------------------------------------------
# # Smooth QSM
# cylinder <- smooth_qsm(cylinder)
# 
# # Plot QSM
# plot_qsm(cylinder)
# 
# # QSM Custom Colors & Piping
# cylinder %>%
#   plot_qsm(
#     radius = "radius",
#     cyl_color = "reverseBranchOrder",
#     cyl_palette = "magma"
#   )
# 
# # Plot Twigs Colored by Unique Segment
# cylinder %>%
#   filter(reverseBranchOrder == 1) %>%
#   plot_qsm(
#     radius = "radius",
#     cyl_color = "reverseBranchOrder",
#     cyl_palette = "rainbow"
#   )

## ----eval=FALSE---------------------------------------------------------------
# # Export Mesh Colored by RBO
# cylinder %>%
#   export_mesh(
#     filename = "QSM_mesh",
#     radius = "radius",
#     color = "reverseBranchOrder",
#     palette = "magma"
#   )
# 
# # Export Twigs Colored by Unique Segments
# cylinder %>%
#   filter(reverseBranchOrder == 1) %>%
#   export_mesh(
#     filename = "QSM_mesh",
#     radius = "radius",
#     color = "reverseBranchOrder",
#     palette = "rainbow"
#   )

## ----eval=FALSE---------------------------------------------------------------
# # Import QSM
# file <- system.file("extdata/QSM.mat", package = "rTwig")
# 
# # Real Twig Main Steps
# cylinder <- run_rtwig(file, twig_radius = 4.23)
# 
# # Tree Metrics
# metrics <- tree_metrics(cylinder)
# 
# # Plot Results
# plot_qsm(cylinder)

