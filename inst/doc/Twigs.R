## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("rTwig")

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("https://github.com/aidanmorales/rTwig")

## ----message=FALSE------------------------------------------------------------
library(rTwig)

# Useful packages
library(dplyr)
library(ggplot2)

## ----eval=FALSE---------------------------------------------------------------
#  # If the rTwig library has been loaded
#  twigs

## -----------------------------------------------------------------------------
# If rTwig hasn't been loaded, but just the twigs are needed
rTwig::twigs

## -----------------------------------------------------------------------------
unique(twigs$scientific_name)

## ----fig.height=6, fig.width=7------------------------------------------------
# Lets look at a subset of oak species
twigs %>%
  filter(grepl("Quercus", scientific_name)) %>%
  ggplot(aes(x = scientific_name, y = radius_mm, color = scientific_name)) +
  geom_point(aes(size = n)) +
  geom_errorbar(aes(ymax = max, ymin = min)) +
  coord_flip() +
  labs(
    title = "Quercus Twig Radii",
    x = "",
    y = "Twig Radius (mm)",
    color = "Species",
    size = "Sample Size"
  ) +
  scale_x_discrete(limits = rev) +
  theme_classic()

