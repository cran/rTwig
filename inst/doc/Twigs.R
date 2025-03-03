## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("rTwig")

## ----eval=FALSE---------------------------------------------------------------
# devtools::install_github("https://github.com/aidanmorales/rTwig")

## ----message=FALSE------------------------------------------------------------
library(rTwig)

# Useful packages
library(dplyr)
library(ggplot2)

## ----eval=FALSE---------------------------------------------------------------
# # If the rTwig library has been loaded
# twigs

## -----------------------------------------------------------------------------
# If rTwig hasn't been loaded, but just the twigs are needed
rTwig::twigs

## -----------------------------------------------------------------------------
unique(twigs$scientific_name)

## -----------------------------------------------------------------------------
twigs_index

## ----fig.height=6, fig.width=7, echo = FALSE----------------------------------
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

twigs %>%
  mutate(
    size_index = case_when(
      radius_mm <= 1 ~ "slender",
      radius_mm > 1 & radius_mm <= 2 ~ "moderately slender",
      radius_mm > 2 & radius_mm <= 2.5 ~ "moderately stout",
      radius_mm > 2.5 ~ "stout"
    ),
    size_index = factor(
      size_index, 
      levels = c("slender", "moderately slender", "moderately stout", "stout")
    ),
    index = case_when(
      size_index == "slender" ~ 1,
      size_index == "moderately slender" ~ 2,
      size_index == "moderately stout" ~ 3,
      size_index == "stout" ~ 4
    )
  ) %>%
  ggplot(
    aes(
      y = radius_mm,
      x = index,
      color = size_index,
      fill = size_index
    )
  ) +
  geom_boxplot() +
  stat_summary(
    fun = "mean",
    geom = "crossbar",
    width = 0.75,
    colour = "black",
    show.legend = FALSE
  ) +
  labs(
    x = "",
    y = "Twig Radius (mm)",
    color = "Twig Size Index",
    fill = "Twig Size Index"
  ) +
  theme_classic()


