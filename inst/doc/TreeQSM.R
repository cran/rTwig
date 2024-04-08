## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
#  install.packages("rTwig")

## ----eval=FALSE---------------------------------------------------------------
#  devtools::install_github("https://github.com/aidanmorales/rTwig")

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
qsm <- import_qsm(file)

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
qsm2 <- import_qsm(file, version = "2.0")

# QSM Info
summary(qsm2)
str(qsm2$cylinder)

## ----message=FALSE------------------------------------------------------------
# Save cylinders to new object
cylinder <- qsm$cylinder

# Update cylinder data
cylinder <- update_cylinders(cylinder)

# Check ordering
cylinder %>%
  select(branch, parent, extension, PositionInBranch) %>%
  head()

# Check New Variables
cylinder %>%
  select(reverseBranchOrder, totalChildren, GrowthLength, segment, parentSegment) %>%
  head()

## ----eval = FALSE-------------------------------------------------------------
#  # Load the input point cloud
#  file <- system.file("extdata/cloud.txt", package = "rTwig")
#  cloud <- read.table(file, header = FALSE)
#  
#  # Plot the qsm and point cloud
#  plot_qsm(cylinder = cylinder, cloud = cloud, radius = cylinder$UnmodRadius)

## -----------------------------------------------------------------------------
# Look at the twigs database
twigs

# Find our species
filter(twigs, scientific.name == "Gymnocladus dioicus")

## ----message = FALSE----------------------------------------------------------
# QSM summary
qsm_summary(cylinder, radius = "unmodified")

# QSM summary with Triangulation
qsm_summary(cylinder = cylinder, triangulation = qsm$triangulation, radius = "unmodified")

## ----echo=FALSE, fig.width = 7, fig.height = 4, fig.align='center'------------
# Diagnostic Plot 1
cylinder %>%
  ggplot(aes(x = GrowthLength, y = radius, color = GrowthLength)) +
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

## ----echo=FALSE, fig.width = 7, fig.height = 4, fig.align='center'------------
# Diagnostic Plot 2
cylinder %>%
  ggplot(aes(x = GrowthLength, y = radius, color = branch)) +
  geom_point() +
  facet_wrap(~BranchOrder) +
  labs(
    title = "Radius vs Growth Length by Branch Order",
    x = "Growth Length (m)",
    y = "Radius (m)",
    color = "Branch"
  ) +
  theme_classic() +
  scale_y_log10() +
  scale_x_log10() +
  geom_hline(yintercept = .00423)

## ----message = FALSE, results = 'hide'----------------------------------------
# Correct cylinder radii
cylinder <- correct_radii(cylinder, twigRad = 4.23)

## ----r, message = FALSE-------------------------------------------------------
# Corrected QSM summary
qsm_summary(cylinder, radius = "modified")

## ----echo=FALSE, fig.width = 7, fig.height = 4, fig.align='center'------------
# Diagnostic Plot 1
cylinder %>%
  ggplot(aes(x = GrowthLength, y = radius, color = GrowthLength)) +
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

## ----echo=FALSE, fig.width = 7, fig.height = 4, fig.align='center'------------
# Diagnostic Plot 2
cylinder %>%
  ggplot(aes(x = GrowthLength, y = radius, color = branch)) +
  geom_point() +
  facet_wrap(~BranchOrder) +
  labs(
    title = "Radius vs Growth Length by Branch Order",
    x = "Growth Length (m)",
    y = "Radius (m)",
    color = "Branch"
  ) +
  theme_classic() +
  scale_y_log10() +
  scale_x_log10() +
  geom_hline(yintercept = .00423)

## ----eval=FALSE---------------------------------------------------------------
#  # Smooth QSM
#  cylinder <- smooth_qsm(cylinder)
#  
#  # Plot QSM
#  plot_qsm(cylinder)
#  
#  # QSM Custom Colors & Piping
#  cylinder %>%
#    plot_qsm(
#      radius = .$radius,
#      cyl_color = .$reverseBranchOrder,
#      cyl_palette = "magma"
#    )
#  
#  # Plot Twigs Colored by Unique Segment
#  cylinder %>%
#    filter(reverseBranchOrder == 1) %>%
#    plot_qsm(
#      radius = .$radius,
#      cyl_color = .$reverseBranchOrder,
#      cyl_palette = "rainbow"
#    )

## ----eval=FALSE---------------------------------------------------------------
#  # Export Mesh Colored by RBO
#  cylinder %>%
#    export_mesh(
#      filename = "QSM_mesh",
#      radius = .$radius,
#      cyl_color = .$reverseBranchOrder,
#      cyl_palette = "magma"
#    )
#  
#  # Export Twigs Colored by Unique Segments
#  cylinder %>%
#    filter(reverseBranchOrder == 1) %>%
#    export_mesh(
#      filename = "QSM_mesh",
#      radius = .$radius,
#      cyl_color = .$reverseBranchOrder,
#      cyl_palette = "rainbow"
#    )

## ----eval=FALSE---------------------------------------------------------------
#  # Import QSM
#  file <- system.file("extdata/QSM.mat", package = "rTwig")
#  qsm <- import_qsm(file)
#  
#  # Real Twig Main Steps
#  cylinder <- qsm$cylinder
#  cylinder <- update_cylinders(cylinder)
#  cylinder <- correct_radii(cylinder, twigRad = 4.23)
#  
#  # Summary Metrics
#  qsm_summary(cylinder)
#  
#  # Plot Results
#  plot_qsm(cylinder)

## ----echo=FALSE, results='hide', message=FALSE, warning=FALSE-----------------
# Import Data
file <- system.file("extdata/validation.csv", package = "rTwig")
all_biomass <- read.csv(file) %>%
  rename(Version = version)

# Distinct colors
factors <- distinct(all_biomass, Version)
cbPalette <- c("#009E73", "#CC79A7", "#56B4E9", "#999999", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00")
cbPalette <- cbPalette[1:nrow(factors)]
factors <- bind_cols(factors, cbPalette) %>%
  rename(colors = 2)

# Filter Data
all_biomass <- all_biomass %>%
  left_join(factors, by = join_by(Version)) %>%
  filter(Version %in% c("TreeQSM v2.4.1", "Real Twig (TreeQSM v2.4.1)")) %>%
  mutate(id = paste0(SpCode, Tree))

# Total Biomass
p1 <- ggplot(data = all_biomass, aes(x = Mt.DS, y = Mt.TLS, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name), ) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_abline() +
  stat_poly_eq(aes(
    label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")
  )) +
  labs(
    x = "", # "Reference Biomass (kg)",
    y = "TLS Biomass (kg)",
    title = "Total",
    shape = "Species",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

# Main Stem Biomass
p2 <- ggplot(data = all_biomass, aes(x = Md.DS, y = Md.TLS, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_abline() +
  stat_poly_eq(aes(
    label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")
  )) +
  labs(
    x = "", # "Reference Biomass (kg)",
    y = "", # "TLS Biomass (kg)",
    title = "Main Stem",
    shape = "Species",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

# Branch Biomass
p3 <- ggplot(data = all_biomass, aes(x = Mb.DS, y = Mb.TLS, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_abline() +
  stat_poly_eq(aes(
    label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")
  )) +
  labs(
    x = "", # "Reference Biomass (kg)",
    y = "", # "TLS Biomass (kg)",
    title = "Branch",
    shape = "Species",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

# Total Biomass % Error
p4 <- ggplot(data = all_biomass, aes(x = Mt.DS, y = tperr, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_hline(yintercept = 0) +
  labs(
    x = "", # "Reference Biomass (kg)",
    y = "% TLS Biomass Error",
    title = "Total % Error",
    shape = "Speces",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

# Main Stem Biomass % Error
p5 <- ggplot(data = all_biomass, aes(x = Md.DS, y = sperr, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_hline(yintercept = 0) +
  labs(
    x = "Reference Biomass (kg)",
    y = "", # "% TLS Biomass Error",
    title = "Main Stem % Error",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

# Branch Biomass % Error
p6 <- ggplot(data = all_biomass, aes(x = Mb.DS, y = tperr, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_hline(yintercept = 0) +
  labs(
    x = "", # "Reference Biomass (kg)",
    y = "", # "% TLS Biomass Error",
    title = "Branch % Error",
    shape = "Species",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

## ----echo=FALSE, fig.width=7, fig.height=4, fig.align='center', warning=FALSE----
ggarrange(p1, p4, common.legend = TRUE, legend = "bottom")
ggarrange(p2, p5, common.legend = TRUE, legend = "bottom")
ggarrange(p3, p6, common.legend = TRUE, legend = "bottom")

## ----echo=FALSE, warning=FALSE------------------------------------------------
### TOTALS #####################################################################
total_stats <- all_biomass %>%
  group_by(Version) %>%
  summarize(
    MRE.pct = mean(tperr),
    RMSE.kg = sqrt(mean(tdiff^2, na.rm = TRUE)),
    RRMSE.pct = RMSE.kg / mean(Mt.DS) * 100
  )

CCC_total <- all_biomass %>%
  group_by(Version) %>%
  ccc(Mt.TLS, Mt.DS) %>%
  select(Version, .estimate) %>%
  rename(CCC = 2)

total_stats <- left_join(total_stats, CCC_total, by = "Version") %>%
  mutate(type = "total")

### MAIN STEM ##################################################################
stem_stats <- all_biomass %>%
  group_by(Version) %>%
  summarize(
    MRE.pct = mean(sperr),
    RMSE.kg = sqrt(mean(sdif^2, na.rm = TRUE)),
    RRMSE.pct = RMSE.kg / mean(Md.DS) * 100
  )

CCC_stem <- all_biomass %>%
  group_by(Version) %>%
  ccc(Md.TLS, Md.DS) %>%
  select(Version, .estimate) %>%
  rename(CCC = 2)

stem_stats <- left_join(stem_stats, CCC_stem, by = "Version") %>%
  mutate(type = "stem")

### BRANCHES ###################################################################
branch_stats <- all_biomass %>%
  group_by(Version) %>%
  summarize(
    MRE.pct = mean(bperr),
    RMSE.kg = sqrt(mean(bdiff^2, na.rm = TRUE)),
    RRMSE.pct = RMSE.kg / mean(Mb.DS) * 100
  )

CCC_branch <- all_biomass %>%
  group_by(Version) %>%
  ccc(Mb.TLS, Mb.DS) %>%
  select(Version, .estimate) %>%
  rename(CCC = 2)

branch_stats <- left_join(branch_stats, CCC_branch, by = "Version") %>%
  mutate(type = "branch")

all_stats <- bind_rows(total_stats, stem_stats, branch_stats)

### TABLES ###################################################################
data <- all_stats %>%
  mutate_if(is.numeric, round, 3) %>%
  rename(
    "Mean Relative Error (%)" = MRE.pct,
    "RMSE (kg)" = RMSE.kg,
    "Relative RMSE (%)" = RRMSE.pct
  ) %>%
  pivot_longer(cols = 2:5, names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  group_by(Version) %>%
  group_split()

tables <- vector(mode = "list", length(data))

data[[2]] %>%
  gt() %>%
  tab_header(title = "TreeQSM v2.4.1") %>%
  cols_hide(Version) %>%
  cols_label(
    metric = "Metric",
    total = "Total Woody AGB",
    stem = "Main Stem Biomass",
    branch = "Branch Biomass"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.hlines.color = "white",
    table.font.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

data[[1]] %>%
  gt() %>%
  tab_header(title = "Real Twig (TreeQSM v2.4.1)") %>%
  cols_hide(Version) %>%
  cols_label(
    metric = "Metric",
    total = "Total Woody AGB",
    stem = "Main Stem Biomass",
    branch = "Branch Biomass"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.hlines.color = "white",
    table.font.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

## ----echo=FALSE, results='hide', message=FALSE, warning=FALSE-----------------
# Import Data
file <- system.file("extdata/validation.csv", package = "rTwig")
all_biomass <- read.csv(file) %>%
  rename(Version = version)

# Distinct colors
factors <- distinct(all_biomass, Version)
cbPalette <- c("#009E73", "#CC79A7", "#56B4E9", "#999999", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00")
cbPalette <- cbPalette[1:nrow(factors)]
factors <- bind_cols(factors, cbPalette) %>%
  rename(colors = 2)

# Filter Data
all_biomass <- all_biomass %>%
  left_join(factors, by = join_by(Version)) %>%
  filter(Version %in% c("TreeQSM v2.3.0", "Real Twig (TreeQSM v2.3.0)")) %>%
  mutate(id = paste0(SpCode, Tree))

# Total Biomass
p1 <- ggplot(data = all_biomass, aes(x = Mt.DS, y = Mt.TLS, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name), ) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_abline() +
  stat_poly_eq(aes(
    label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")
  )) +
  labs(
    x = "", # "Reference Biomass (kg)",
    y = "TLS Biomass (kg)",
    title = "Total",
    shape = "Species",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

# Main Stem Biomass
p2 <- ggplot(data = all_biomass, aes(x = Md.DS, y = Md.TLS, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_abline() +
  stat_poly_eq(aes(
    label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")
  )) +
  labs(
    x = "", # "Reference Biomass (kg)",
    y = "", # "TLS Biomass (kg)",
    title = "Main Stem",
    shape = "Species",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

# Branch Biomass
p3 <- ggplot(data = all_biomass, aes(x = Mb.DS, y = Mb.TLS, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_abline() +
  stat_poly_eq(aes(
    label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")
  )) +
  labs(
    x = "", # "Reference Biomass (kg)",
    y = "", # "TLS Biomass (kg)",
    title = "Branch",
    shape = "Species",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

# Total Biomass % Error
p4 <- ggplot(data = all_biomass, aes(x = Mt.DS, y = tperr, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_hline(yintercept = 0) +
  labs(
    x = "", # "Reference Biomass (kg)",
    y = "% TLS Biomass Error",
    title = "Total % Error",
    shape = "Speces",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

# Main Stem Biomass % Error
p5 <- ggplot(data = all_biomass, aes(x = Md.DS, y = sperr, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_hline(yintercept = 0) +
  labs(
    x = "Reference Biomass (kg)",
    y = "", # "% TLS Biomass Error",
    title = "Main Stem % Error",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

# Branch Biomass % Error
p6 <- ggplot(data = all_biomass, aes(x = Mb.DS, y = tperr, color = Version, fill = Version)) +
  geom_point(aes(shape = scientific.name)) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_hline(yintercept = 0) +
  labs(
    x = "", # "Reference Biomass (kg)",
    y = "", # "% TLS Biomass Error",
    title = "Branch % Error",
    shape = "Species",
    color = "Method",
    fill = "Method"
  ) +
  theme_classic() +
  scale_color_manual(values = unique(all_biomass$colors)) +
  scale_fill_manual(values = unique(all_biomass$colors))

## ----echo=FALSE, fig.width=7, fig.height=4, fig.align='center', warning=FALSE, message=FALSE----
ggarrange(p1, p4, common.legend = TRUE, legend = "bottom")
ggarrange(p2, p5, common.legend = TRUE, legend = "bottom")
ggarrange(p3, p6, common.legend = TRUE, legend = "bottom")

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------------------
### TOTALS #####################################################################
total_stats <- all_biomass %>%
  group_by(Version) %>%
  summarize(
    MRE.pct = mean(tperr),
    RMSE.kg = sqrt(mean(tdiff^2, na.rm = TRUE)),
    RRMSE.pct = RMSE.kg / mean(Mt.DS) * 100
  )

CCC_total <- all_biomass %>%
  group_by(Version) %>%
  ccc(Mt.TLS, Mt.DS) %>%
  select(Version, .estimate) %>%
  rename(CCC = 2)

total_stats <- left_join(total_stats, CCC_total, by = "Version") %>%
  mutate(type = "total")

### MAIN STEM ##################################################################
stem_stats <- all_biomass %>%
  group_by(Version) %>%
  summarize(
    MRE.pct = mean(sperr),
    RMSE.kg = sqrt(mean(sdif^2, na.rm = TRUE)),
    RRMSE.pct = RMSE.kg / mean(Md.DS) * 100
  )

CCC_stem <- all_biomass %>%
  group_by(Version) %>%
  ccc(Md.TLS, Md.DS) %>%
  select(Version, .estimate) %>%
  rename(CCC = 2)

stem_stats <- left_join(stem_stats, CCC_stem, by = "Version") %>%
  mutate(type = "stem")

### BRANCHES ###################################################################
branch_stats <- all_biomass %>%
  group_by(Version) %>%
  summarize(
    MRE.pct = mean(bperr),
    RMSE.kg = sqrt(mean(bdiff^2, na.rm = TRUE)),
    RRMSE.pct = RMSE.kg / mean(Mb.DS) * 100
  )

CCC_branch <- all_biomass %>%
  group_by(Version) %>%
  ccc(Mb.TLS, Mb.DS) %>%
  select(Version, .estimate) %>%
  rename(CCC = 2)

branch_stats <- left_join(branch_stats, CCC_branch, by = "Version") %>%
  mutate(type = "branch")

all_stats <- bind_rows(total_stats, stem_stats, branch_stats)

### TABLES ###################################################################
data <- all_stats %>%
  mutate_if(is.numeric, round, 3) %>%
  rename(
    "Mean Relative Error (%)" = MRE.pct,
    "RMSE (kg)" = RMSE.kg,
    "Relative RMSE (%)" = RRMSE.pct
  ) %>%
  pivot_longer(cols = 2:5, names_to = "metric", values_to = "value") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  group_by(Version) %>%
  group_split()

tables <- vector(mode = "list", length(data))

data[[2]] %>%
  gt() %>%
  tab_header(title = "TreeQSM v2.3.0") %>%
  cols_hide(Version) %>%
  cols_label(
    metric = "Metric",
    total = "Total Woody AGB",
    stem = "Main Stem Biomass",
    branch = "Branch Biomass"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.hlines.color = "white",
    table.font.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

data[[1]] %>%
  gt() %>%
  tab_header(title = "Real Twig (TreeQSM v2.3.0)") %>%
  cols_hide(Version) %>%
  cols_label(
    metric = "Metric",
    total = "Total Woody AGB",
    stem = "Main Stem Biomass",
    branch = "Branch Biomass"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_options(
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    table_body.hlines.color = "white",
    table.font.color = "black",
    heading.border.bottom.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------
# Future Package Cleanup
future::plan("sequential")

