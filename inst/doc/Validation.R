## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, message=FALSE------------------------------------------------
# Plotting Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(ggpmisc)
library(yardstick)
library(gt)

## ----echo=FALSE, results='hide', message=FALSE, warning=FALSE-----------------
# Import all data
file <- system.file("extdata/validation2.csv", package = "rTwig")
data_comp <- read.csv(file)

p_all <- ggplot(data = data_comp, aes(x = dbh.cm, y = tperr, color = version, fill = version)) +
  geom_point(aes(shape = Dataset, color = version, fill = version)) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  geom_hline(yintercept = 0) +
  labs(
    x = "DBH (cm)",
    y = "Total Mass Error (%)",
    color = "Method",
    fill = "Method",
    shape = "Dataset"
  ) +
  theme_classic() +
  scale_color_manual(values = c("#D41159", "#1A85FF")) + 
  scale_fill_manual(values = c("#D41159", "#1A85FF")) +
  geom_hline(
    yintercept = 10,
    linetype = "dashed",
    color = "black",
    linewidth = 0.25
  ) +
  geom_hline(
    yintercept = -10,
    linetype = "dashed",
    color = "black",
    linewidth = 0.25,
    show.legend = TRUE
  )

## ----echo=FALSE, fig.width=7, fig.height=4, fig.align='center', warning=FALSE----
p_all

## ----echo=FALSE, warning=FALSE------------------------------------------------
### ALL TREES ##################################################################
total_stats <- data_comp %>%
  ungroup() %>%
  group_by(version) %>%
  summarize(
    MRE.pct = mean(tperr, na.rm = TRUE),
    RMSE.kg = sqrt(mean(tdiff^2, na.rm = TRUE)),
    RRMSE.pct = RMSE.kg / mean(Mt.DS, na.rm = TRUE) * 100
  )

CCC_total <- data_comp %>%
  ungroup() %>%
  group_by(version) %>%
  yardstick::ccc(Mt.TLS, Mt.DS) %>%
  select(.estimate) %>%
  rename(CCC = 1)

total_stats <- bind_cols(total_stats, CCC_total) %>%
  mutate(type = "total") %>%
  relocate(type, .before = MRE.pct) %>%
  select(-type)

data <- total_stats %>%
  mutate_if(is.numeric, round, 3) %>%
  rename(
    "Mean Relative Error (%)" = MRE.pct,
    "RMSE (kg)" = RMSE.kg,
    "Relative RMSE (%)" = RRMSE.pct
  )

data %>%
  gt() %>%
  # tab_header(title = unique(.$`_data`$version)) %>%
  # cols_hide(version) %>%
  cols_label(
    version = "Method",
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
  summarise(
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
  summarise(
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
  summarise(
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
  summarise(
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
  summarise(
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
  summarise(
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
  filter(Version %in% c("SimpleForest", "Real Twig (SimpleForest)")) %>%
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

## ----echo=FALSE, warning=FALSE, message=FALSE---------------------------------
### TOTALS #####################################################################
total_stats <- all_biomass %>%
  group_by(Version) %>%
  summarise(
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
  summarise(
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
  summarise(
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
  tab_header(title = "SimpleForest") %>%
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
  tab_header(title = "Real Twig (SimpleForest)") %>%
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

