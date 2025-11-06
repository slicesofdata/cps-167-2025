################################################################################
# Script Name: Data Wrangling
# Author:Jaimie Yu
# GitHub: CPS
# Date Created: 10/21/25
#
# Purpose: Thi script will include all step in which I wrangle the data to prepare for plots
#
################################################################################

################################################################################
# Note: When sourcing script files, if you do not want objects
# available in this script, use the source() function along with
# the local = TRUE argument. By default, source() will make
# objects available in the current environment.

################################################################################
# Load necessary libraries/source any function directories
# Example:
#R.utils::sourceDirectory(here::here("src", "functions"))
source(here::here("src", "functions", "load-libraries.R"))

################################################################################
# ----------------------------
# ==========================================================

library(tidyverse)
library(here)

# ----------------------------
# 1. LOAD RAW FILES
# ----------------------------
# ==========================================================
# 📊 CPS Visualization: Graph 2 & Graph 7
# ==========================================================
# Graph 2 — Family Income Recovery by Household Type
# Graph 7 — Unemployment Rate by Demographic Group Over Time
# ==========================================================

library(tidyverse)
library(here)

# ----------------------------
# 1. Load and Combine Data
# ----------------------------
cleaning_12.20_7.20 <- read_csv(here("data","raw","12.20_7.20.csv"))
cleaning_1.21_7.21  <- read_csv(here("data","raw","1.21_7.21.csv"))
cleaning_7.22_6.23  <- read_csv(here("data","raw","7.22_6.23.csv"))
cleaning_7.23_7.24  <- read_csv(here("data","raw","7.23_7.24.csv"))

make_long <- \(df) {
  df |>
    pivot_longer(cols = everything(),
                 names_to = "month_year_variable",
                 values_to = "response") |>
    separate(month_year_variable,
             into = c("month", "year", "variable"),
             sep = "_",
             convert = TRUE)
}

df_all <- bind_rows(
  make_long(cleaning_12.20_7.20),
  make_long(cleaning_1.21_7.21),
  make_long(cleaning_7.22_6.23),
  make_long(cleaning_7.23_7.24)
)

# ----------------------------
# 2. Filter, Reshape Wide
# ----------------------------
wide_data <- df_all |>
  filter(response != -1 & !is.na(response)) |>
  group_by(month, year, variable) |>
  mutate(obs_id = row_number()) |>
  ungroup() |>
  pivot_wider(names_from = variable, values_from = response)

# ----------------------------
# 3. Factorize Variables (no ifelse or case_when)
# ----------------------------

# Race: PTDTRACE (CPS categories)
wide_data$ptdtrace <- factor(
  wide_data$ptdtrace,
  levels = c(1, 2, 3, 4, 5, 6),
  labels = c("White", "Black", "American Indian/Alaska Native",
             "Asian", "Pacific Islander", "Multiracial")
)

# Veteran status: PEAFEVER (1 = Non-Veteran, 2 = Veteran)
wide_data$peafever <- factor(
  wide_data$peafever,
  levels = c(1, 2),
  labels = c("Non-Veteran", "Veteran")
)

# Household type: HRHTYPE
wide_data$hrhtype <- factor(
  wide_data$hrhtype,
  levels = c(1, 2, 3, 4),
  labels = c("Married-Couple Family",
             "Male Householder, No Wife",
             "Female Householder, No Husband",
             "Non-Family Household")
)

# ==========================================================
# GRAPH 2 — Average Unemployment Duration by Household Type
# ==========================================================
graph2_plot <- graph2_data |>
  ggplot(aes(x = factor(year), y = mean_duration, fill = hrhtype)) +
  geom_col(position = position_dodge(width = 0.8)) +
  labs(
    title = "Average Unemployment Duration by Household Type",
    subtitle = "Measured in weeks, grouped by year",
    x = "Year",
    y = "Mean Duration of Unemployment (weeks)",
    fill = "Household Type"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# ==========================================================
# GRAPH 7 — UNEMPLOYMENT RATE BY DEMOGRAPHIC GROUP OVER TIME
# ==========================================================

graph7_plot <- wide_data |>
  filter(!is.na(ptdtrace), !is.na(peafever), !is.na(pelaydur)) |>
  ggplot(aes(x = ptdtrace, y = pelaydur, fill = ptdtrace)) +
  geom_boxplot(outlier.alpha = 0.3, width = 0.7) +
  facet_wrap(~ peafever, drop = TRUE) +
  labs(
    title = "Distribution of Unemployment Duration by Race and Veteran Status",
    subtitle = "Boxplots show median and variability across groups",
    x = "Race",
    y = "Unemployment Duration (weeks)",
    fill = "Race"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = 1))


# ==========================================================
# 4. Display Plots
# ==========================================================
save_plot_png(
  plot = graph2_plot,
  file_name = "graph2_unemployment_duration_by_household.png",
  figs_dir = here::here("figs"),
  width = 5000/300,
  height = 1300/300,
  dpi = 300,
  units = "in"
)

save_plot_png(
  plot = graph7_plot,
  file_name = "graph7_unemployment_duration_by_race_veteran.png",
  figs_dir = here::here("figs"),
  width = 2000/300,
  height = 1100/300,
  dpi = 300,
  units = "in"
)


print(graph2_plot)


print(graph7_plot)
