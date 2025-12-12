################################################################################
# Script Name: unemployment duration vs Eudcation and race
# Author:Jaimie Yu
# GitHub:
# Date Created:12/1/25
#
# Purpose: This script plots the unemployment duration vs education and race
#
################################################################################

################################################################################
# Note: When sourcing script files, if you do not want objects
# available in this script, use the source() function along with
# the local = TRUE argument. By default, source() will make
# objects available in the current environment.

################################################################################
library(tidyverse)
library(ggplot2)
library(here)

#load in the processed data
df_all_post_midterm <- readRDS(
  here::here("data", "processed", "cps_all_post_midterm.rds")
)

# Plot saving helper
source(here::here("src", "functions", "save_plot_png.R"))

################################################################################
# Plot 3 — Unemployment Duration by Education and Veteran Status

# keep only rows with valid education, veteran status, and non-negative duration
cps_edu_vet <- df_all_post_midterm |>
  dplyr::filter(
    !is.na(edu),
    !is.na(peafever),
    !is.na(pelaydur),
    pelaydur >= 0
  )

edu_vet_plot <- cps_edu_vet |>
  ggplot(aes(
    x    = peafever,          # Veteran vs Non-Veteran
    y    = pelaydur,          # unemployment duration
    fill = peafever
  )) +
  geom_boxplot(
    width         = 0.3,
    outlier.alpha = 0.15,
    outlier.shape = NA
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(..y.., 1)),
    color = "black",
    vjust = -0.5,
    size = 3
  )+
  facet_wrap(
    vars(edu),                # one panel per education level
    ncol = 1,
    strip.position = "top"
  ) +
  labs(
    title    = "Unemployment Duration by Education and Veteran Status",
    subtitle = "CPS July 2019–July 2024",
    x        = "Veteran Status",
    y        = "Unemployment Duration (weeks)",
    fill     = "Veteran Status"
  ) +
  scale_fill_manual(
    name = "Veteran Status", 
    values = c(
      "Veteran" = "#1f78b4",        # blue
      "Non-Veteran" = "#33a02c"     # green
    )
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    strip.text       = element_text(face = "bold", size = 11),
    axis.text.x      = element_text(angle = 0),
    legend.position  = "bottom"
  ) +
  theme(legend.position = "none"
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, by = 10),
    limits = c(0, 50)
  ) 



edu_vet_plot

################################################################################
# save plot

save_plot_png(
  plot     = edu_vet_plot,
  filename = "unemp_edu_veteran.png",
  figs_dir = figs_dir,
  width    = 2000,
  height   = 3000,
  dpi      = 300,
  units    = "px"
)
