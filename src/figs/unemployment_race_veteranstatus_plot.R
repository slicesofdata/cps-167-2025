################################################################################
# Script Name: Finals plot
# Author: Jaimie Yu
#
# Purpose: Finals plots for unemployment by race and veteran status
################################################################################

################################################################################
# Load libraries and source helper scripts
################################################################################

library(tidyverse)
library(ggplot2)
library(here)

# Cleaning script: creates df_all_post_midterm and saves cps_all_post_midterm.rds
#source(here::here("src", "functions", "cleaning_script_cps.R"))

# Plot saving helper
source(here::here("src", "functions", "save_plot_png.R"))
df_all_post_midterm <- readRDS(
  here::here("data", "processed", "cps_all_post_midterm.rds")
)

################################################################################
# Use cleaned data from cleaning_script_cps.R
# df_all_post_midterm has:
#   prtage, peeduca, ptdtrace (race factor: White, Black, etc.),
#   hrhtype, peafever (Veteran / Non-Veteran), pelaydur, ...
################################################################################

cps_for_plots <- df_all_post_midterm |>
  filter(!is.na(pelaydur), pelaydur >= 0)

################################################################################
# Plot 2: Average Unemployment Duration by Race and Veteran Status (bar)
################################################################################

unemp_race_vet_bar_data <- df_all_post_midterm |>
  filter(
    !is.na(ptdtrace),
    !is.na(peafever),
    !is.na(pelaydur),
    pelaydur >= 0
  ) |>
  group_by(peafever, ptdtrace) |>
  summarise(
    mean_pelaydur = mean(pelaydur, na.rm = TRUE),
    .groups       = "drop"
  )

# median of the race means within each veteran status
median_data <- unemp_race_vet_bar_data |>
  group_by(peafever) |>
  summarise(
    median_value = median(mean_pelaydur, na.rm = TRUE),
    .groups      = "drop"
  )

unemp_race_vet_plot <- unemp_race_vet_bar_data |>
  ggplot(aes(
    x    = ptdtrace,
    y    = mean_pelaydur,
    fill = ptdtrace
  )) +
  geom_col(width = 0.5) +
  coord_flip() +
  
  # labels inside bars instead of legend
  geom_text(
    aes(
      label = ptdtrace,
      y     = mean_pelaydur * 0.95
    ),
    hjust = 1.5,
    color = "white",
    size  = 3.3
  ) +
  scale_fill_manual(
    name = "Race",
    values = c(
      "Asian" = "#3E53BD",
      "Black" = "#3E53BD",
      "Multi-racial" = "#3E53BD",
      "Native American" = "#3E53BD",
      "Pacific Islander" = "#3E53BD",
      "White" = "#3E53BD")
    ) +
  #creating small multiple plots
  facet_wrap(
    facets  = vars(peafever),
    labeller = labeller(
      peafever = c("Veteran" = "Veteran", "Non-Veteran" = "Non-Veteran")
    )
  ) +
  #increase the scalling so it's readable
  scale_y_continuous(
    breaks = seq(0, 18, by = 2),
    limits = c(0, 18)
  ) +
  
  # Median lines per facet
  geom_segment(
    data = median_data,
    aes(
      x    = 0.5,
      xend = length(levels(unemp_race_vet_bar_data$ptdtrace)) + 0.5,
      y    = median_value,
      yend = median_value
    ),
    inherit.aes = FALSE,
    color       = "red",
    linetype    = "dashed",
    linewidth   = 1
  ) +
  
  # Labels for median lines
  geom_text(
    data = median_data,
    aes(
      x     = length(levels(unemp_race_vet_bar_data$ptdtrace)) + 0.3,
      y     = median_value,
      label = paste0("Median = ", round(median_value, 1))
    ),
    inherit.aes = FALSE,
    color       = "red",
    angle       = 270,
    hjust = 0.1,
    vjust       = -0.7,
    size        = 4
  ) +
  
  labs(
    title = "Average Unemployment Duration by Race and Veteran Status",
    x     = "Race",
    y     = "Unemployment Duration (weeks)"
  ) +
  theme_minimal() +
  theme(
    legend.position  = "none",
    axis.text.y      = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

print(unemp_race_vet_plot)
################################################################################
# Save plots
################################################################################


save_plot_png(
  plot    = unemp_race_vet_plot,
  filename = "unemp_by_race_veteran.png",
  figs_dir = figs_dir,
  width    = 3000,
  height   = 1600,
  dpi      = 300,
  units    = "px"
)
