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
source(here::here("src", "functions", "cleaning_script_cps.R"))

# Plot saving helper
source(here::here("src", "functions", "save_plot_png.R"))

################################################################################
# Use cleaned data from cleaning_script_cps.R
# df_all_post_midterm has:
#   prtage, peeduca, ptdtrace (race factor: White, Black, etc.),
#   hrhtype, peafever (Veteran / Non-Veteran), pelaydur, ...
################################################################################

cps_for_plots <- df_all_post_midterm |>
  filter(!is.na(pelaydur), pelaydur >= 0)

################################################################################
# Plot 1: Unemployment Duration by Race (boxplot)
################################################################################

# Median per race (ptdtrace is already labeled factor from cleaning script)
race_medians <- cps_for_plots |>
  group_by(ptdtrace) |>
  summarise(med = median(pelaydur, na.rm = TRUE), .groups = "drop")

overall_median <- median(cps_for_plots$pelaydur, na.rm = TRUE)

unemp_race_plot <- cps_for_plots |>
  ggplot(aes(
    x    = ptdtrace,
    y    = pelaydur,
    fill = ptdtrace
  )) +
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  
  # labels printed next to boxes
  geom_text(
    data = race_medians,
    aes(
      x     = ptdtrace,
      y     = med * 1.10,
      label = ptdtrace
    ),
    inherit.aes = FALSE,
    color       = "black",
    hjust       = 0,
    size        = 4
  ) +
  
  # y-axis scaling
  scale_y_continuous(
    breaks = seq(0, 50, by = 5),
    limits = c(0, 50)
  ) +
  
  # overall median line
  annotate(
    "segment",
    x        = -Inf,
    xend     = Inf,
    y        = overall_median,
    yend     = overall_median,
    color    = "red",
    linewidth = 1,
    linetype = "dashed"
  ) +
  
  # text label for the median line
  annotate(
    geom  = "text",
    x     = Inf,
    y     = 5,  # manually chosen so it doesn't get cut off
    label = paste0("Median = ", round(overall_median, 1), " weeks"),
    hjust = 0.6,
    color = "red",
    size  = 4
  ) +
  
  labs(
    title = "Unemployment Duration by Race",
    x     = "Race",
    y     = "Unemployment Duration (weeks)",
    fill  = "Race"
  ) +
  theme_minimal() +
  theme(
    legend.position  = "none",
    axis.text.y      = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

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
    hjust = 1,
    color = "white",
    size  = 3.3
  ) +
  #creating small multiple plots
  facet_wrap(
    facets  = vars(peafever),
    labeller = labeller(
      peafever = c("Veteran" = "Veteran", "Non-Veteran" = "Non-Veteran")
    )
  ) +
  
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
    y     = "Mean Unemployment Duration (weeks)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "none",
    axis.text.y      = element_blank(),
    axis.title.y     = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    strip.text       = element_text(face = "bold", size = 14)
  )
print(unemp_race_vet_plot)
################################################################################
# Save plots
################################################################################

figs_dir <- here::here("figs")
dir.create(figs_dir, showWarnings = FALSE)

save_plot_png(
  plot    = unemp_race_plot,
  filename = "unemp_by_race.png",
  figs_dir = figs_dir,
  width    = 2600,
  height   = 2000,
  dpi      = 300,
  units    = "px"
)

save_plot_png(
  plot    = unemp_race_vet_plot,
  filename = "unemp_by_race_veteran.png",
  figs_dir = figs_dir,
  width    = 3000,
  height   = 1600,
  dpi      = 300,
  units    = "px"
)
