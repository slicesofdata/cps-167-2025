################################################################################
# Script Name: unemployment by race
# Author: Jaimie Yu
#
# Purpose: the plot for unemployment by race
################################################################################

################################################################################
# Load libraries and source helper scripts
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
    x     = 6.5,
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
print(unemp_race_plot)
################################################################################
# Save plots
################################################################################

figs_dir <- here::here("figs")
dir.create(figs_dir, showWarnings = FALSE)

save_plot_png(
  plot    = unemp_race_plot,
  filename = "unemp_by_race.png",
  figs_dir = figs_dir,
  width    = 3000,
  height   = 2600,
  dpi      = 300,
  units    = "px"
)
