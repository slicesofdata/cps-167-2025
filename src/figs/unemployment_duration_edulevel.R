################################################################################
# Script Name: Unemployment duration vs education
# Author: Jaimie Yu
# GitHub:
# Date Created:12/6/25
#
# Purpose: This script plots the unemployment duration vs education
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

# Load processed CPS data
df_all_post_midterm <- readRDS(
  here::here("data", "processed", "cps_all_post_midterm.rds")
)
# Plot saving helper
source(here::here("src", "functions", "save_plot_png.R"))

################################################################################
# for this plot only
cps_edu <- df_all_post_midterm |>
  filter(!is.na(pelaydur), pelaydur >= 0, !is.na(edu)) 
# NOTE: no fct_relevel() here

# Median by education group
edu_medians <- cps_edu |>
  group_by(edu) |>
  summarise(med = median(pelaydur, na.rm = TRUE), .groups = "drop")

# Overall median
overall_median <- median(cps_edu$pelaydur, na.rm = TRUE)


################################################################################
# plotting
unemp_edu_plot <- cps_edu |>
  ggplot(aes(
    x    = edu,
    y    = pelaydur,
  )) +
  
  geom_boxplot(outlier.shape = NA) +
  coord_flip() +
  geom_boxplot(fill = "#3E53BD", outlier.shape = NA
               ) +
  # --- LABELS INSIDE BOXES ---
  geom_text(
    data = edu_medians,
    aes(
      x     = edu,
      y     = 12,    
      label = edu
    ),
    inherit.aes = FALSE,
    color = "white",
    size  = 4,
  ) +
  
  # Overall median line
  annotate(
    "segment",
    x     = -Inf,
    xend  = Inf,
    y     = overall_median,
    yend  = overall_median,
    color = "red",
    linewidth = 1,
    linetype = "dashed"
  ) +
  scale_y_continuous(
    breaks = seq(0, 55, by = 5),
    limits = c(0, 55)
  ) +
  # text label for the median line
  annotate(
    geom  = "text",
    x     = 7.54,
    y     = 7,  # manually chosen so it doesn't get cut off
    label = paste0("Median = ", round(overall_median, 1), " weeks"),
    hjust = 0.6,
    color = "red",
    size  = 4
  ) +
  
  labs(
    title = "Unemployment Duration by Education Level",
    x = "Education Level",
    y = "Unemployment Duration (weeks)"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position  = "none",
    axis.text.y      = element_blank(),  # remove because labels are inside
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.title      = element_text(color = "grey40"),
    plot.subtitle   = element_text(color = "grey40"),
    plot.caption    = element_text(color = "grey40"),
    axis.title.x    = element_text(color = "grey40"),
    axis.title.y    = element_text(color = "grey40"),
    axis.text.x     = element_text(color = "grey40"),
    axis.text.y     = element_text(color = "grey40"),
    legend.title    = element_text(color = "grey40"),
    legend.text     = element_text(color = "grey40")
  )



################################################################################
# Save plot

save_plot_png(
  plot    = unemp_edu_plot,
  filename = "unemp_by_edu.png",
  figs_dir = figs_dir,
  width    = 3000,
  height   = 2600,
  dpi      = 300,
  units    = "px"
)
