################################################################################
# Script Name: stag_ht_chamionship.R
# Author: Jaimie Yu
# GitHub: jaimieyu2753_web
# Date Created: 10/5/2025
#
# Purpose: This script makes the plot for Homeowork 4, from the champoinship 
# data:https://raw.githubusercontent.com/slicesofdata/dataviz25/main/figs/beavis_stag_ht_chamionship.png
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

library(dplyr)
library(ggplot2)
library(stringr)
################################################################################
# reading data
data <- readr::read_csv("https://raw.githubusercontent.com/slicesofdata/dataviz25/main/data/tfrrs/ht-cleaned.csv")


################################################################################
# cleaning data
set.seed(167)
Cleaned_data <- data |>
  filter(!is.na(Year)) |>
  filter(Team == "stag") |>
  filter(str_detect(Meet, "Championships")) |>
  group_by(Season, Year) |>
  mutate(median_mark = median(Mark))


################################################################################
# Plotting 
championship_plot <- ggplot(Cleaned_data, aes(x = Season, y = Mark, color = Year)) +
  scale_color_manual(values = c(
    "FR" = "green",
    "SO" = "purple",
    "JR" = "blue",
    "SR" = "red"
  )) +
  geom_point(
    size = 2,
    position = position_jitter(width = 0.5, height = 0, seed = 167),
    alpha = 0.6
  ) +
  geom_line(aes(y = median_mark, group = Year)) +
  geom_point(aes(y = median_mark, group = Year), size = 3, shape = 21, fill = "white") +
  theme_minimal() +
  labs(
    x = "Season",
    y = "Distance (meters)",
    color = "Year"
  )

  
################################################################################
# Saving Plot
save_plot_png(
  plot= championship_plot,
  filename = "championship_plot.png",
  figs_dir = here::here("figs"),
  width = 1600, 
  height = 1100, 
  dpi = 300, 
  units = "px"
)



################################################################################
# End of script
