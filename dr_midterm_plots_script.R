################################################################################
# Script Name: DR_Midterm_plots_script
# Author: Daniella Reyes
# GitHub: 
# Date Created:
#
# Purpose: This script is for the midterm presentation
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
library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
source(here::here("src", "functions", "load-libraries.R"))

################################################################################
# load in data
source("cleaning_script_cps.R")

df_all_post_midterm |>
  ggplot(mapping = aes(x= peafever, y= pelaydur, fill = peafever ))+
  geom_boxplot()+
  facet_wrap(~year) +
  theme_minimal()+
  labs(title = "Unemployment Duration by Veteran Status and Year",
       x = "Veteran status",
       y = "Unemployment duration (weeks)") +
  theme(
    axis.text.x = element_text(size = 8)
  )+
  scale_fill_discrete(
    name = "Veteran Status")

df_all_post_midterm |>
  mutate(
    peafever = factor(peafever, levels = c(1, 2), labels = c("Veteran", "Non-Veteran"))
  )|>
  mutate(race_group = if_else(ptdtrace == 1, "White", "Non-White"))|>
  mutate(race_or_veteran = paste(race_group, peafever, sep = "_"))|>
  ggplot(aes(x = pelaydur, fill = race_or_veteran)) +
  geom_histogram(position = "dodge", alpha = 1, bins = 5) +
  scale_y_continuous(breaks = seq(0, 8000, by = 500))+
  scale_x_continuous(breaks = seq(0, 60, by = 10))+
  scale_fill_discrete(
    name = "Group",  
    labels = c(
      "Non-White Non-Veteran",
      "Non-White Veteran",
      "White Non-Veteran",
      "White Veteran"
    )
  ) +
  labs(
    title = "Unemployment Duration By Race and Veteran Status",
    x = "Unemployment Duration (weeks)",
    y = "Count"
  ) +
  theme_minimal()
############################################
# saving function
save_plot_png <- function(plot,
                          file_name,
                          figs_dir,
                          units,
                          width,
                          height,
                          dpi){
  saving_path_file <- here::here(figs_dir,file_name)
  ggplot2::ggsave(
    filename=saving_path_file,
    plot = plot,
    width = 1600,
    height = 1100,
    units = units,
    dpi = dpi
  )
}

save_plot_png(
  plot = graph1_mid,
  file_name = "Unemployment Duration by Veteran Status.png",
  figs_dir = "src/figs",
  units = "px",
  width = 1900,
  height = 1100,
  dpi = 300 
)

save_plot_png(
  plot = graph2,
  file_name = "Unemployment Duration By Race and Veteran Status.png",
  figs_dir = "src/figs",
  units = "px",
  width = 1600,
  height = 1100,
  dpi = 300 
)


