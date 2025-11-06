################################################################################
# Script Name: Jessica_stag_ht_championship.R
# Author: Jessica Lin
# GitHub: JessicaLin92
# Date Created: Oct 5 
#
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
library(ggplot2)
library(dplyr)
library(stringr)
library(here)


source(here::here("src","figs","save_plot_png.R"))
################################################################################
# Reading the data

data <- readr::read_csv("https://raw.githubusercontent.com/slicesofdata/dataviz25/main/data/tfrrs/ht-cleaned.csv")
################################################################################
# Cleaning the data and constructing the plot 

set.seed (167)
CLeaned_data <- data |>
  filter(!is.na(Year)) |>
  filter(Team == "Stag") |>
  mutate(Meet = tolower(Meet)) |> 
  filter(str_detect(Meet, "championship")) |> 
  group_by(Season, Year) |>
  mutate(median_mark = median(Mark))
  
championship_plot <- ggplot(CLeaned_data, aes(x=Season, y=Mark, color = Year)) +
  scale_color_manual(values = c(
    "FR" = "green", 
    "JR" = "blue", 
    "SO" = "purple",
    "SR" = "red"        
  )) +
  geom_point(size = 2, position = position_jitter(width = 0.5, height = 0, seed = 167), alpha = 0.6) +
  geom_line(aes(x=Season, y= median_mark, group = Year)) +
  geom_point(aes(x=Season, y= median_mark, group = Year)) +
  theme_minimal() +
    labs(x = "Season",
      y = "Distance (meters)")

championship_plot

################################################################################
# Saving the plot as an PNG
save_plot_png(
  plot = championship_plot,
  filename = "championship_plot.png",
  figs_dir = here::here("figs"),
  width = 1600, height = 1100, dpi = 300, units = "px"
)

################################################################################
# End of script
