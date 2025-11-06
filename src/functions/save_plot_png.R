################################################################################
# Script Name: Plot saving Function
# Author: Jaimie Yu
# GitHub: jaimieyu2753_web
# Date Created: 9/27/25
#
# Purpose: This script has the function that saves plots
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
#source(here::here("src", "functions", "load-libraries.R"))

################################################################################
# Load libraries
library(ggplot2)
library(dplyr)


################################################################################
# Building the funciton
save_plot_png <- function(
    plot,
    file_name,
    figs_dir,
    units = "px",
    width = 1600,
    height = 1100,
    dpi = 300
) {
  ggsave(
    filename = file.path(figs_dir, file_name),
    plot=plot,
    device = "png",
    path = figs_dir,
    scale = 1,
    width = width,
    height = height,
    units = c("in", "cm", "mm", "px"),
    dpi = 300,
    limitsize = TRUE,
    bg = NULL,
    create.dir = FALSE
  )
}


################################################################################
# Testing the function
test <- ggplot(diamonds, aes(x=cut, y= price))+
  geom_point()

test

save_plot_png(
  plot = test,
  file_name = "diamonds_scatter.png",
  figs_dir = here::here("report", "figs"),
  width = 10,
  height = 20,
  dpi = 300,
  units = "px"
)




################################################################################
# End of script
