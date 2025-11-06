################################################################
# Script Name: save_plot_png()
# Author: Jessica Lin
# GitHub: JessicaLin92
# Date Created: Sep 29th
#
# Purpose: This script creates the function to save plot objects
#
################################################################

################################################################
# Note: When sourcing script files, if you do not want objects
# available in this script, use the source() function along with
# the local = TRUE argument. By default, source() will make
# objects available in the current environment.

################################################################
# Load necessary libraries/source any function directories
# Example:
#R.utils::sourceDirectory(here::here("src", "functions"))
library(ggplot2)

################################################################
# Writing the function
save_plot_png <- function(
    plot,
    filename,
    figs_dir,
    units = "px",
    width = 1600,
    height = 1100, 
    dpi = 300,
    ...
) {
  ggsave(filename = filename,
         plot = plot,
         device = "png",
         path = figs_dir,
         scale = 1,
         width = width,
         height = height,
         units = "px",
         dpi = dpi,
         limitsize = TRUE, 
         bg = NULL,
         create.dir = FALSE)
}

################################################################
#Test the function 
test <- ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

test

save_plot_png(
  plot = test,
  filename = "diamonds_scatter.png",
  figs_dir = here::here("figs"),
  width = 1600, height = 1100, dpi = 300, units = "px"
)

################################################################
################################################################
# End of script
