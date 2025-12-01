################################################################################
# Script Name:
# Author:
# GitHub:
# Date Created:
#
# Purpose: This script ...
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


library(ggplot2)
library(dplyr)

################################################################################
# mpg data
#data <- mpg |>
#  ggplot(mapping = aes(x= displ, y = hwy))
  
data

################################################################################
# modify plot
better_mpg <- 
  mpg |>
  ggplot(mapping = aes(x = displ, y = hwy)) +
  geom_point(position = position_jitter())


################################################################################
# ...

################################################################################
# End of script
