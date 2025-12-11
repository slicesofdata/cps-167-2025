################################################################################
# Script Name: DR_Midterm_plots
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
source(here::here("src", "functions", "load-libraries.R"))

################################################################################
# load in data

cleaning_12.20_7.20 <- read_csv(file = here::here("data","raw", "12.20_7.20.csv"))
cleaning_1.21_7.21 <- read_csv(file = here::here("data","raw", "1.21_7.21.csv"))
cleaning_7.22_6.23 <- read_csv(file = here::here("data","raw", "7.22_6.23.csv"))
cleaning_7.23_7.24 <- read_csv(file = here::here("data","raw", "7.23_7.24.csv"))

################################################################################
# m



################################################################################
# ...

################################################################################
# End of script
