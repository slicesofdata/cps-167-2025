################################################################################
# Script Name: DR_cleaning script
# Author: Daniella Reyes
# GitHub: 
# Date Created:
#
# Purpose: This script is for cleaning plots
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
#source(here::here("src", "functions", "load-libraries.R"))
library(dplyr)
library(tidyr)
library(forcats)

################################################################################
# load in data

cleaning_12.20_7.20 <- read_csv(file = here::here("data","raw", "12.20_7.20.csv"))
cleaning_1.21_7.21 <- read_csv(file = here::here("data","raw", "1.21_7.21.csv"))
cleaning_7.22_6.23 <- read_csv(file = here::here("data","raw", "7.22_6.23.csv"))
cleaning_7.23_7.24 <- read_csv(file = here::here("data","raw", "7.23_7.24.csv"))

################################################################################
# data
df_long <- cleaning_12.20_7.20 |>
  pivot_longer(
    cols = everything(),
    names_to = "month_year_variable",
    values_to = "response"
  )

long_12.20_7.20 <- df_long |>
  separate(month_year_variable,
           into = c("month", "year", "variable"),
           sep = "_",
           convert = TRUE)

df_long2 <- cleaning_1.21_7.21 |>
  pivot_longer(
    cols = everything(),
    names_to = "month_year_variable",
    values_to = "response"
  )

long_1.21_7.21 <- df_long2 |>
  separate(month_year_variable,
           into = c("month", "year", "variable"),
           sep = "_",
           convert = TRUE)

df_long3 <- cleaning_7.22_6.23 |>
  pivot_longer(
    cols = everything(),
    names_to = "month_year_variable",
    values_to = "response"
  )

long_7.22_6.23 <- df_long3 |>
  separate(month_year_variable,
           into = c("month", "year", "variable"),
           sep = "_",
           convert = TRUE)

df_long4 <- cleaning_7.23_7.24 |>
  pivot_longer(
    cols = everything(),
    names_to = "month_year_variable",
    values_to = "response"
  )

long_7.23_7.24 <- df_long4 |>
  separate(month_year_variable,
           into = c("month", "year", "variable"),
           sep = "_",
           convert = TRUE)

#check <- df_all |>
 # filter(variable== "ptdtrace")|>
  
  
  
  ################################################################################
# combining

df_all <- bind_rows(long_12.20_7.20, long_1.21_7.21, long_7.22_6.23, long_7.23_7.24)

df_all_post_midterm <- df_all |>
  filter(response != -1 & !is.na(response)) |>
  group_by(month, year, variable) |>
  mutate(obs_id = row_number()) |>
  ungroup() |>
  pivot_wider(
    names_from = variable,
    values_from = response
  ) |>
  mutate(
    peafever = factor(peafever, levels = c(1, 2), labels = c("Veteran", "Non-Veteran")),
    ptdtrace = fct_collapse(
      as.factor(ptdtrace),
      White = c("1"),
      Black = c("2"),
      `Native American` = c("3"),
      Asian = c("4"),
      `Pacific Islander` = c("5"),
      `Multi-racial` = as.character(6:26)
    ),
    edu = case_when(
      peeduca %in% 31:34 ~ "Less than High School",
      peeduca %in% 35:38 ~ "Some High School, No Diploma",
      peeduca == 39 ~ "High School Graduate",
      peeduca %in% 40:42 ~ "Some College or Associate Degree",
      peeduca == 43 ~ "Bachelor’s Degree",
      peeduca %in% 44:45 ~ "Master’s and Professional Degree",
      peeduca > 45 ~ "Doctorate degree"
    )
  )
saveRDS(df_all_post_midterm, file = here::here("data","processed","cps_all_post_midterm.rds"))

