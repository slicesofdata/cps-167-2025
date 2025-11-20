################################################################################
# Script Name: Midterm presentation graphs
# Author: Jessica Lin
# Date Created:
#
# Purpose: This script creates the two plots needed for the midterm presentation 
#
################################################################################
library(tidyverse)
source(here::here("src","functions","save_plot_png.R"))

################################################################################
cleaning_12.20_7.20 <- read_csv(file = here::here("data","raw", "12.20_7.20.csv"))
cleaning_1.21_7.21 <- read_csv(file = here::here("data","raw", "1.21_7.21.csv"))
cleaning_7.22_6.23 <- read_csv(file = here::here("data","raw", "7.22_6.23.csv"))
cleaning_7.23_7.24 <- read_csv(file = here::here("data","raw", "7.23_7.24.csv"))

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

df_all <- bind_rows(long_12.20_7.20, long_1.21_7.21, long_7.22_6.23, long_7.23_7.24)

#############################################
df_all <- df_all |>
  filter(response != -1 & !is.na(response))|>
  group_by(month, year, variable) |>
  mutate(obs_id = row_number()) |>
  ungroup()|>
  pivot_wider(
    names_from = variable,
    values_from = response)

df_all <- df_all |> 
  select(year, ptdtrace, pelaydur) |> 
  mutate(race = case_when(
    ptdtrace == 1 ~ "White",
    ptdtrace == 2 ~ "Black",
    ptdtrace == 3 ~ "Native American",
    ptdtrace == 4 ~ "Asian",
    ptdtrace == 5 ~ "Pacific Islander",
    ptdtrace > 5~ "Multiracial"
  )) 

df_all <- df_all |> 
  mutate(year = paste0("20", year))


saveRDS(object = df_all, 
        file = here::here("data", "processed", "cps_all_post_midterm.rds")
)

race_unemploy <- df_all |>
  na.omit() |>
  ggplot(aes(x = year, y = pelaydur, fill = race)) +
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge()
  ) +
  coord_cartesian(ylim = c(0, 25)) +
  labs (title = "Mean Unemployment Duration by Racial Groups",
        x = "Year", 
        y = "Mean Unemployment Duration (Weeks)",
        fill = "Race")+
  theme_minimal()  +
  annotate("segment",
           x = -Inf, xend = Inf,
           y = 15,  yend = 15,
           color = "red", linewidth = 1) + 
  annotate("text",
           x = Inf, y = 15,
           label = "Long-Term Unemployed",
           hjust = 1.1, vjust = -0.5,
           size = 4)

save_plot_png(
  plot = race_unemploy,
  filename = "mean_race_unemploy.png",
  figs_dir = here::here("figs"),
  width = 2000, height = 1100, dpi = 300, units = "px"
)

################################################################################

graph2 <- df_all |>
  select(peeduca, pelaydur, year) |>
  mutate(edu = case_when(
      peeduca %in% 31:34 ~ "Less than High School",
      peeduca %in% 35:38 ~ "Some High School, No Diploma",
      peeduca == 39 ~ "High School Graduate",
      peeduca %in% 40:42 ~ "Some College or Associate Degree",
      peeduca == 43 ~ "Bachelor’s Degree",
      peeduca == 44 | peeduca == 45 ~ "Master’s and Professional Degree",
      peeduca > 45 ~ "Doctorate degree"))


graph2 |>
  na.omit() |>
  ggplot(aes(x = year, y = pelaydur, fill = edu)) +
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge()
  ) +
  coord_cartesian(ylim = c(0, 25)) +
  scale_y_continuous(breaks = seq(0, 25, by = 5)) +
  labs (title = "Average Unemployment Duration by Educational Background",
        x = "Year", 
        y = "Avg Unemployment Duration (Weeks)",
        fill = "Education")+
  theme_minimal() +
  theme (plot.margin = unit(c(1, 2, 1, 2), "cm"))



################################################################################
# End of script
