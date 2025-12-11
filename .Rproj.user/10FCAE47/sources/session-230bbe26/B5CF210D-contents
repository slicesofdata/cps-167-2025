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
library(tidyverse)
library(ggplot2)
source(here::here("src", "functions", "load-libraries.R"))

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

check <- df_all |>
filter(variable== "ptdtrace")



################################################################################
# combining

df_all <- bind_rows(long_12.20_7.20, long_1.21_7.21, long_7.22_6.23, long_7.23_7.24)

df_all_post_midterm <- df_all |>
  filter(response != -1 & !is.na(response))|>
  group_by(month, year, variable) |>
  mutate(obs_id = row_number()) |>
  ungroup()|>
  pivot_wider(
    names_from = variable,
    values_from = response 
  )|>
  mutate(
    peafever = factor(peafever, levels = c(1, 2), labels = c("Veteran", "Non-Veteran"))
  )|>
  mutate(
    ptdtrace = fct_collapse(
      as.factor(ptdtrace),
      White = c("1"),
      Black = c("2"),
      `Native American` = c("3"),  # example
      Asian = c("4"),
      `Hawaiian/Pacific Islander` = c("5"),
      `Mixed Race` = c("6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26")
    ))
saveRDS(df_all_post_midterm, file = here::here("data","processed","df_all_cps.rds"))
################################################################################
# combining
 df_all |>
  filter(variable %in% c("pelaydur", "peafever")) |>
  filter(response != -1 & !is.na(response))|>
  group_by(month, year, variable) |>
  mutate(obs_id = row_number()) |>
  ungroup()|>
  pivot_wider(
    names_from = variable,
    values_from = response 
  )|>
  mutate(
    peafever = factor(peafever, levels = c(1, 2), labels = c("Veteran", "Non-Veteran"))
  )|>
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

df_all |>
  filter(response != -1 & !is.na(response))|>
  group_by(month, year, variable) |>
  mutate(obs_id = row_number()) |>
  ungroup()|>
  pivot_wider(
    names_from = variable,
    values_from = response 
  )|>
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

m

################################################################################
# website graph 1 

web_graph_1 <- df_all |>
  filter(response != -1 & !is.na(response))|>
  group_by(month, year, variable) |>
  mutate(obs_id = row_number()) |>
  ungroup()|>
  pivot_wider(
    names_from = variable,
    values_from = response 
  )|>
  mutate(
    peafever = factor(peafever, levels = c(1, 2), labels = c("Veteran", "Non-Veteran"))
  )|>
  filter(peafever %in% c("Veteran", "Non-Veteran"))|>
  ggplot(aes(x = peafever , y = pelaydur, fill = peafever)) +
geom_violin()+
  labs(
    title = "Unemployment Duration Density Distribution",
    x = "Veteran Status",
    y = "Unemployment Duration (weeks)"
  ) +
 scale_y_continuous(breaks = seq(0, 50, by = 5))+
  coord_flip()+
  scale_fill_manual(
    values = c(
      "Veteran" = "#1f78b4",        # blue
      "Non-Veteran" = "#33a02c"     # green
    ))+
  theme_minimal() +
  theme(
    plot.title      = element_text(color = "grey40"),
    plot.subtitle   = element_text(color = "grey40"),
    plot.caption    = element_text(color = "grey40"),
    axis.title.x    = element_text(color = "grey40"),
    axis.title.y    = element_text(color = "grey40"),
    axis.text.x     = element_text(color = "grey40"),
    axis.text.y     = element_text(color = "grey40"),
    legend.title    = element_text(color = "grey40"),
    legend.text     = element_text(color = "grey40")
  )+
  theme(legend.position = "none")

web_graph_1
 
################################################################################
# website graph 2
web_graph_avg <- df_all |>
  filter(response != -1 & !is.na(response))|>
  group_by(month, year, variable) |>
  mutate(obs_id = row_number()) |>
  ungroup()|>
  pivot_wider(
    names_from = variable,
    values_from = response 
  )|>
  mutate(
    peafever = factor(peafever, levels = c(1, 2), labels = c("Veteran", "Non-Veteran"))
  )|>
  mutate(
    year = year + 2000)|>
  filter(peafever %in% c("Veteran", "Non-Veteran"))|>
  filter(pelaydur == 9)|>
 ggplot(mapping = aes(fill= peafever, x= year, y= pelaydur))+
  geom_bar(position="fill", stat = "identity")+
scale_fill_manual(
  name = "Veteran Status", 
  values = c(
    "Veteran" = "#1f78b4",        # blue
    "Non-Veteran" = "#33a02c"     # green
  )
)+
  labs(
    title = "Proportion of People Unemployed for 9 Weeks, 2020-2024",
    x = "Year",
    y = "Proportion"
  ) +
  theme_minimal()+
  theme(
    plot.title      = element_text(color = "grey40"),
    plot.subtitle   = element_text(color = "grey40"),
    plot.caption    = element_text(color = "grey40"),
    axis.title.x    = element_text(color = "grey40"),
    axis.title.y    = element_text(color = "grey40"),
    axis.text.x     = element_text(color = "grey40"),
    axis.text.y     = element_text(color = "grey40"),
    legend.title    = element_text(color = "grey40"),
    legend.text     = element_text(color = "grey40")
  )+
  coord_flip()
  
web_graph_avg


######################## finding mean
df_all_post_midterm|>
  class(pelaydur)

head(df_all_post_midterm)

mean(df_all_post_midterm$pelaydur, na.rm = TRUE)
filtered <- df_all_post_midterm |>
  filter(!is.na(pelaydur))

median(filtered$pelaydur)
################################################################################
# saving
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
  width = 3000,
  height = 2600,
  dpi = 300 
)

save_plot_png(
  plot = web_graph_1,
  file_name = "Unemployment_Duration Distribution_by_Veteran_Status.png",
  figs_dir = "src/figs",
  units = "px",
  width = 9000,
  height = 3000,
  dpi = 300 
)

save_plot_png(
  plot = web_graph_avg,
  file_name = "Median_proportion_plot.png",
  figs_dir = "src/figs",
  units = "px",
  width = 2000,
  height = 1300,
  dpi = 300 
)
################################################################################
# End of script



cleannn <- df_all |>
  filter(response != -1 & !is.na(response))|>
  group_by(month, year, variable) |>
  mutate(obs_id = row_number()) |>
  ungroup()|>
  pivot_wider(
    names_from = variable,
    values_from = response 
  )|>
  mutate(
    peafever = factor(peafever, levels = c(1, 2), labels = c("Veteran", "Non-Veteran"))
  )
