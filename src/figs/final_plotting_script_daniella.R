################################################################################
# Script Name: DR_plotting_finals
# Author: Daniella Reyes
# GitHub: 
# Date Created:
#
# Purpose: Using our most recent changes, here are my plotting codes
#
################################################################################

################################################################################
# Load necessary libraries/source any function directories
# Example:
#R.utils::sourceDirectory(here::here("src", "functions"))
library(readr)
library(readxl)
library(tidyverse)
library(ggplot2)
################################################################################
# load in data
df_all <- readRDS(here::here("data", "processed", "cps_all_post_midterm.rds"))
################################################################################
# website graph 1 

web_graph_1 <- df_all |>
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


################################################################################
# website graph 3

#############
web_graph_track_n <- df_all |>
  mutate(
    peafever = as.character(peafever),
    peafever = replace_na(peafever, "Unknown")
  )|>
  mutate(
    year = year + 2000,
    peafever = replace_na(peafever, "Unknown")  # replace NAs with "Unknown"
  ) |>
  group_by(year, peafever) |>
  summarise(n = n(), .groups = "drop") |>
  ggplot(aes(x = year, y = n, color = peafever)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Veteran"     = "#1f78b4",
    "Non-Veteran" = "#33a02c",
    "Unknown"     = "#e31a1c"  
  )) +
  labs(
    title = "Count Across Years by Veteran Status",
    x = "Year",
    y = "Count",
    color = "Veteran Status"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(color = "grey40"),
    axis.title.x  = element_text(color = "grey40"),
    axis.title.y  = element_text(color = "grey40"),
    axis.text.x   = element_text(color = "grey40"),
    axis.text.y   = element_text(color = "grey40"),
    legend.title  = element_text(color = "grey40"),
    legend.text   = element_text(color = "grey40")
  )

new<- directlabels::direct.label(web_graph_track_n, "angled.boxes")
new


################################################################################
# website graph 4

# Prepare data
df_plot <- df_all |>
  filter(!is.na(ptdtrace) & !is.na(peafever)) |>
  mutate(
    race_group = if_else(tolower(ptdtrace) == "white", "White", "Non-White"),
    peafever = as.character(peafever) 
  ) |>
  unite(col = Race_veteran, race_group, peafever, sep = " ") |>
  mutate(
    year = year + 2000
  ) |>
  group_by(year, Race_veteran) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(Race_veteran)

custom_colors <- c(
  "White Non-Veteran" = "#08DD2F",
  "White Veteran" = "#1f78b4",
  "Non-White Non-Veteran" = "#33a02c",
  "Non-White Veteran" = "#3D3DFF"
)


df_labels <- df_plot |>
  group_by(Race_veteran) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    # Hard-coded y positions to avoid intersection
    y_label = case_when(
      Race_veteran == "White Veteran" ~ n + 38000,
      Race_veteran == "White Non-Veteran" ~ n + 90000,
      Race_veteran == "Non-White Veteran" ~ n + 16000,
      Race_veteran == "Non-White Non-Veteran" ~ n + 38000,
      TRUE ~ n
    )
  )

# Plot
plot12 <- ggplot(df_plot, aes(x = year, y = n, color = Race_veteran)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::comma,
                     breaks = c(100000, 200000, 300000, 400000, 500000,600000, 700000, 800000))+
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Annual Total Respondants Across Demographics",
    x = "Year",
    y = "Number of Respondants"
  ) +
  labs(color = "Veteran Status")+
  theme_minimal() +
  theme(
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title    = element_text(color = "grey40"),
    axis.title.x  = element_text(color = "grey40"),
    axis.title.y  = element_text(color = "grey40"),
    axis.text.x   = element_text(color = "grey40"),
    axis.text.y   = element_text(color = "grey40"),
    legend.title  = element_text(color = "grey40"),
    legend.text   = element_text(color = "grey40")
  )
plot12

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
#############

save_plot_png(
  plot = web_graph_1,
  file_name = "Unemployment_Duration Distribution_by_Veteran_Status.png",
  figs_dir = "src/figs",
  units = "px",
  width = 9000,
  height = 3000,
  dpi = 300 
)
#########
save_plot_png(
  plot = plot12,
  file_name = "Total_Respondants_Across_Year_Divded_by_Race_and_Veteran_Status.png",
  figs_dir = "src/figs",
  units = "px",
  width = 9000,
  height = 6000,
  dpi = 300 
)
#######
save_plot_png(
  plot = web_graph_avg,
  file_name = "Median_proportion_plot.png",
  figs_dir = "src/figs",
  units = "px",
  width = 2000,
  height = 1300,
  dpi = 300 
)
###############
save_plot_png(
  plot = new,
  file_name = "Total_Respondents_Across_Years.png",
  figs_dir = "src/figs",
  units = "px",
  width = 2000,
  height = 1300,
  dpi = 300 
)
################################################################################
# End of script
