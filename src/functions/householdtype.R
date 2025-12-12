
library(tidyverse)   # includes ggplot2, dplyr, readr, forcats, etc.
library(ggplot2)
library(forcats)
df_all_post_midterm <- read_rds("data/processed/cps_all_post_midterm.rds")
source(here::here("src","functions","save_plot_png.R"))


df_all_post_midterm |> 
  count(hrhtype) |> 
  mutate(
    pct = n / sum(n),
    hrhtype = fct_reorder(hrhtype, pct)
  ) |> 
  ggplot(aes(x = pct, y = hrhtype)) +
  geom_col(fill = "#1F78B4") +
  
  geom_label(
    aes(label = hrhtype),
    hjust = 0,                 
    x = 0.001,                 
    label.size = NA,           
    fill = "white",            
    alpha = 0.8,               
    size = 3.5
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of Household Types in CPS Sample",
    x = "Share of Observations",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


df_all_post_midterm |> 
  filter(pelaydur > 0, pelaydur <= 52) |> 
  group_by(hrhtype) |> 
  summarise(avg_weeks = mean(pelaydur, na.rm = TRUE), .groups = "drop") |> 
  mutate(hrhtype = fct_reorder(hrhtype, avg_weeks)) |> 
  ggplot(aes(x = avg_weeks, y = hrhtype)) +
  geom_col(fill = "#33A02C") +
  
  geom_label(
    aes(label = hrhtype),
    hjust = 0,                 
    x = 0.05,                  
    label.size = NA,          
    fill = "white",            
    alpha = 0.8,
    size = 3.5
  ) +
  
  labs(
    title = "Average Unemployment Duration by Household Type",
    x = "Average Weeks Unemployed",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )


df_all_post_midterm |> 
  filter(pelaydur > 0, pelaydur <= 52) |> 
  group_by(hrhtype, peafever) |> 
  summarise(avg_weeks = mean(pelaydur, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(x = avg_weeks, y = fct_reorder(hrhtype, avg_weeks), fill = peafever)) +
  geom_col(position = "dodge") +
  labs(
    title = "Unemployment Duration by Household Type + Veteran Status",
    x = "Average Weeks Unemployed",
    y = "Household Type",
    fill = "Veteran Status"
  ) +
  theme_minimal()

df_all_post_midterm |> 
  filter(pelaydur > 0, pelaydur <= 52) |> 
  group_by(hrhtype, ptdtrace) |> 
  summarise(avg_weeks = mean(pelaydur, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(
    x = avg_weeks,
    y = fct_reorder(hrhtype, avg_weeks)
  )) +
  geom_point(color = "#1F78B4", size = 3) +
  facet_wrap(~ ptdtrace, ncol = 3) +
  labs(
    title = "Unemployment Duration by Household Type, Split by Race",
    x = "Average Weeks Unemployed",
    y = "Household Type"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face="bold"),
    axis.text.y = element_text(size=8)
  )


df_all_post_midterm |> 
  filter(pelaydur > 0, pelaydur <= 52) |> 
  group_by(hrhtype, edu) |> 
  summarise(avg_weeks = mean(pelaydur, na.rm = TRUE), .groups = "drop") |> 
  ggplot(aes(
    x = avg_weeks, 
    y = fct_reorder(hrhtype, avg_weeks),
    fill = edu
  )) +
  geom_col(position = "stack") +
  labs(
    title = "Education Composition of Unemployment Duration by Household Type",
    x = "Combined Avg Weeks Unemployed",
    y = "Household Type",
    fill = "Education Level"
  ) +
  theme_minimal()


df_all_post_midterm |>
  filter(pelaydur > 0, pelaydur <= 52) |> 
  ggplot(aes(
    x = fct_reorder(hrhtype, pelaydur, .fun = median),
    y = pelaydur
  )) +
  geom_boxplot(fill = "#FDBF6F") +
  coord_flip() +
  labs(
    title = "Distribution of Unemployment Duration by Household Type",
    x = "Household Type",
    y = "Weeks Unemployed (0–52)"
  ) +
  theme_minimal()

