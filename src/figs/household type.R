---
title: "housholdtype"
editor: visual
---
library(tidyverse)   # includes ggplot2, dplyr, readr, forcats, etc.
library(ggplot2)
library(forcats)
df_all_post_midterm <- read_rds("data/processed/cps_all_post_midterm.rds")
source(here::here("src","functions","save_plot_png.R"))

plot1 <- df_all_post_midterm |>
  mutate(hrhtype = factor(hrhtype)) |>
  count(hrhtype) |>
  mutate(
    pct = n / sum(n),
    hrhtype = fct_reorder(hrhtype, pct)
  ) |>
  ggplot(aes(x = pct, y = hrhtype)) +
  geom_col(fill = "#1F78B4") +
  geom_text(
    aes(label = scales::percent(pct, accuracy = 0.1)),
    hjust = -0.1,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = scales::percent_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Distribution of Household Types in CPS Sample",
    x = "Share of Observations",
    y = NULL
  ) +
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
  )
  

plot2 <- df_all_post_midterm |>
  filter(pelaydur > 0, pelaydur <= 52) |>
  mutate(hrhtype = factor(hrhtype)) |>
  group_by(hrhtype) |>
  summarise(
    avg_weeks = mean(pelaydur, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(hrhtype = fct_reorder(hrhtype, avg_weeks)) |>
  ggplot(aes(x = avg_weeks, y = hrhtype)) +
  geom_col(fill = "#33A02C") +
  labs(
    title = "Mean Unemployment Duration by Household Type",
    x = "Mean Weeks Unemployed",
    y = NULL
  ) +
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
  )


vetplot3 <- df_all_post_midterm |>
  filter(pelaydur > 0, pelaydur <= 52) |>
  mutate(
    hrhtype = factor(hrhtype),
    peafever = factor(peafever)
  ) |>
  group_by(hrhtype, peafever) |>
  summarise(
    avg_weeks = mean(pelaydur, na.rm = TRUE),
    .groups = "drop"
  ) |>
  ggplot(
    aes(
      x = avg_weeks,
      y = fct_reorder(hrhtype, avg_weeks),
      fill = peafever
    )
  ) +
  geom_col(position = "dodge") +
  labs(
    title = "Unemployment Duration by Household Type + Veteran Status",
    x = "Average Weeks Unemployed",
    y = "Household Type",
    fill = "Veteran Status"
  ) +
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
  )
  
raceplot4 <- df_all_post_midterm |>
  filter(pelaydur > 0, pelaydur <= 52) |>
  mutate(
    hrhtype  = haven::as_factor(hrhtype),
    ptdtrace = haven::as_factor(ptdtrace)
  ) |>
  group_by(hrhtype, ptdtrace) |>
  summarise(avg_weeks = mean(pelaydur, na.rm = TRUE), .groups = "drop") |>
  group_by(hrhtype) |>
  mutate(overall_avg = mean(avg_weeks)) |>
  ungroup() |>
  ggplot(aes(
    x = avg_weeks,
    y = fct_reorder(hrhtype, overall_avg),
    color = ptdtrace
  )) +
  geom_point(size = 3) +
  facet_wrap(~ ptdtrace, ncol = 3) +
  scale_color_manual(
    values = c(
      "Asian" = "#871DAE",
      "Black" = "#EE4266",
      "Multi-racial" = "#FFD23F",
      "Native American" = "#01BAEF",
      "Pacific Islander" = "#223127",
      "White" = "#0EAD69"
    ),
    guide = "none"
  ) +
  labs(
    title = "Unemployment Duration by Household Type, by Race",
    x = "Average Weeks Unemployed",
    y = "Household Type"
  ) +
  theme_minimal()


eduplo5 <- df_all_post_midterm |>
  filter(pelaydur > 0, pelaydur <= 52) |>
  mutate(
    hrhtype = haven::as_factor(hrhtype),
    edu     = haven::as_factor(edu)
  ) |>
  count(hrhtype, edu) |>
  group_by(hrhtype) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(
    x = pct,
    y = hrhtype,
    fill = edu
  )) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Education Composition of the Unemployed by Household Type",
    x = "Share of Unemployed Individuals",
    y = "Household Type",
    fill = "Education Level"
  ) +
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
  )
  
  
plot6 <- df_all_post_midterm |>
  filter(pelaydur > 0, pelaydur <= 52) |>
  mutate(hrhtype = haven::as_factor(hrhtype)) |>
  ggplot(aes(
    x = fct_reorder(hrhtype, pelaydur, .fun = median),
    y = pelaydur
  )) +
  geom_boxplot(fill = "#FDBF6F", outlier.alpha = 0.3) +
  coord_flip() +
  labs(
    title = "Distribution of Unemployment Duration by Household Type",
    x = "Household Type",
    y = "Weeks Unemployed (0–52)"
  ) +
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
  )
  
save_plot_png(
  plot = plot1,
  filename = "plot1.png",
  figs_dir = here::here("figs"),
  width = 2000, height = 1100, dpi = 300, units = "px"
)

save_plot_png(
  plot = plot2,
  filename = "plot2.png",
  figs_dir = here::here("figs"),
  width = 2000, height = 1100, dpi = 300, units = "px"
)

save_plot_png(
  plot = vetplot3,
  filename = "plot3.png",
  figs_dir = here::here("figs"),
  width = 2000, height = 1100, dpi = 300, units = "px"
)


save_plot_png(
  plot = raceplot4,
  filename = "plot4.png",
  figs_dir = here::here("figs"),
  width = 2000, height = 1100, dpi = 300, units = "px"
)

save_plot_png(
  plot = eduplo5,
  filename = "plot5.png",
  figs_dir = here::here("figs"),
  width = 2000, height = 1100, dpi = 300, units = "px"
)


save_plot_png(
  plot = plot6,
  filename = "plot6.png",
  figs_dir = here::here("figs"),
  width = 2000, height = 1100, dpi = 300, units = "px"
)