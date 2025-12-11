################################################################################
# Script Name: Unemployment Duration Over Time by Education
# Author: Jaimie
# GitHub:
# Date Created: 12/6/25
#
# Purpose: This script plots the Unemployment Duration Over Time by Education
#
################################################################################

################################################################################

################################################################################
# loads libraries
library(tidyverse)
library(ggplot2)
library(here)

# Use cleaned data created by cleaning_script_cps.R
# df_all_post_midterm already has:
# month, year, pelaydur, edu, ptdtrace, peafever, etc.
df_all_post_midterm <- readRDS(
  here::here("data", "processed", "cps_all_post_midterm.rds")
)

################################################################################
# Minimal summarising for plotting: mean unemployment duration by month-year
################################################################################

edu_time <- df_all_post_midterm |>
  filter(!is.na(pelaydur), pelaydur >= 0, !is.na(edu)) |>
  group_by(year, month, edu) |>
  summarise(
    mean_pelaydur = mean(pelaydur, na.rm = TRUE),
    .groups       = "drop"
  ) |>
  mutate(
    # turn CPS year/month into a Date for the x-axis
    year_full = 2000 + year,
    date      = as.Date(paste(year_full, month, 1, sep = "-"))
  )

################################################################################
# plot
unemp_edu_time_plot <- edu_time |>
  ggplot(aes(x = date, y = mean_pelaydur)) +
  geom_line(linewidth = 0.8, color = "#3E53BD") +
  
  facet_wrap(
    vars(edu),        # use vars() as you asked
    ncol = 1,         # 1 column, 7 rows
    strip.position = "top"
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, by = 5),
    limits = c(0, 50)
  ) +
  
  labs(
    title = "Unemployment Duration Over Time by Education Level",
    subtitle = "Average unemployment duration (weeks) from 2019–2024 by education group",
    x = "Year",
    y = "Mean Unemployment Duration (weeks)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.background    = element_rect(fill = "white", color = NA),
    plot.background     = element_rect(fill = "white", color = NA),
    strip.text          = element_text(face = "bold", size = 12),
    panel.spacing.y     = unit(0.4, "lines"),
  )

print(unemp_edu_time_plot)


################################################################################
# save plot 


save_plot_png(
  plot    = unemp_edu_time_plot,
  filename = "unemp_edu_time_plot.png",
  figs_dir = figs_dir,
  width    = 2000,
  height   = 6000,
  dpi      = 300,
  units    = "px"
)

