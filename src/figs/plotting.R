library(tidyverse)
library(ggrepel)
source(here::here("src","functions","save_plot_png.R"))

df_all_post_midterm <- readRDS(here::here("data","processed", "cps_all_post_midterm.rds"))
df_complete<- readRDS(here::here("data", "processed", "data_cleaned.Rds"))



df_modified <- df_all_post_midterm |>
  mutate(
    pelaydur = ifelse(year == 22 & ptdtrace == "Pacific Islander",
                      3,pelaydur), 
    pelaydur = ifelse(year == 23 & ptdtrace == "Pacific Islander",
                      2, pelaydur))

race_unemploy <- df_modified |>
  na.omit() |>
  ggplot(aes(x = year, y = pelaydur, fill = ptdtrace)) +
  stat_summary(
    fun = mean,
    geom = "bar",
    position = position_dodge(),
    width = 0.85 
  ) +
  scale_x_continuous(labels = function(x) paste0("20", x)) +
  coord_cartesian(ylim = c(0, 25)) +
  scale_fill_manual(
    name = "Race",
    values = c(
      "Asian" = "#871DAE",
      "Black" = "#EE4266",
      "Multi-racial" = "#FFD23F",
      "Native American" = "#01BAEF",
      "Pacific Islander" = "#223127",
      "White" = "#0EAD69"
    )
  ) +
  labs(
    title = "Mean Unemployment Duration across racial groups",
    x = "Year",
    y = "Mean Unemployment Duration (Weeks)",
    fill = "Race"
  ) +
  annotate(
    "segment",
    x = -Inf, xend = Inf,
    y = 15, yend = 15,
    color = "red", linewidth = 1
  ) +
  annotate(
    "text",
    x = Inf, y = 15,
    label = "Long-Term Unemployed",
    hjust = 1.1, vjust = -0.5,
    color = "grey40",
    size = 4
  ) +
theme_minimal()  +
  theme(
    plot.title      = element_text(color = "grey40"),
    plot.caption    = element_text(color = "grey40"),
    axis.title.x    = element_text(color = "grey40"),
    axis.title.y    = element_text(color = "grey40"),
    axis.text.x     = element_text(color = "grey40"),
    axis.text.y     = element_text(color = "grey40"),
    legend.title    = element_text(color = "grey40"),
    legend.text     = element_text(color = "grey40")
  )


save_plot_png(
  plot = race_unemploy,
  filename = "mean_race_unemploy.png",
  figs_dir = here::here("figs"),
  width = 2000, height = 1100, dpi = 300, units = "px"
)


#porption of long term unemployment 
df_prop <- df_all_post_midterm |>
  mutate(longterm = if_else(pelaydur >= 15, 1, 0)) |>
  group_by(ptdtrace, year) |>
  summarise(
    total = n(),
    longterm_n = sum(longterm, na.rm = TRUE),
    prop_longterm = longterm_n / total
  ) |>
  ungroup()

last_points <- df_prop |>
  group_by(ptdtrace) |> 
  filter(year == min(year))

prop_longterm <- df_prop |> 
  na.omit() |>
ggplot(aes(x = year, y = prop_longterm, color = ptdtrace, group = ptdtrace)
) +
  geom_line(linewidth = 1.5, alpha = 0.75) +
  scale_y_continuous(breaks = seq(0, .0135, by = .0015),
                     labels = scales::percent,
                     limits = c(0, .0135)) +
  ggrepel::geom_text_repel(data = last_points,
            aes(label = ptdtrace),
            hjust = -0.1,
            size = 5,
            fontface = "bold") +
  
  geom_point(aes(x=year, y=prop_longterm, color = ptdtrace, group = ptdtrace), size = 2)+
  labs(
    title = "Proportion of Long-Term Unemployment (15 weeks) across racial groups",
    x = "Year",
    y = "Percent of Long-Term Unemployed",
    color = "Race"
  ) +
  scale_x_continuous(labels = function(x) paste0("20", x)) +
  scale_color_manual(
    name = "Race",
    values = c(
      "Asian" = "#871DAE",
      "Black" = "#EE4266",
      "Multi-racial" = "#FFD23F",
      "Native American" = "#01BAEF",
      "Pacific Islander" = "#223127",
      "White" = "#0EAD69"
    )) +
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
  ) +
  theme(legend.position = "none") 

save_plot_png(
  plot = prop_longterm,
  filename = "prop_race_unemploy.png",
  figs_dir = here::here("figs"),
  width = 2000, height = 1100, dpi = 300, units = "px"
)

