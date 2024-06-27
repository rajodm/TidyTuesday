# Libraries -------------------------------------------------------------------------

library(tidyverse)
library(waffle)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)


# Data ------------------------------------------------------------------------------

tt <- tidytuesdayR::tt_load("2024", week = 27)
tt_datasets <- tt$tt_datasets


skimr::skim(tt_datasets$variables)
skimr::skim(tt_datasets$observations)


plot_df <- tt_datasets |>
  group_by(year, week) |>
  mutate(
    obs_num = cut(
      observations,
      breaks = c(0, 200, 2000, 20000, 3771792),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("< 200", "200 to 1999", "2k to 19999", "≥ 20k")
    ),
    var_num = cut(
      variables,
      breaks = c(0, 5, 10, 15, 130),
      include.lowest = TRUE,
      right = FALSE,
      labels = c("< 5", "5 to 9", "10 to 14", "≥ 15")
    )
  )

# dataframe about the number of observations in each dataset
obs_df <- plot_df |>
  group_by(year) |>
  count(obs_num)

# dataframe about the number of variables in each dataset
var_df <- plot_df |>
  group_by(year) |>
  count(var_num)

# Fonts and colors ---------------------------------------------------------------------

showtext_auto()

# Space Grotesk
font_add_google("Space Grotesk", family = "spaceG")
font_plt <- "spaceG"


# Font Awesome
font_add("fa6-brands", "w27/fonts/Font Awesome 6 Brands-Regular-400.otf")


# Colors
bg_color <- "#f1f0ef"
text_color <- "#005522"
c_squares <- c("#006e89", "#ff6037", "#e6bc00", "#3a9234")


# Texts -----------------------------------------------------------------------------


# Title
title_text <- "#TidyTuesday Datasets"

# Subtitle
sub_text <- subtitle <- glue(
  "From 2018 to today (2024, week 27), 644 datasets have been released for the **#TidyTuesday** Challenge.<br>",
  "Here, each **square** represents a **dataset**.<br>",
  "The **colors** indicate the number of **observations/variables** in the dataset."
)

# Caption
data <- glue("**Data**: TidyTuesday Datasets, {{ttmeta}}")
chart <- glue("**#TidyTuesday**: 2024 week 27 | {data}")
X_icon <- glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")
author <- glue("**Graphic**: {X_icon} @AndriambeloRajo")

caption_text <- glue("{chart} <br> {author} | #RStats")


# plots -----------------------------------------------------------------------------

# plot 1: observations
obs_plot <- obs_df |>
  ggplot(aes(fill = obs_num, values = n)) +
  geom_waffle(color = text_color, size = .15, flip = TRUE) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(
    labels = function(x) x * 10,
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c_squares) +
  labs(
    fill = "Number of observations"
  ) +
  theme_minimal() +
  theme(

    # background
    plot.background = element_rect(fill = bg_color, color = bg_color),

    # axis
    axis.title = element_blank(),
    axis.text.y = element_text(color = text_color, family = font_plt, size = 10),

    # legend
    legend.position = "top",
    legend.title.position = "top",
    legend.title = element_text(
      color = text_color,
      hjust = 0.5,
      family = font_plt,
      face = "bold",
      size = 14.5,
    ),
    legend.text = element_text(
      color = text_color,
      family = font_plt,
      size = 12
    ),

    # Strip text (facet)
    strip.text = element_blank()
  ) +
  coord_equal()


# plot 2 : variables
var_plot <- var_df |>
  ggplot(aes(fill = var_num, values = n)) +
  geom_waffle(color = text_color, size = .15, flip = TRUE) +
  facet_wrap(~year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() +
  scale_y_continuous(
    labels = function(x) x * 10,
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c_squares) +
  labs(
    fill = "Number of variables"
  ) +
  theme_minimal() +
  theme(

    # background
    plot.background = element_rect(fill = bg_color, color = bg_color),

    # axis
    axis.title = element_blank(),
    axis.text.y = element_text(colour = text_color, family = font_plt, size = 10),

    # legend
    legend.position = "top",
    legend.title.position = "top",
    legend.title = element_text(
      color = text_color,
      hjust = 0.5,
      family = font_plt,
      face = "bold",
      size = 14.5,
    ),
    legend.text = element_text(
      color = text_color,
      family = font_plt,
      size = 12
    ),

    # Strip text (facet)
    strip.text = element_text(
      color = text_color,
      family = font_plt,
      face = "bold",
      size = 12,
    ),
  ) +
  coord_equal()


# Combining plots
patchwork <- obs_plot / var_plot

final_plot <- patchwork +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = title_text,
    subtitle = sub_text,
    caption = caption_text,
    theme = theme(

      # background
      plot.background = element_rect(colour = bg_color, fill = bg_color),


      # title
      plot.title = element_markdown(
        color = text_color,
        family = font_plt,
        face = "bold",
        hjust = 0.5,
        lineheight = 1.5,
        size = 28,
        margin = margin(t = 30, b = 15)
      ),

      # subtitle
      plot.subtitle = element_markdown(
        color = text_color,
        family = font_plt,
        hjust = 0.5,
        lineheight = 1.2,
        size = 14,
        margin = margin(b = 30),
      ),

      # caption
      plot.caption = element_markdown(
        color = text_color,
        family = font_plt,
        hjust = 0.0,
        lineheight = 1.2,
        size = 9,
        margin = margin(t = 30, b = 5)
      )
    ),
  )


# Saving the figure -----------------------------------------------------------------

showtext_opts(dpi = 300)
ggsave(
  "tidytuesday.png",
  final_plot,
  width = 29,
  height = 21,
  units = "cm"
)
