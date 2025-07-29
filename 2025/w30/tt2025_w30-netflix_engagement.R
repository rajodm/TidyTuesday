# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(scales)
library(ggh4x)
library(systemfonts)

# Colors -----------------------------------------------------------------

color_2K25 <- "#996515"
color_2K24 <- "#8b0000"
color_2K23 <- "#1c1c1c"
color_title <- "#2d2d2d"
color_subtitle <- "#666666"
color_white <- "#f8f9fa"
color_white1 <- colorspace::adjust_transparency(color_white, 0.7)

palette <- c(
  "2023" = color_2K23,
  "2024" = color_2K24,
  "2025" = color_2K25
)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 30)
shows <- tt$shows

top_limited_series <-
  shows |>
  filter(
    str_detect(title, "Limited Series") & !is.na(release_date) & !is.na(runtime)
  ) |>
  reframe(
    .by = title,
    runtime = runtime,
    release_date = release_date,
    total_views = sum(views)
  ) |>
  slice_head(by = title, n = 1) |>
  separate_wider_delim(
    title,
    names = c("title", "original_title"),
    delim = " // ",
    too_few = "align_end"
  ) |>
  mutate(
    title = str_remove_all(title, ":[^:]*$") |>
      str_trim(),
    original_title = str_remove_all(original_title, ":[^:]*$") |>
      str_trim(),
    year = year(release_date),
    runtime = period(runtime) |>
      as.numeric("minutes"),
    labs_runtime = glue::glue(
      "<span style='font-size: 12pt; color: {color_white1}'>{year} • {runtime} min</span>"
    ),
    labs = if_else(
      !is.na(title),
      glue::glue(
        "**{title}** (<span style='font-size: 14pt'>{original_title}</span>)<br>",
        "{labs_runtime}"
      ),
      glue::glue(
        "**{original_title}**<br>",
        "{labs_runtime}"
      )
    ),
    labs_views = number(total_views, accuracy = .1, scale = 1e-6, suffix = "M")
  ) |>
  slice_max(order_by = total_views, n = 10)

# Fonts ------------------------------------------------------------------

register_variant(
  "fa6-brands",
  "Font Awesome 6 Brands",
)

# Texts ------------------------------------------------------------------

# Title & Subtitle
title_text <- "Top Netflix Limited Series by Views"
subtitle_text <- "**A limited series is a TV program with a predetermined number of episodes that tells a complete, self-contained story within a single season.** This chart displays the limited series generating the highest engagement on the platform as of July 2025. With 3 of these top performers released earlier this year, is this highlighting the strong audience response to this type of content?"

# Caption
src <- glue::glue(
  "**Source**: Netflix Engagement Report"
)

chart <- glue::glue(
  "**#TidyTuesday**: 2025 Week 30"
)

bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{src} | {chart} | {author} | #rstats")

# Plot -------------------------------------------------------------------

plot <- top_limited_series |>
  ggplot(aes(
    fct_reorder(original_title, total_views),
    total_views,
    fill = factor(year)
  )) +
  geom_col() +
  geom_hline(
    yintercept = seq(
      from = 25e6,
      to = 150e6,
      by = 25e6
    ),
    color = color_white,
    alpha = .06,
    linewidth = .4
  ) +
  geom_richtext(
    aes(y = 1, label = labs),
    label.padding = margin(l = 6),
    label.margin = unit(0, "pt"),
    hjust = 0,
    lineheight = .75,
    size = 5.614,
    label.color = NA,
    color = color_white,
    fill = NA,
    family = "Atkinson Hyperlegible Next"
  ) +
  geom_richtext(
    aes(label = labs_views),
    family = "Atkinson Hyperlegible Next",
    fontface = "bold",
    hjust = 1.08,
    fill = NA,
    label.color = NA,
    color = color_white,
    size = 4.912
  ) +
  scale_fill_manual(values = palette) +
  coord_flip(
    clip = "off",
    expand = FALSE
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  theme_void(base_family = "Atkinson Hyperlegible Next") +
  theme(
    plot.background = element_rect(fill = color_white, color = NA),
    panel.background = element_rect(fill = color_white, color = NA),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 32,
      face = "bold",
      color = color_title
    ),
    plot.subtitle = element_textbox_simple(
      size = 14,
      color = color_subtitle,
      lineheight = 1.2,
      margin = margin(t = 12, b = 18)
    ),
    plot.margin = margin(25, 25, 5, 25),
    legend.position = "top",
    legend.justification.top = c(0, 0),
    plot.caption.position = "panel",
    plot.caption = element_textbox_simple(
      color = "#888888",
      size = 9,
      halign = 1,
      margin = margin(t = 0)
    )
  ) +
  guides(
    fill = guide_stringlegend(
      title = "Release Year:",
      theme = theme(
        legend.title = element_text(size = 12, color = color_subtitle),
        legend.text = element_text(size = 14, face = "bold"),
        legend.key.spacing.y = unit(3, "pt"),
      ),
    )
  )

ggsave(
  "2025_w30-netflix_engagement_report.png",
  plot,
  dpi = 300,
  height = 21.7,
  width = 29,
  units = "cm"
)
