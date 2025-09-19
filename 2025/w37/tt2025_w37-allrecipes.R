# Packages -------------------------------------------------------

library(tidyverse)
library(ggsankey)
library(ggtext)
library(ggarrow)
library(systemfonts)

# Data -----------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 37)

cuisines <- tt$cuisines


monthly_country <- cuisines |>
  mutate(
    year = year(date_published),
    month = month(date_published, label = TRUE),
    month = factor(month, levels = month.abb)
  ) |>
  filter(year == 2024) |>
  mutate(country = fct_lump_n(country, n = 6)) |>
  summarize(
    .by = c(year, month, country),
    n = n(),
  ) |>
  arrange(month) |>
  group_by(year) |>
  complete(month, country) |>
  ungroup() |>
  mutate(
    .by = c(country),
    n = replace_na(n, 0),
    cumsum = cumsum(n)
  ) |>
  arrange(desc(year), desc(month), desc(cumsum))

month_stats <- monthly_country |>
  summarise(
    .by = month,
    pub = sum(n),
  ) |>
  arrange(month) |>
  mutate(
    cumulative_pub = cumsum(pub),
    total_pub = sum(pub),
    prop = round(pub / total_pub * 100)
  )

nov <- month_stats |>
  filter(month == "Nov")

annotation <- glue::glue(
  "In November 2024, {nov$pub} new recipes were published, ",
  "representing {nov$prop}% of all recipes added to the platform this year."
)

labels <- monthly_country |>
  filter(month == "Dec") |>
  arrange(cumsum) |>
  mutate(
    labs = glue::glue("{country}: {cumsum}"),
    stacked_sum = cumsum(cumsum),
    seg_inf = lag(stacked_sum, default = 0),
    pos = seg_inf + cumsum / 2
  )

y_axis <- tibble(
  y = c(200, 400, 600),
  x = rep("Jan", 3),
  xend = rep("Dec", 3)
)

# Misc -----------------------------------------------------------

register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

ahn <- "Atkinson Hyperlegible Next"

color_black <- "#2d3c44"
color_white <- "#fafafa"
color_black_light <- colorspace::lighten(color_black, .4)

pal <- c(
  "Other" = "#b8c1c8",
  "French" = "#5a7ba6",
  "Soul Food" = "#8b6f47",
  "Indian" = "#f4bf77",
  "German" = "#2a3640",
  "Filipino" = "#b85450",
  "Jewish" = "#8d5eb7",
  "Cajun and Creole" = "#6b8e5a"
)

# Title
title <- "Monthly Recipe Publications on allrecipes.com by Cuisine Origin (2024)"

# Caption
src <- glue::glue(
  "**Source**: allrecipes.com, {{tastyR}}"
)

chart <- glue::glue(
  "**#TidyTuesday**: 2025 Week 37"
)

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{src} | {chart} | {author} | #rstats")

# Plot -----------------------------------------------------------

p <- monthly_country |>
  ggplot() +
  geom_segment(
    data = y_axis,
    aes(y = y, x = x, xend = xend),
    linetype = "FF",
    linewidth = .1,
    color = color_black_light
  ) +
  geom_text(
    data = y_axis,
    aes(x = x, y = y, label = y),
    position = position_nudge(x = .18, y = 9),
    family = ahn,
    size = 3.86,
    color = color_black_light,
  ) +
  geom_sankey_bump(
    aes(
      x = month,
      node = country,
      fill = country,
      value = cumsum,
      label = country,
    ),
    color = "transparent",
    space = 0,
    smooth = 10,
    type = "alluvial",
    linewidth = .5,
    show.legend = FALSE
  ) +
  geom_richtext(
    data = labels,
    aes(x = 12.1, y = pos, label = labs),
    label.colour = color_black_light,
    fill = color_white,
    color = color_black,
    family = ahn,
    hjust = 0,
    size = 4.56,
    show.legend = FALSE
  ) +
  geom_arrow_curve(
    data = nov,
    aes(
      x = 9,
      y = cumulative_pub - 30,
      xend = 10.6,
      yend = cumulative_pub + 10
    ),
    curvature = -.5,
    angle = 75,
    linewidth = .6,
    color = color_black
  ) +
  annotate(
    "text",
    x = 9,
    y = nov$cumulative_pub - 110,
    label = str_wrap(annotation, 22),
    size = 4.56,
    lineheight = .9,
    color = color_black,
    family = ahn
  ) +
  scale_fill_manual(values = pal) +
  coord_cartesian(
    expand = FALSE,
    clip = "off"
  ) +
  labs(
    title = title,
    caption = caption_text
  ) +
  theme(
    text = element_text(family = ahn),
    panel.background = element_rect(fill = color_white, color = NA),
    plot.background = element_rect(fill = color_white, color = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      size = 11.5,
      color = color_black_light
    ),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 24,
      family = ahn,
      face = "bold",
      color = color_black
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      family = ahn,
      size = 9,
      halign = 0,
      color = colorspace::lighten(color_black, .3),
      margin = margin(t = 12)
    ),
    plot.margin = margin(25, 135, 10, 25)
  )


ggsave(
  "2025_w37-allrecipes.png",
  p,
  width = 29.7,
  height = 21,
  units = "cm"
)
