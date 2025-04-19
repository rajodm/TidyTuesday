# Packages ---------------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)

# Fonts ------------------------------------------------------------------

# font_add_google("Lato", "lato")
font_add(
  "lato",
  "fonts/Lato-Regular.ttf",
  "fonts/Lato-Bold.ttf"
)

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 300)

# Captions ---------------------------------------------------------------

data <- glue::glue(
  "**Data**: Penguins (R Datasets)"
)
chart <- glue::glue(
  "**#TidyTuesday**: 2025 Week 15"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart}<br>{data} | {author} | #rstats")

# Colors -----------------------------------------------------------------

color_black <- "#101a24"
color_black1 <- "#464850"
color_grey <- "#74777b"
color_cap <- "#8d7a62"

# Data -------------------------------------------------------------------
# Penguins dataset from R v4.5.0
summ <- penguins |>
  filter(!is.na(body_mass)) |>
  summarize(
    .by = c(island, species),
    avg_body_mass = median(body_mass, na.rm = TRUE),
    ttl = n()
  )

# Plot -------------------------------------------------------------------

theme_set(theme_minimal(base_size = 12, base_family = "lato"))

plot <- summ |>
  ggplot(aes(island, species)) +
  geom_tile(
    aes(fill = avg_body_mass),
    color = "#fcfcfa",
    alpha = .8,
    linewidth = .4
  ) +
  scale_fill_gradient2(
    low = "#ddf1e4",
    mid = "#66a0c8",
    high = "#8b7ec8",
    midpoint = median(penguins$body_mass, na.rm = TRUE),
    name = "Median Body Mass (g)"
  ) +
  geom_text(
    aes(label = ttl),
    color = color_black1,
    fontface = "bold",
    size = 4.2
  ) +
  coord_fixed(
    expand = FALSE,
    clip = "off"
  ) +
  labs(
    title = "Palmer Penguins",
    subtitle = "Median Body Mass and Total Count by Island and Species",
    x = "Island",
    y = "Species",
    caption = caption_text
  ) +
  theme(
    text = element_text(family = "lato", size = 14, color = color_black),
    plot.background = element_rect(fill = "#f4f0ec", color = NA),
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(
      face = "bold",
      size = 22,
      margin = margin(b = 4),
      color = color_black
    ),
    plot.subtitle = element_text(
      size = 12,
      margin = margin(b = 12),
      color = color_black1
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      color = color_cap,
      size = 8,
      halign = 1,
      margin = margin(t = 12)
    ),
    axis.title = element_text(
      hjust = 1,
      size = 12,
      face = "bold",
      color = color_black1
    ),
    axis.text = element_text(color = color_grey),
    legend.position = "top",
    legend.title.position = "top",
    legend.title = element_text(hjust = .5, size = 11, color = color_black1),
    legend.text = element_text(size = 10, color = color_black1),
    plot.margin = margin(25, 0, 10, 0)
  ) +
  guides(
    fill = guide_colorbar(
      alpha = .8,
      theme = theme(
        legend.key.height = unit(.6, "lines"),
        legend.key.width = unit(12, "lines")
      )
    )
  )

ggsave(
  "2025_w15-base_penguins.png",
  plot,
  height = 8.5,
  width = 8.5,
  units = "in"
)
