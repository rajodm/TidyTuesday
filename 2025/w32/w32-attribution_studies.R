# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggsankey)
library(ggtext)
library(systemfonts)

# Fonts ------------------------------------------------------------------

#! Fonts needs to be installed locally
register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands",
)

ahn <- "Atkinson Hyperlegible Next"

# Colors -----------------------------------------------------------------

color_white <- "#fafafa"
color_cap <- "#9ca3af"


palette <-
  c(
    "Europe" = "#6b8db5",
    "Northern America" = "#5a9fa8",
    "Eastern and\nsouth-eastern Asia" = "#7a8ea4",
    "Australia and New\nZealand" = "#6a7f8c",
    "Latin America and\nthe Caribbean" = "#6b8073",
    "Sub-Saharan Africa" = "#85906d",
    "Central and\nsouthern Asia" = "#8497a7",
    "Northern Africa\nand western Asia" = "#7d8a7a",
    storm = "#4f5d75",
    "Rain & flooding" = "#2563eb",
    Heat = "#ea580c",
    Drought = "#d97706",
    "Cold, snow & ice" = "#0891b2",
    Other = "#64748b"
  )

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 32)
attrib_studies <- tt$attribution_studies

attrib_studies <- attrib_studies |>
  filter(!is.na(iso_country_code) & study_focus == "Event") |>
  mutate(
    event_type = fct_lump(event_type, 5),
    cb_region = str_wrap(cb_region, 18),
    classification = str_wrap(classification, 18)
  )

levels <- c(
  "Northern America",
  "Europe",
  "Eastern and\nsouth-eastern Asia",
  "Central and\nsouthern Asia",
  "Northern Africa\nand western Asia",
  "Sub-Saharan Africa",
  "Latin America and\nthe Caribbean",
  "Australia and New\nZealand",
  "Storm",
  "Rain & flooding",
  "Heat",
  "Drought",
  "Cold, snow & ice",
  "Other",
  "More severe or\nmore likely to\noccur",
  "Decrease, less\nsevere or less\nlikely to occur",
  "No discernible\nhuman influence",
  "Insufficient\ndata/inconclusive"
)

sk_data <- attrib_studies |>
  make_long(cb_region, event_type, classification) |>
  mutate(
    node = factor(node, levels = levels),
    node = fct_rev(node)
  )

# Texts ------------------------------------------------------------------
src <- glue::glue(
  "**Source**: Carbon Brief - Attribution Studies"
)

chart <- glue::glue(
  "**#TidyTuesday**: 2025 Week 32"
)

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{src} <br> {chart} | {author} | #rstats")

subtitle <- "Sankey Diagram showing the link between where events occurred, what happened, and how climate change affected them"
# Plot -------------------------------------------------------------------

plot <- sk_data |>
  ggplot(aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    fill = node,
  )) +
  geom_sankey(
    flow.alpha = .6,
    node.color = color_white,
    linewidth = .3,
    show.legend = FALSE,
    width = .02,
    node.fill = "#2d3748"
  ) +
  geom_sankey_text(
    aes(label = factor(node)),
    position = position_nudge(x = .03, y = 0),
    size = 4.21,
    color = "#374151",
    fontface = "bold",
    family = ahn,
    hjust = 0,
    lineheight = .85
  ) +
  scale_x_discrete(
    labels = c("Study Region", "Studied Event", "Climate Change Influence"),
    expand = expansion(add = c(.1, .45)),
    position = "top"
  ) +
  scale_y_discrete(
    expand = expansion(add = 0)
  ) +
  scale_fill_manual(values = palette) +
  labs(
    title = "Extreme Weather Attribution Studies Results",
    subtitle = subtitle,
    caption = caption_text
  ) +
  theme_sankey(base_family = ahn, base_size = 14) +
  theme(
    panel.background = element_rect(fill = color_white, color = NA),
    plot.background = element_rect(fill = color_white, color = NA),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      face = "bold",
      size = 22,
      color = "#1f2937",
      margin = margin(b = 4)
    ),
    plot.margin = margin(25, 25, 5, 25),
    axis.title = element_blank(),
    plot.subtitle = element_textbox_simple(
      size = 14,
      color = "#6b7280",
      margin = margin(b = 25)
    ),
    axis.text.x = element_text(
      color = "#374151",
      hjust = .05,
      family = ahn,
      size = 12
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      family = ahn,
      size = 8.5,
      halign = 0,
      margin = margin(t = 25),
      color = color_cap,
      lineheight = 1.2
    )
  )

ggsave(
  "2025_w32-attribution_studies.png",
  plot,
  height = 21,
  width = 29.7,
  units = "cm",
  dpi = 300
)
