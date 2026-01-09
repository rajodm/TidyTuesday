# Packages ---------------------------------------------------------------

library(tidyverse)
library(treemapify)
library(patchwork)
library(ggtext)
library(ggarrow)
library(systemfonts)

# Data -------------------------------------------------------------------

mpox_search_mg <- gtrendsR::gtrends(
  c("monkeypox", "mpox", "variole du singe"),
  "MG",
  time = "2025-12-25 2026-01-09",
  tz = -180
)

search_interest <- mpox_search_mg$interest_over_time |>
  as_tibble()

write_csv(
  search_interest,
  here::here("2026/w01/data/", "mpox_si.csv"),
)

trends_result <- mpox_search_mg$related_queries |>
  as_tibble() |>
  filter(related_topics == "top")

write_csv(
  search_interest,
  here::here("2026/w01/data/", "mpox_trends.csv"),
)

levels <- c("mpox", "variole du singe", "monkeypox")

trends_data <- trends_result |>
  mutate(
    .by = keyword,
    cleaned_value = str_remove_all(value, keyword) |>
      str_trim(),
    cleaned_value = str_remove_all(cleaned_value, "de la|les|la  ") |>
      str_trim(),
    cleaned_value = case_when(
      cleaned_value %in% c("le", "la") ~ paste(cleaned_value, keyword),
      TRUE ~ cleaned_value
    ),
    cleaned_value = case_when(
      str_detect(cleaned_value, "sympt|signe") ~ "symptômes",
      str_detect(cleaned_value, "image|photo") ~ "Image",
      TRUE ~ cleaned_value
    )
  )

tree_map_data <- trends_data |>
  summarize(
    .by = c(keyword, cleaned_value),
    popularity = sum(as.numeric(subject))
  ) |>
  mutate(
    keyword = factor(keyword, levels = levels)
  )

# Misc -------------------------------------------------------------------

register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

# colors

color_paper <- "#f8f9fa"
color_ink <- "#333333"
color_ink1 <- colorspace::lighten(color_ink, .2)
color_blue <- "#0369a1"
color_orange <- "#c2410c"
color_green <- "#65a30d"


pal <- c(
  "monkeypox" = color_orange,
  "mpox" = color_blue,
  "variole du singe" = color_green
)

# Texts

text_1 <- glue::glue(
  "<span style='color: {color_ink1}; font-weight: 200; font-size: 10pt;'>",
  "First OFFICIALLY<br>CONFIRMED cases</span>",
  "<br>",
  "<span style='color: {color_ink}; font-size: 12pt; font-weight = 900'>",
  "31 Dec 2025</span>",
)

text_2 <- glue::glue(
  "<span style='color: {color_ink1}; font-weight: 200; font-size: 10pt'>Search interest rose<br>",
  "before official reports</span>"
)

# Caption

chart <- glue::glue(
  "**#TidyTuesday**: 2026 Week 01"
)

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{chart} | {author} | #rstats")

# Plots ------------------------------------------------------------------
# Treemap
tm <- tree_map_data |>
  ggplot(
    aes(
      area = popularity,
      fill = keyword,
      subgroup = cleaned_value,
    )
  ) +
  geom_treemap(start = "topleft", show.legend = FALSE) +
  geom_treemap_subgroup_text(
    ggblanket::aes_contrast(
      light = color_paper,
      dark = "#0a0a0a"
    ),
    start = "topleft",
    size = 12,
    family = "Atkinson Hyperlegible Next",
    place = "centre",
    reflow = TRUE,
    grow = FALSE,
  ) +
  geom_treemap_subgroup_border(
    start = "topleft",
    color = color_paper,
    size = .5,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = pal) +
  facet_wrap(~keyword) +
  labs(title = "Top Related Search Queries") +
  theme_minimal() +
  theme_sub_plot(
    title.position = "plot",
    title = element_text(size = 14, face = "bold")
  ) +
  theme_sub_strip(text = element_text(size = 12, face = "bold"))

# Line chart
line_chart <- search_interest |>
  ggplot2::ggplot(
    aes(x = date, y = hits, color = keyword)
  ) +
  geom_line(
    linewidth = 1.4,
    lineend = "round",
    show.legend = FALSE
  ) +
  annotate(
    "richtext",
    x = ymd("2025-12-31") + .15,
    y = 100,
    family = "Atkinson Hyperlegible Next",
    label = text_1,
    label.r = unit(0, "lines"),
    label.padding = unit(c(0, 0, 0, 0), "lines"),
    color = "transparent",
    fill = "transparent",
    hjust = 0,
    vjust = 1,
    lineheight = 1.1
  ) +
  annotate(
    "richtext",
    x = ymd("2025-12-28") - .3,
    y = 62,
    family = "Atkinson Hyperlegible Next",
    label = text_2,
    label.r = unit(0, "lines"),
    label.padding = unit(c(0, 0, 0, 0), "lines"),
    color = "transparent",
    fill = "transparent",
    hjust = 0.5,
    vjust = 1,
    lineheight = 1.1
  ) +
  annotate(
    "arrow_curve",
    x = ymd("2025-12-28") - .5,
    xend = ymd("2025-12-29"),
    y = 65,
    yend = 70,
    curvature = -.5,
    angle = 75,
    linewidth = .6,
    color = color_ink
  ) +
  annotate(
    "segment",
    y = 0,
    yend = 100,
    color = color_ink,
    linewidth = .6,
    x = ymd("2025-12-31"),
    linetype = "dashed"
  ) +
  ggrepel::geom_text_repel(
    data = search_interest |>
      filter(date == "2026-01-09"),
    aes(family = keyword, label = str_wrap(keyword, 10)),
    fontface = "bold",
    family = "Atkinson Hyperlegible Next",
    hjust = 0,
    segment.size = .7,
    segment.alpha = .6,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -.1,
    segment.ncp = 3,
    segment.angle = 20,
    nudge_x = 1,
    nudge_y = 1.6,
    size = 4.21,
    show.legend = FALSE
  ) +
  scale_color_manual(values = pal) +
  scale_x_date(
    breaks = scales::date_breaks("4 days"),
    labels = scales::label_date_short(sep = " "),
    expand = expansion(add = c(0, .6))
  ) +
  labs(
    title = "Search Interest since Dec 25, 2025",
    y = "Relative interest"
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_minimal(
    base_family = "Atkinson Hyperlegible Next",
    base_size = 12,
    ink = color_ink,
    paper = color_paper
  ) +
  theme_sub_axis_left(title = element_text(size = 11)) +
  theme_sub_axis_bottom(
    title = element_blank(),
    text = element_text(size = 11)
  ) +
  theme_sub_plot(
    title.position = "plot",
    title = element_text(face = "bold", size = 14)
  ) +
  theme_sub_panel(
    grid = element_line(linewidth = .5, linetype = "31"),
    grid.minor.y = element_blank()
  )

# Combine plots
final_chart <- line_chart /
  tm +
  plot_annotation(
    title = "Mpox Search Trends, Madagascar",
    subtitle = glue::glue(
      "Google Trends analysis for mpox-related search terms surrounding ",
      "first mpox cases in Madagascar"
    ),
    caption = glue::glue(
      "**Source**: Google Trends via {{gtrendsR}} R package, ",
      "MG, Retrieved: January 09, 2026 | ",
      "{caption_text}"
    ),
    theme = theme(
      text = element_text(family = "Atkinson Hyperlegible Next"),
      plot.title.position = "plot",
      plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 12, color = color_ink1),
      plot.margin = margin(t = 15, r = 10, l = 10, b = 10),
      plot.caption.position = "plot",
      plot.caption = element_textbox_simple(
        halign = 1,
        size = 8,
        color = color_ink1,
        family = "Atkinson Hyperlegible Next",
        margin = margin(t = 6)
      )
    )
  ) +
  plot_layout(heights = c(1.6, 1)) &
  theme_sub_plot(background = element_rect(fill = color_paper, color = NA)) &
  theme_sub_panel(background = element_rect(fill = color_paper, color = NA))

ggsave(
  "2026_w01-byod_mpox_trend.png",
  final_chart,
  height = 21.7,
  width = 29.1,
  units = "cm"
)
