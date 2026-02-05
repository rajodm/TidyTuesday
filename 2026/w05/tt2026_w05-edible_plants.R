# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggalluvial)
library(ggtext)
library(systemfonts)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 5)
edible_plants <- tt$edible_plants


plot_data <-
  edible_plants |>
  dplyr::mutate(
    soil = stringr::str_replace_all(soil, "\\band\\b|/", ",") |>
      stringr::str_remove_all(stringr::regex(
        "soil.*|\\bany\\b",
        ignore_case = TRUE
      )) |>
      stringr::str_replace("-", " ") |>
      stringr::str_to_title()
  ) |>
  tidyr::separate_longer_delim(soil, ",") |>
  tidyr::separate_longer_delim(soil, ".") |>
  dplyr::mutate(
    soil = stringr::str_trim(soil) |>
      dplyr::replace_values(
        "Sand" ~ "Sandy",
        "Loam" ~ "Loamy",
        "Chalk" ~ "Chalky",
      ) |>
      forcats::fct_lump_min(3)
  ) |>
  dplyr::filter_out(is.na(soil)) |>
  dplyr::summarize(
    .by = c(cultivation, soil),
    n_plants = dplyr::n()
  )

# Miscs ------------------------------------------------------------------

# Add font-awesome for the caption
# The font is already installed on my computer
systemfonts::register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

# Caption
chart <- glue::glue(
  "**#TidyTuesday**: 2026 Week 05"
)

caption <- "**Source**: Edible Plants Database - GROW Observatory"

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")

author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{caption}<br>{chart} | {author} | #rstats")

# Colors
color_ink <- "#1a252f"
color_paper <- "#f8f9fa"
color_gray <- "#5a6c7d"
soil_color <- c(
  "Loamy" = "#a0522d",
  "Chalky" = "#d97742",
  "Clay" = "#c0392b",
  "Fertile" = "#7cb342",
  "Sandy" = "#f39c12",
  "Silty" = "#8e44ad",
  "Well Drained" = "#16a085",
  "Other" = "#95a56a"
)

# Plot -------------------------------------------------------------------

# Base theme
ggplot2::theme_set(
  ggplot2::theme_minimal(
    base_size = 12,
    base_family = "Atkinson Hyperlegible Next",
    paper = color_paper,
    ink = color_ink
  )
)

final_plot <- plot_data |>
  ggplot2::ggplot(ggplot2::aes(
    axis1 = cultivation,
    axis2 = soil,
    y = n_plants
  )) +
  ggalluvial::geom_alluvium(
    ggplot2::aes(fill = soil),
    alpha = .8,
    linewidth = .1,
    color = color_paper,
    show.legend = FALSE
  ) +
  ggalluvial::geom_stratum(
    width = 1 / 4,
    alpha = .6,
    fill = color_paper,
    color = color_gray
  ) +
  ggplot2::geom_text(
    stat = "stratum",
    ggplot2::aes(label = after_stat(stratum)),
    size = 4.56,
  ) +
  ggplot2::labs(
    title = "Plant Cultivation and Soil Preference",
    subtitle = "Flow width shows association strength; colors indicate soil characteristics",
    caption = caption_text
  ) +
  ggplot2::scale_x_discrete(
    limits = c("Cultivation Class", "Soil Characteristics"),
    expand = c(.15, .05)
  ) +
  ggplot2::scale_y_continuous(
    expand = c(.02, 0)
  ) +
  ggplot2::scale_fill_manual(values = soil_color) +
  ggplot2::theme_sub_plot(
    background = ggplot2::element_rect(fill = color_paper, color = NA),
    title.position = "plot",
    title = ggtext::element_textbox_simple(
      size = 24,
      face = "bold",
      color = color_ink,
      halign = .5,
      margin = ggplot2::margin(b = 4)
    ),
    subtitle = ggtext::element_textbox_simple(
      size = 12,
      color = color_gray,
      halign = .5,
      margin = ggplot2::margin(b = 12)
    ),
    caption.position = "plot",
    caption = ggtext::element_textbox_simple(
      size = 9,
      color = color_gray,
      halign = 1,
      margin = ggplot2::margin(t = 10)
    ),
    margin = ggplot2::margin(25, 25, 10, 25)
  ) +
  ggplot2::theme_sub_panel(
    background = ggplot2::element_rect(fill = color_paper, color = NA),
    grid = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_axis_y(
    title = ggplot2::element_blank(),
    text = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_axis_x(
    text = ggplot2::element_text(size = 14, face = "bold", color = color_ink)
  )


ggh4x::save_plot(
  "2026_w05-edible_plants.png",
  plot = final_plot,
  width = 21.7,
  height = 29.1,
  units = "cm"
)
