# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggh4x)
library(ggtext)
library(systemfonts)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 8)
body_condition <- tt$tortoise_body_condition_cleaned


tortoises_sr <- body_condition |>
  dplyr::distinct(individual, year, locality, sex) |>
  dplyr::count(year, locality, sex) |>
  tidyr::complete(year, locality, sex, fill = list(n = NA)) |>
  tidyr::pivot_wider(
    names_from = sex,
    values_from = n
  ) |>
  dplyr::mutate(
    .by = c(year, locality),
    sex_ratio = m / f
  )

# Tables for the annotations
grid_beach_labs <-
  tibble::tibble(
    locality = "Beach",
    ratio = c(1, 10, 50, 90),
    label = c(
      "",
      "10 males for every female",
      "50 males for every female",
      "90 males for every female"
    )
  )

grid_breaks <-
  grid_beach_labs |>
  dplyr::select(ratio) |>
  tidyr::crossing(locality = c("Konjsko", "Plateau"))


# Miscs ------------------------------------------------------------------

loc_colors <- c(
  # Wes.egypt palette
  "Plateau" = "#C52E19",
  "Konjsko" = "#AC9765",
  "Beach" = "#54D8B1"
)

color_paper <- "#f8f9fa"
color_ink <- "#1a252f"
color_gray <- "#4a5568"
text_muted <- "#8896a5"

systemfonts::register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

# Caption
chart <- glue::glue(
  "**#TidyTuesday**: 2026 Week 09"
)

caption <-
  glue::glue(
    "**Source**: Sex ratio bias triggers demographic suicide in a dense tortoise population (Arsovski et al. 2026)<br>",
    "**Data**: doi.org/10.6084/m9.figshare.30752687"
  )

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")

author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{caption}<br>{chart} | {author} | #rstats")

chart_title <- "Hermann's Tortoise Sex Ratios Vary Strongly Across Three Locations on Golem Grad Island, with the Plateau Remaining Heavily Male-Biased"

chart_subtitle <- "Sixteen years of capture-recapture data on Hermann's tortoises on Golem Grad island reveal a striking imbalance. Values above the dark dashed line indicate more males than females; the y-axis is on a log scale."
# Plot -------------------------------------------------------------------

ggplot2::set_theme(ggplot2::theme_minimal(
  base_size = 14,
  base_family = "Atkinson Hyperlegible Next",
  paper = color_paper,
  ink = color_ink
))

# Facets backgrounds
facet_bg <- tibble::enframe(
  loc_colors,
  name = "locality",
  value = "fill"
) |>
  dplyr::mutate(
    color = colorspace::darken(fill, .3),
    fill = colorspace::lighten(fill, .84)
  )

tortoise <- tortoises_sr |>
  ggplot2::ggplot(ggplot2::aes(
    year,
    sex_ratio,
    color = locality,
    label = locality
  )) +
  # Facets Backgrounds
  ggplot2::geom_rect(
    data = facet_bg,
    ggplot2::aes(fill = fill),
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_rect(
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = 0,
    fill = scales::alpha("#dde3ea", .032),
    inherit.aes = FALSE
  ) +
  # Gridlines
  ggplot2::geom_hline(
    data = grid_breaks |>
      dplyr::filter_out(ratio == 1),
    ggplot2::aes(yintercept = ratio),
    linewidth = .3,
    linetype = "dashed",
    color = colorspace::lighten(color_ink, .3)
  ) +
  ggplot2::geom_hline(
    data = grid_breaks |>
      dplyr::filter(ratio == 1),
    ggplot2::aes(yintercept = ratio),
    linetype = "31",
    linewidth = .8,
    color = color_ink
  ) +
  # The line for the data
  ggplot2::geom_line(linewidth = 1.6, lineend = "round", show.legend = FALSE) +
  ggtext::geom_richtext(
    data = facet_bg,
    aes(label = locality, text.color = color),
    label.margin = ggplot2::margin(t = .24, l = .12, unit = "pt"),
    size = 4.56,
    fontface = "bold",
    x = -Inf,
    y = Inf,
    hjust = 0,
    vjust = 1,
    fill = NA,
    label.color = NA,
    show.legend = FALSE
  ) +
  geomtextpath::geom_texthline(
    data = grid_beach_labs |>
      dplyr::filter_out(ratio == 1),
    ggplot2::aes(yintercept = ratio, label = label),
    family = "Atkinson Hyperlegible Next",
    size = 4.21,
    linewidth = .3,
    linetype = "dashed",
    hjust = .02,
    textcolor = color_gray,
    linecolor = colorspace::lighten(color_ink, .3),
    alpha = .8
  ) +
  geomtextpath::geom_texthline(
    data = grid_beach_labs |>
      dplyr::filter(ratio == 1),
    ggplot2::aes(yintercept = ratio, label = label),
    family = "Atkinson Hyperlegible Next",
    size = 4.21,
    linetype = "31",
    linewidth = .8,
    hjust = .02,
    fontface = "bold",
    color = color_ink
  ) +
  ggplot2::facet_wrap(~locality) +
  ggplot2::labs(
    title = chart_title,
    subtitle = chart_subtitle,
    caption = caption_text
  ) +
  ggplot2::scale_color_manual(values = loc_colors) +
  ggplot2::scale_fill_identity() +
  ggplot2::scale_y_log10() +
  ggplot2::scale_x_continuous(
    breaks = scales::breaks_pretty()
  ) +
  ggplot2::theme_sub_plot(
    title.position = "plot",
    title = ggtext::element_textbox_simple(
      size = 22,
      face = "bold",
      width = ggplot2::unit(10.521, "in"),
      halign = 0,
      hjust = 0,
      margin = ggplot2::margin(b = 6)
    ),
    subtitle = ggtext::element_textbox_simple(
      size = 12,
      color = color_gray,
      width = ggplot2::unit(10.64, "in"),
      halign = 0,
      hjust = 0,
      margin = ggplot2::margin(b = 12)
    ),
    caption.position = "plot",
    caption = ggtext::element_textbox_simple(
      size = 10,
      color = text_muted,
      halign = 1,
      margin = ggplot2::margin(t = 10)
    ),
    margin = ggplot2::margin(25, 25, 10, 25)
  ) +
  ggplot2::theme_sub_panel(
    grid.minor = ggplot2::element_blank(),
    grid.major = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_axis_y(
    title = ggplot2::element_blank(),
    text = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_axis_x(title = ggplot2::element_blank()) +
  ggplot2::theme_sub_strip(
    text = ggplot2::element_blank()
  )

ggh4x::save_plot(
  "2026_w09-hermanns-toirtoises.png",
  plot = tortoise,
  height = 21,
  width = 29.7,
  device = ragg::agg_png,
  units = "cm",
  dpi = 288
)
