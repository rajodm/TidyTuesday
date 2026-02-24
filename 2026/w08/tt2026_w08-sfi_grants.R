# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggtext)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 8)
sfi_grants <- tt$sfi_grants

plot_data <-
  sfi_grants |>
  dplyr::arrange(start_date) |>
  dplyr::mutate(
    decade = lubridate::year(start_date) %/% 10 * 10,
    decade = forcats::fct_inorder(as.character(decade))
  ) |>
  dplyr::summarize(
    .by = c(decade, programme_name),
    distinct_program = dplyr::n()
  )

skim <- skimr::skim(sfi_grants)

# Miscs ------------------------------------------------------------------
# Colors
color_ink <- "#1a252f"
color_paper <- "#f8f9fa"
color_gray <- "#4a5568"
color_gray1 <- "#5d6b7a"

systemfonts::register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

# Caption
chart <- glue::glue(
  "**#TidyTuesday**: 2026 Week 08"
)

caption <- "**Source**: Science Foundation Ireland Grants Commitments | Ireland's Open Data Portal"

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")

author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{caption}<br>{chart} | {author} | #rstats")

# Subtitle
subtitle <- glue::glue(
  "Science Foundation Ireland awarded {scales::number(nrow(sfi_grants), big.mark = ',')} grants between {format(skim$Date.min[1], '%B %Y')} and {format(skim$Date.max[1], '%B %Y')}. Grants are grouped by funding programmes; programmes with more than 150 grants are highlighted."
)

# Plot -------------------------------------------------------------------

ggplot2::set_theme(ggplot2::theme_minimal(
  base_size = 12,
  base_family = "Atkinson Hyperlegible Next",
  paper = "#f8f9fa",
  ink = "#1a252f"
))

plot <- plot_data |>
  ggplot2::ggplot(ggplot2::aes(decade, distinct_program)) +
  ggrepel::geom_text_repel(
    data = plot_data |>
      dplyr::filter_out(distinct_program < 150),
    ggplot2::aes(label = stringr::str_wrap(programme_name, 24)),
    family = "Atkinson Hyperlegible Next",
    lineheight = .85,
    nudge_y = 10,
    nudge_x = 0,
    size = 4.21,
    color = color_gray
  ) +
  ggbeeswarm::geom_beeswarm(size = 2, color = "#1a252f") +
  ggplot2::labs(
    x = "Decades",
    title = "Science Foundation Ireland: Evolving Grant Programmes Across the 2000s, 2010s, and 2020s",
    subtitle = subtitle,
    caption = caption_text
  ) +
  ggplot2::scale_y_continuous(
    breaks = seq(0, 800, by = 200),
    labels = c(
      "0",
      "200",
      "400",
      "600",
      stringr::str_wrap("800 grants awarded", 12)
    )
  ) +
  ggplot2::theme_sub_axis(
    text = ggplot2::element_text(size = 11, color = color_gray1),
    title = ggplot2::element_text(size = 12, color = color_gray)
  ) +
  ggplot2::theme_sub_axis_y(title = ggplot2::element_blank()) +
  ggplot2::theme_sub_plot(
    title.position = "plot",
    title = ggtext::element_textbox_simple(
      size = 24,
      lineheight = 1.2,
      width = ggplot2::unit(8.19, "in"),
      hjust = 0,
      face = "bold",
      margin = ggplot2::margin(b = 6)
    ),
    subtitle = ggtext::element_textbox_simple(
      size = 12,
      width = ggplot2::unit(8.5, "in"),
      hjust = 0,
      color = color_gray,
      margin = ggplot2::margin(b = 14)
    ),
    caption.position = "plot",
    caption = ggtext::element_textbox_simple(
      size = 9,
      color = color_gray,
      margin = ggplot2::margin(t = 10),
      halign = 1
    ),
    margin = ggplot2::margin(25, 25, 10, 25),
  )


ggh4x::save_plot(
  "2026_w08-SFI_grants.png",
  plot = plot,
  width = 29.7,
  height = 21,
  units = "cm"
)
