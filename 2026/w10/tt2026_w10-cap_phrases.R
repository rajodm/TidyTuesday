# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(patchwork)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 10)
abs_judgements <- tt$absolute_judgements

raw_data <- rnorm(5000)

arrow_annotation <- tibble::tibble(
  x = c(-1.5, 2, .6),
  x1 = c(-.5, 1.1, 1.55),
  y = c(.2, .2, -.2),
  xend = c(-.18, .9, 1.7),
  yend = c(.02, .02, -.02),
  curvature = c(-.4, .4, .4),
  label = c(
    "50% of responses<br>fall within this range",
    "80% of responses<br>fall within this range",
    "95% of responses<br>fall within this range"
  )
)

arrow_layers <-
  arrow_annotation |>
  dplyr::rowwise() |>
  dplyr::group_split() |>
  purrr::map(
    \(x) {
      ggarrow::geom_arrow_curve(
        data = x,
        linewidth = .3,
        length_head = unit(1.6, "mm"),
        arrow_fins = ggarrow::arrow_head_minimal(),
        ggplot2::aes(x = x1, y = y, xend = xend, yend = yend),
        curvature = x$curvature
      )
    }
  )

# Miscs ------------------------------------------------------------------

systemfonts::register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

# Colors
# Palette from scico nuuk
pal <- c("#d0d085", "#819192", "#396982")

color_paper <- "#f8f9fa"
color_ink <- "#1a252f"
color_gray <- "#4a5568"
color_light_gray <- "#cbd5e0"
text_muted <- "#8896a5"

# Title
title_text <- tibble::tibble(
  x = 0,
  y = 0,
  label = glue::glue(
    "<span style='font-size: 24pt;'>**How much do we agree on what \"likely\" means?**</span><br>",
    "<span style='font-size: 12pt; color: #4a5568; padding-top: .5em;'>5,174 respondents reveal wide variation in how people interpret everyday probability expressions</span>"
  )
)

# Caption
chart <- glue::glue(
  "**#TidyTuesday**: 2026 Week 10"
)

caption <-
  glue::glue(
    "**Source**: Kucharski AJ (2026). CAPphrase: Comparative and Absolute Probability phrase dataset. DOI: 10.5281/zenodo.18750055"
  )

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")

author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{caption}<br>{chart} | {author} | #rstats")

# Plots ------------------------------------------------------------------

# base theme
ggplot2::theme_set(ggplot2::theme_minimal(
  base_size = 14,
  base_family = "Atkinson Hyperlegible Next",
  paper = color_paper,
  ink = color_ink
))

# Main plot
cap_dist_plot <- abs_judgements |>
  ggplot2::ggplot(ggplot2::aes(
    probability,
    reorder(term, probability, median)
  )) +
  ggdist::stat_dots(color = color_ink, scale = .99) +
  ggdist::stat_interval(
    linewidth = 3.1,
    position = ggplot2::position_nudge(y = -.20),
    show.legend = FALSE
  ) +
  ggdist::stat_pointinterval(
    .width = 0,
    point_size = 2.7,
    stroke = .6,
    shape = 24,
    color = color_paper,
    fill = color_ink,
    position = ggplot2::position_nudge(y = -.34)
  ) +
  ggplot2::scale_color_manual(
    values = pal
  ) +
  ggplot2::scale_x_continuous(
    labels = scales::label_percent(scale = 1)
  ) +
  ggplot2::theme_sub_axis_y(
    title = ggplot2::element_blank(),
    text = ggplot2::element_text(colour = color_gray, size = 12, hjust = 0)
  ) +
  ggplot2::theme_sub_axis_x(
    title = ggplot2::element_text(face = "bold", hjust = 1),
    text = ggplot2::element_text(colour = color_gray, size = 12)
  ) +
  ggplot2::theme_sub_panel(
    grid.minor.y = ggplot2::element_blank(),
    grid.major.y = ggplot2::element_blank(),
    grid.minor.x = ggplot2::element_line(
      colour = color_light_gray,
      linetype = "dashed",
      linewidth = .2,
    ),
    grid.major.x = ggplot2::element_line(
      colour = color_light_gray,
      linetype = "dashed",
      linewidth = .3
    )
  )

# How to read/legend
legend <- raw_data |>
  tibble::tibble() |>
  ggplot2::ggplot(ggplot2::aes(raw_data)) +
  ggdist::stat_interval(
    linewidth = 3.1,
    show.legend = FALSE
  ) +
  ggdist::stat_pointinterval(
    position = ggplot2::position_nudge(y = -.008),
    .width = 0,
    point_size = 2.7,
    stroke = .6,
    shape = 24,
    color = color_paper,
    fill = color_ink,
  ) +
  arrow_layers +
  ggtext::geom_richtext(
    data = arrow_annotation,
    ggplot2::aes(x = x, y = y, label = label),
    size = 4.21,
    label.size = 0,
    fill = NA,
    family = "Atkinson Hyperlegible Next"
  ) +
  ggplot2::labs(title = "How to read") +
  ggplot2::annotate(
    "richtext",
    fill = NA,
    size = 4.21,
    x = 0,
    y = -.07,
    label.size = 0,
    label = "**Median**",
    family = "Atkinson Hyperlegible Next"
  ) +
  ggplot2::scale_color_manual(values = pal) +
  ggplot2::coord_cartesian(
    expand = FALSE,
    ylim = c(.35, -.3),
    clip = "off"
  ) +
  ggplot2::theme_void(
    base_size = 14,
    paper = color_paper,
    ink = color_ink
  ) +
  ggplot2::theme_sub_plot(
    title.position = "plot",
    title = ggtext::element_textbox_simple(
      size = 14,
      face = "bold",
      margin = ggplot2::margin(l = 8)
    ),
  )


# Title
title_plot <- title_text |>
  ggplot2::ggplot() +
  ggtext::geom_textbox(
    ggplot2::aes(x = x, y = y, label = label),
    box.color = NA,
    fill = NA,
    color = color_ink,
    family = "Atkinson Hyperlegible Next",
    hjust = .9,
    vjust = .1,
    halign = 0,
    lineheight = 1.3,
    width = ggplot2::unit(.9, "npc"),
  ) +
  ggplot2::coord_cartesian(
    xlim = c(-1, 1),
    expand = FALSE,
    clip = "off"
  ) +
  ggplot2::theme_void() +
  ggplot2::theme_sub_plot(margin = ggplot2::margin(0, 0, 0, 0))

# Final chart
final_chart <- (title_plot |
  legend + patchwork::plot_layout(widths = c(3, 1))) /
  cap_dist_plot +
  patchwork::plot_layout(heights = c(3, 9)) +
  patchwork::plot_annotation(
    caption = caption_text,
    theme = ggplot2::theme(
      plot.margin = ggplot2::margin(25, 10, 10, 10),
      plot.caption.position = "plot",
      plot.caption = ggtext::element_textbox_simple(
        halign = 1,
        size = 9,
        color = text_muted,
        margin = ggplot2::margin(t = 10)
      )
    )
  )

ggh4x::save_plot(
  "2026_w10-cap_phrases.png",
  plot = final_chart,
  height = 21,
  width = 29.7,
  units = "cm",
  dpi = 600
)
