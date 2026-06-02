# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(countrycode)
library(ggarrow)


# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 22)
eplp <- tt$eplp
plot_data <- eplp |>
  filter(year == 2024) |>
  select(country, starts_with("mat")) |>
  mutate(
    country = replace_values(country, "UK" ~ "GB"),
    country.name = countrycode::countrycode(
      country,
      origin = "iso2c",
      destination = "country.name.en"
    ),
    country.name = glue::glue("{str_wrap(country.name, 8)}"),
    country.flag = countrycode::countrycode(
      country,
      origin = "iso2c",
      destination = "unicode.symbol"
    ),
    mat_v_ld_bb = mat_m_ld_bb + mat_v_ld_bb,
    mat_v_ld_ab = mat_m_ld_ab + mat_v_ld_ab,
    mat_m_duration = mat_m_ld_bb + mat_m_ld_ab,
    across(
      ends_with("bb"),
      \(x) -x
    ),
    bb_m_xpos = mat_m_ld_bb / 2,
    ab_m_xpos = mat_m_ld_ab / 2,
  )

# eplp <- read_csv(here::here("project/tt2026_w22/data/eplp.csv"))

# Miscs ------------------------------------------------------------------

color_paper <- "#fafaf8"
color_ink <- "#1e2820"
color_prim <- "#2d4a3e"
color_sec <- "#c5d9d0"

# Texts

source <- glue::glue(
  "S. Spitzer et al., “The European Parenting Leave Policies (EPLP) Dataset”. Zenodo, Nov. 19, 2025. doi: 10.5281/zenodo.17648712."
)

subtitle <- glue::glue(
  "Maternity leave refers to leave granted to mothers in connection with childbirth. This chart shows mandatory and voluntary maternity leave available before and after birth for European countries in 2024."
)

caption_text <- glue::glue(
  "**Source**: {source}<br>",
  "**TidyTuesday**: 2026 Week 22 | **Visualization**: Andriambelo Rajo | #rstats"
)

# Plots ------------------------------------------------------------------

theme_set(theme_minimal(
  ink = color_ink,
  paper = color_paper,
  base_size = 12,
  base_family = "Atkinson Hyperlegible Next"
))

# Helper function for annotations
add_annotation <- function(
  arrow_point_x,
  arrow_point_y,
  arrow_butt_x,
  arrow_butt_y,
  label = "",
  curvature = 0.2,
  color = color_ink,
  lab_vjust = 0.5,
  lab_hjust = NULL
) {
  dir <- arrow_point_x - arrow_butt_x

  x_lab_pos <- if (dir >= 0) {
    arrow_butt_x - 0.5
  } else {
    arrow_butt_x + 0.5
  }

  lab_hjust <- if (is.null(lab_hjust)) {
    dplyr::if_else(dir >= 0, 1, 0)
  } else {
    lab_hjust
  }

  list(
    ggarrow::geom_arrow_curve(
      x = arrow_point_x,
      y = arrow_point_y,
      xend = arrow_butt_x,
      yend = arrow_butt_y,
      arrow_head = ggarrow::arrow_fins_minimal(),
      arrow_fins = ggarrow::arrow_head_wings(),
      linewidth = 0.35,
      curvature = curvature,
      color = color
    ),
    ggplot2::annotate(
      "richtext",
      label.colour = NA,
      fill = NA,
      family = "Atkinson Hyperlegible Next",
      x = x_lab_pos,
      y = arrow_butt_y,
      hjust = lab_hjust,
      vjust = lab_vjust,
      lineheight = 1,
      color = color_ink,
      label = label
    )
  )
}

p <- plot_data |>
  ggplot(aes(y = reorder(country.name, mat_m_duration))) +
  geom_text(aes(x = -18, label = country.flag), size = 16 / 2.85) +
  ggpattern::geom_rect_pattern(
    aes(xmin = mat_m_ld_bb, xmax = mat_v_ld_bb, height = 0.5),
    pattern = "stripe",
    pattern_spacing = 0.006,
    pattern_size = 0.035,
    pattern_density = 0.5,
    fill = color_sec,
    pattern_fill = colorspace::lighten(color_prim, 0.3)
  ) +
  ggpattern::geom_rect_pattern(
    aes(xmin = mat_m_ld_ab, xmax = mat_v_ld_ab, height = 0.5),
    pattern = "stripe",
    pattern_spacing = 0.006,
    pattern_size = 0.035,
    pattern_density = 0.5,
    fill = color_sec,
    pattern_fill = colorspace::lighten(color_prim, 0.3),
  ) +
  geom_rect(
    aes(xmin = mat_m_ld_bb, xmax = mat_m_ld_ab, height = 0.5),
    fill = color_prim
  ) +
  geom_rect(
    aes(xmin = -0.16, xmax = 0.16, height = 0.65),
    fill = "#e8c547",
    color = color_ink,
    linewidth = unit(0.35, "lines")
  ) +
  annotate_arrow(
    x = c(-Inf, Inf),
    y = 21.7,
    linewidth = 1.5,
    color = color_prim,
    arrow_head = ggarrow::arrow_head_wings(),
    arrow_fins = ggarrow::arrow_head_wings(),
    length_head = unit(3, "mm"),
    length_fins = unit(3, "mm")
  ) +
  annotate(
    "segment",
    y = 21.5,
    yend = 21.9,
    x = 0,
    linewidth = 0.65,
    color = color_prim,
  ) +
  geom_text(
    aes(
      x = bb_m_xpos,
      label = if_else(
        bb_m_xpos != 0,
        paste0(round(abs(mat_m_ld_bb))),
        ""
      )
    ),
    size = 10 / 2.85,
    color = color_paper
  ) +
  geom_text(
    aes(
      x = ab_m_xpos,
      label = if_else(
        ab_m_xpos != 0,
        paste0(round(abs(mat_m_ld_ab))),
        ""
      )
    ),
    size = 10 / 2.85,
    color = color_paper
  ) +
  geom_text(
    aes(
      mat_v_ld_bb,
      label = if_else(
        mat_v_ld_bb != mat_m_ld_bb,
        paste0(round(abs(mat_v_ld_bb))),
        ""
      )
    ),
    nudge_x = -1.1,
    size = 10 / 2.85,
    color = color_prim,
    fontface = "bold"
  ) +
  geom_text(
    aes(
      mat_v_ld_ab,
      label = if_else(
        mat_v_ld_ab != mat_m_ld_ab,
        paste0(round(abs(mat_v_ld_ab))),
        ""
      )
    ),
    nudge_x = 1.1,
    # size = 10 / 2.85,
    color = color_prim,
    fontface = "bold"
  ) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(),
    position = "top",
    expand = expansion(add = c(6, 3))
  ) +
  scale_y_discrete(
    expand = expansion(add = c(0, 0))
  ) +
  coord_cartesian(clip = "off", xlim = c(-13, 43)) +
  labs(
    title = glue::glue(
      "European Maternity Leave in 2024:<br>",
      "Who Gives the Most Time?"
    ),
    subtitle = subtitle,
    caption = caption_text
  ) +
  theme_sub_panel(
    background = element_rect(fill = color_paper, color = NA),
    grid = element_line(color = "#e5e8e5", linetype = "31"),
    grid.major.y = element_blank(),
    grid.minor = element_blank()
  ) +
  theme_sub_plot(
    title.position = "plot",
    title = element_textbox_simple(
      size = rel(1.6),
      face = "bold",
      family = "Fraunces"
    ),
    subtitle = element_textbox_simple(
      size = rel(1),
      color = colorspace::lighten(color_ink, 0.3),
      margin = margin(t = 8, b = 40)
    ),
    caption.position = "plot",
    caption = element_textbox_simple(
      width = 1,
      size = rel(0.85),
      halign = 0,
      color = colorspace::lighten(color_ink, 0.3),
      margin = margin(t = 16)
    ),
    background = element_rect(fill = color_paper, color = NA),
    margin = margin(16, 16, 10, 16)
  ) +
  theme_sub_axis_y(
    text = element_markdown(
      size = rel(1.1),
      hjust = 0
    ),
    title = element_blank()
  ) +
  theme_sub_axis_x(
    text = element_blank(),
    title = element_blank()
  )

# Labels
lab_df <- tibble::tribble(
  ~x  , ~labs                      ,
  -10 , "10 weeks<br>before birth" ,
    0 , "**DAY OF<br> BIRTH**"     ,
   10 , "+10 wks"                  ,
   20 , "+20 wks"                  ,
   30 , "+30 wks"                  ,
   40 , "40 weeks<br>after birth"
)

final_plot <- p +
  ggtext::geom_richtext(
    data = lab_df,
    aes(x = x, y = 22, label = labs),
    fill = NA,
    color = color_prim,
    family = "Atkinson Hyperlegible Next",
    label.size = 0,
    size = 12 / 2.85,
    label.margin = unit(c(0, 0, 1.4, 0), "lines")
  ) +
  add_annotation(
    20.5,
    20,
    25,
    20.5,
    "**Solid** bars indicate<br>**mandatory leave**",
    0.2,
  ) +
  add_annotation(
    20,
    17.4,
    25,
    18,
    "**Striped** bars indicate<br>**voluntary leave**<br>(maximum duration)",
    -0.4,
  ) +
  add_annotation(
    41,
    8.7,
    38,
    7.5,
    "Ireland offers the longest<br>postpartum maternity leave<br>(up to 40 weeks)",
    -0.3,
    lab_hjust = 1
  ) +
  add_annotation(
    6,
    1,
    13,
    1.5,
    "Lithuania & Estonia<br>don't have mandatory leave"
  ) +
  add_annotation(
    9.8,
    1.8,
    13,
    1.5,
  )

ggh4x::save_plot(
  plot = final_plot,
  here::here("2026/w22", "2026_w22-eplp.png"),
  width = 21,
  height = 25,
  units = "cm"
)
