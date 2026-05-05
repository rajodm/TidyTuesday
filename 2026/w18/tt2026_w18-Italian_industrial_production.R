# Packages ---------------------------------------------------------------

library(tidyverse)
library(marimekko)

# Colors -----------------------------------------------------------------

color_other <- "#9986A5"
color_flock <- "#79402E"
color_cotton <- "#CCBA72"

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2028, 18)

textiles <- tt$textiles |>
  janitor::clean_names()

data_long <- textiles |>
  tidyr::pivot_longer(
    cols = year,
    values_to = "year"
  ) |>
  dplyr::select(!c(name, tidyr::starts_with("total"), raw_silk)) |>
  tidyr::pivot_longer(
    cols = !year,
    names_sep = "_",
    names_to = c("type", "class"),
    values_drop_na = TRUE
  )

# Plot -------------------------------------------------------------------

plot <- data_long |>
  filter(year >= 1934) |>
  nest(.by = class) |>
  mutate(
    plot = map2(.x = data, .y = class, \(df, title) {
      title <- str_to_title(title)
      df |>
        ggplot() +
        geom_marimekko(
          ggplot2::aes(fill = type, weight = value),
          formula = ~ year | type,
          show.legend = FALSE,
          gap_x = 0.003,
          gap_y = 0
        ) +
        cols4all::scale_fill_discrete_c4a_cat(
          "wes.isle_of_dogs2",
          reverse = TRUE
        ) +
        guides(x = guide_axis(check.overlap = TRUE)) +
        coord_cartesian(expand = FALSE, clip = "off") +
        labs(title = title) +
        theme_minimal(
          paper = "#171717",
          ink = "#e6e6e4",
          base_family = "Atkinson Hyperlegible Next",
          base_size = 12
        ) +
        theme_sub_axis(title = element_blank()) +
        theme_sub_axis_y(text = element_blank()) +
        theme_sub_panel(
          grid.major = element_blank(),
          grid.minor = element_blank()
        ) +
        theme_sub_plot(
          title.position = "plot",
          title = element_text(hjust = 0.5),
        )
    })
  )

final_plot <- patchwork::wrap_plots(plot$plot, ncol = 1) +
  patchwork::plot_annotation(
    title = "Italian Industrial Textile Production Remained Dominated by Cotton",
    subtitle = glue::glue(
      "Bar width shows production quantity for each fiber type ",
      "(<span style='color: {color_cotton}; size: 14pt;'>**Cotton**</span>, ",
      "<span style='color: {color_flock}; size: 14pt;'>**Flock**</span>, ",
      "<span style='color: {color_other}; size: 14pt;'>**Other**</span>) ",
      "from 1934-1985. Wider bars indicate higher production."
    ),
    caption = glue::glue(
      "**Source**: ISTAT (Istituto nazionale di statistica / Italian National Institute of Statistics)<br>",
      "**#TidyTuesday**: 2026 Week 18 | **Graphic**: Andriambelo Rajo | #rstats"
    ),
    theme = theme_minimal(
      base_size = 14,
      ink = "#e6e6e4",
      paper = "#171717",
      base_family = "Atkinson Hyperlegible Next"
    ) +
      theme_sub_plot(
        title.position = "plot",
        title = ggtext::element_textbox_simple(
          family = "Domine",
          face = "bold",
          size = rel(1.4),
          margin = ggplot2::margin(b = 6)
        ),
        subtitle = ggtext::element_textbox_simple(
          size = rel(0.95),
          color = "#969693",
          margin = ggplot2::margin(b = 12)
        ),
        caption.position = "plot",
        caption = ggtext::element_textbox_simple(
          color = "#5e5e5c",
          size = rel(0.8),
          halign = 0,
          margin = ggplot2::margin(t = 8)
        ),
        margin = margin(16, 16, 10, 16)
      )
  )

ggh4x::save_plot(
  here::here("2026/w18/2026_w18-italian_industrial_production.png"),
  plot = final_plot,
  height = 25,
  width = 21,
  units = "cm",
)
