# Packages ---------------------------------------------------------------

library(tidyverse)
library(biscale)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(showtext)
library(cowplot)
library(ggtext)
library(ggpattern)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 25)
cases_year <- tt$cases_year


# map of the africa region
af <- ne_countries("large", continent = "africa", returnclass = "sf") |>
  select(iso_a3, name_en)

measles_year <- cases_year |>
  # filter data for 2024 and africa region
  filter(year == 2024 & (iso3 %in% af$iso_a3 | country %in% af$name_en)) |>
  select(
    country,
    iso3,
    measles_total,
    measles_incidence_rate_per_1000000_total_population,
    measles_lab_confirmed
  ) |>
  mutate(
    lab_confiramtion_rate = case_when(
      measles_lab_confirmed == 0 ~ 0,
      TRUE ~ (measles_lab_confirmed / measles_total) * 100
    )
  )

# Join data
map_data <- af |>
  left_join(measles_year, by = join_by(iso_a3 == iso3)) |>
  mutate(
    across(
      where(is.numeric),
      \(x) replace_na(x, 0)
    )
  )

# Create a bi_calss category for the biscale colors
bi_data <- map_data |>
  filter(measles_total != 0) |>
  bi_class(
    x = measles_incidence_rate_per_1000000_total_population,
    y = lab_confiramtion_rate,
    style = "quantile",
    dim = 4
  )

# Breaks for the legend
b_break <- bi_data |>
  bi_class_breaks(
    x = measles_incidence_rate_per_1000000_total_population,
    y = lab_confiramtion_rate,
    style = "quantile",
    dim = 4,
    dig_lab = c(x = 3, y = 3),
    si_levels = FALSE,
    split = TRUE
  )

# Total cases for the subtitle
total_cases <- map_data |>
  st_drop_geometry() |>
  summarize(tt_cases = sum(measles_total)) |>
  pull(tt_cases)

# Fonts ------------------------------------------------------------------
# Fonts
font_add_google(
  "Work Sans",
  "workS"
)

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 480)

# Texts ------------------------------------------------------------------
# title
title <- "Measles Incidence & Laboratory Confirmation Rates in Africa, 2024"
# subtitle
subtitle <- glue::glue(
  "Africa reported {total_cases} measles cases in 2024. Laboratory confirmation rates vary significantly across countries."
)
# Caption
src <- glue::glue(
  "**Source**: World Health Organisation Provisional monthly measles and rubella data."
)

chart <- glue::glue(
  "**#TidyTuesday**: 2025 Week 25"
)

bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{src}<br>{chart} | {author} | #rstats")

# Colors -----------------------------------------------------------------

color_white <- "#f8f7f5"
color_black <- "#1a1c23"
color_text <- "#4c4c4c"
map_pal <- "DkViolet2"

# Plot -------------------------------------------------------------------

# Main map
map <- ggplot(bi_data) +
  geom_sf(
    data = bi_data,
    aes(fill = bi_class, geometry = geometry),
    color = color_white,
    linewidth = .08,
    show.legend = FALSE
  ) +
  # There was 0 case reported in eSwatini
  geom_sf(
    data = map_data |>
      filter(country == "Eswatini"),
    aes(geometry = geometry),
    fill = "white",
    ,
    linewidth = .08,
    show.legend = FALSE
  ) +
  # Pattern for missing data
  geom_sf_pattern(
    data = map_data |>
      filter(iso_a3 == -99 | is.na(country)),
    pattern_type = "stripe",
    pattern_angle = 45,
    pattern_spacing = .007,
    pattern_density = .3,
    pattern_fill = "white",
    pattern_color = "#ff69b4",
    pattern_size = .3,
    colour = "white",
    size = .2,
    show.legend = FALSE
  ) +
  bi_scale_fill(pal = map_pal, dim = 4, flip_axes = FALSE, rotate_pal = FALSE) +
  theme_void(base_family = "workS")


# Create the legend
legend <- bi_legend(
  pal = map_pal,
  flip_axes = FALSE,
  rotate_pal = FALSE,
  dim = 4,
  breaks = b_break,
  xlab = "Cases per 1M population",
  ylab = "Laboratory confirmation rate %",
  size = 11,
  # require dev. version of biscale
  base_family = "workS"
) +
  theme(
    plot.background = element_rect(fill = color_white, color = NA),
    panel.background = element_rect(fill = color_white, color = NA)
  )

# Custom funtion for the annotations
add_custom_annotation <- function(
  plot,
  x,
  y,
  color,
  size,
  hjust,
  label,
  r_l = c("l", "r")
) {
  r_l <- match.arg(r_l)
  ggdraw() +
    draw_plot(plot, 0, 0, 1, 1) +
    draw_line(
      x = x,
      y = y,
      color = color,
      size = size
    ) +
    geom_textbox(
      aes(
        x = if_else(r_l == "l", x[3] - .01, x[3] + .01),
        y = y[3],
        label = label
      ),
      text.color = color,
      box.color = color,
      box.padding = unit(c(5, 5, 5, 5), "pt"),
      width = NULL,
      maxwidth = unit(.2, "inch"),
      fill = "#ffffff",
      fontface = "plain",
      box.size = 0.05,
      box.r = unit(.04, "in"),
      hjust = hjust
    )
}

main_plot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, -.07, .05, .42, .42)

final_plot <- main_plot |>
  add_custom_annotation(
    x = c(0.743, 0.79, 0.81),
    y = c(0.61, 0.58, 0.58),
    color = "#311e3b",
    size = .9,
    hjust = 0,
    r_l = "r",
    label = glue::glue(
      "<span style= 'font-size: 12pt'>**High Incidence +<br>High lab confirmation**</span><br><span style='color: {color_text}; font-size: 10pt;'>Outbreak with<br>good surveillance (Somalia)</span>"
    )
  ) |>
  add_custom_annotation(
    x = c(0.344, 0.28, 0.23),
    y = c(0.61, 0.609, 0.65),
    color = "#9d3546",
    size = .9,
    label = glue::glue(
      "<span style= 'font-size: 12pt'>**High Incidence +<br>Low lab confirmation**</span><br><span style='color: {color_text}; font-size: 10pt;'>Surveillance improvement needed<br>(Liberia)</span>"
    ),
    hjust = 1
  ) |>
  add_custom_annotation(
    x = c(0.57, 0.57, 0.5),
    y = c(0.271, 0.1, 0.1),
    color = "#4279af",
    size = .9,
    label = glue::glue(
      "<span style= 'font-size: 12pt'>**Low Incidence +<br>High lab confirmation**</span><br><span style='color: {color_text}; font-size: 10pt;'> Good surveillance,<br>maintaining elimination<br>(Botswana)</span>"
    ),
    hjust = 1
  ) |>
  add_custom_annotation(
    x = c(0.63, 0.68, 0.72),
    y = c(0.26, 0.18, 0.18),
    color = color_black,
    size = .9,
    r_l = "r",
    hjust = 0,
    label = glue::glue(
      "**Zero Cases** <span style='font-size: 10pt;'>(eSwatini)</span>"
    )
  ) |>
  add_custom_annotation(
    x = c(0.65, 0.68, 0.72),
    y = c(0.84, 0.84, 0.88),
    color = color_text,
    size = .4,
    r_l = "r",
    hjust = 0,
    label = glue::glue(
      "<span style='font-size: 9pt;'>Missing data</span>"
    )
  ) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption_text
  ) +
  theme(
    text = element_text(color = color_black, family = "workS"),
    plot.background = element_rect(fill = color_white, color = NA),
    panel.background = element_rect(fill = color_white, color = NA),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      family = "workS",
      face = "bold",
      size = 22,
      color = color_black,
      margin = margin(b = 6)
    ),
    plot.subtitle = element_textbox_simple(
      size = 11,
      color = color_black,
      margin = margin(b = 8)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      size = 8,
      halign = 1,
      hjust = 0
    ),
    plot.margin = margin(25, 25, 10, 25)
  )

ggsave(
  "2025_w25-measles.png",
  final_plot,
  width = 29.7,
  height = 21,
  units = "cm",
  dpi = 480
)
