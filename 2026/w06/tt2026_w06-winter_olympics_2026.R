# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggstream)
library(ggtext)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 5)
schedule <- tt$schedule

schedule_classified <- schedule |>
  dplyr::filter(is_medal_event) |>
  dplyr::mutate(
    participant_cat = dplyr::case_when(
      stringr::str_detect(
        event_description,
        stringr::regex("wom[ae]n\\b", ignore_case = TRUE)
      ) ~ "Women",
      str_detect(
        event_description,
        stringr::regex("m[ae]n\\b", ignore_case = TRUE)
      ) ~ "Men",
      stringr::str_detect(
        event_description,
        stringr::regex("mixed|team", ignore_case = TRUE)
      ) ~ "Mixed/Team",
      stringr::str_detect(
        event_description,
        stringr::regex("pair", ignore_case = TRUE)
      ) ~ "Pair",
      TRUE ~ "Individual/Other"
    ),
  )


plot_data <-
  schedule_classified |>
  dplyr::count(date, discipline_name, participant_cat)

# Miscs ------------------------------------------------------------------

# Colors
color_ink <- "#1a252f"
color_paper <- "#f8f9fa"
color_gray <- "#cbd5e0"
color_gray1 <- "#5a6c7d"

systemfonts::register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

# Caption
chart <- glue::glue(
  "**#TidyTuesday**: 2026 Week 06"
)

caption <- "**Source**: Milano-Cortina 2026 Winter Olympics Data, Olympics\\.com - Wikipedia"

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")

author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{caption} | {chart} | {author} | #rstats")


# Plot -------------------------------------------------------------------

ggplot2::theme_set(ggplot2::theme_bw(
  base_size = 12,
  base_family = "Atkinson Hyperlegible Next",
  paper = color_paper,
  ink = color_ink
))

plot <- plot_data |>
  ggplot2::ggplot(ggplot2::aes(date, n, fill = participant_cat)) +
  ggstream::geom_stream(type = "ridge", alpha = .8) +
  ggplot2::facet_wrap(~discipline_name, ncol = 4) +
  ggplot2::theme_sub_legend(
    position = "bottom",
    text = ggplot2::element_text(size = 8)
  ) +
  ggplot2::scale_x_date(
    breaks = scales::date_breaks("3 days"),
    labels = scales::label_date("%m-%d")
  ) +
  cols4all::scale_fill_discrete_c4a_cat("cols4all.friendly7") +
  ggplot2::coord_cartesian(expand = FALSE, clip = "off") +
  ggplot2::labs(
    title = "Medal Events at the 2026 Winter Olympics",
    subtitle = "Distribution by participant category across disciplines and competition days",
    caption = caption_text,
    x = "Date (month-day)",
    y = "Number of events",
    fill = "Participant Category"
  ) +
  ggplot2::theme_sub_panel(
    grid.major = ggplot2::element_line(
      color = color_gray,
      linewidth = .2,
      linetype = "dashed"
    ),
    grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_plot(
    title.position = "plot",
    title = ggtext::element_textbox_simple(
      size = 22,
      face = "bold",
      halign = .5,
      color = color_ink,
      margin = ggplot2::margin(b = 6)
    ),
    subtitle = ggtext::element_textbox_simple(
      size = 12,
      halign = .5,
      color = color_gray1,
      margin = ggplot2::margin(b = 24)
    ),
    caption.position = "plot",
    caption = ggtext::element_textbox_simple(
      size = 8,
      color = color_gray1,
      halign = .5,
      margin = ggplot2::margin(t = 10)
    ),
    margin = ggplot2::margin(25, 25, 10, 25)
  ) +
  ggplot2::theme_sub_strip(
    background = ggplot2::element_blank(),
    text = ggplot2::element_text(color = color_ink, size = 12, face = "bold")
  ) +
  ggplot2::theme_sub_axis(
    title = ggplot2::element_text(size = 12),
  ) +
  ggplot2::theme_sub_legend(
    text = ggplot2::element_text(size = 12),
    title = ggplot2::element_text(size = 12, face = "bold")
  )


ggh4x::save_plot(
  "2026_w06-winter_olympics_2026.png",
  plot = plot,
  width = 29.1,
  height = 21.7,
  units = "cm"
)
