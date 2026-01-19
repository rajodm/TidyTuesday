# Packages ---------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(ggtext)
library(ggforce)
library(systemfonts)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 3)
apod <- tt$apod

apod_data <- apod |>
  unnest_tokens(word, title) |>
  anti_join(stop_words)

monthly_data <- apod_data |>
  mutate(
    word = case_match(
      word,
      "geminid" ~ "geminids",
      "perseid" ~ "perseids",
      "meteor" ~ "meteors",
      "could" ~ "clouds",
      "galaxy" ~ "galaxies",
      "star" ~ "stars",
      .default = word
    ),
    month = month(date)
  ) |>
  reframe(
    .by = c(month, word),
    last_post = max(year(date)),
    n = n()
  ) |>
  slice_max(order_by = n, n = 15, by = month) |>
  mutate(
    word = str_to_title(word),
    word = fct_inorder(word),
    month = month(month, label = TRUE),
    last_post = factor(last_post, levels = c(min(last_post):max(last_post))),
  )

# Misc -------------------------------------------------------------------

# Colors
color_paper <- "#f8f9fa"
color_ink <- "#212529"

# FA7 is installed in my systems
register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

# Caption
chart <- glue::glue(
  "**#TidyTuesday**: 2026 Week 03"
)

source <- glue::glue(
  "**source**: NASA API | Astronomy Picture of the Day (APOD) Archive & Wikipedia for the annotations"
)

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")

author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{source}<br>{chart} | {author} | #rstats")


# Plot -------------------------------------------------------------------

# Helper function for the annotations
add_circle_mark <- function(terms, description, ...) {
  geom_mark_circle(
    aes(
      filter = word == terms,
      label = glue::glue(
        "{terms}:\n",
        "{text}",
        text = str_wrap(description, 22)
      )
    ),
    linewidth = .5,
    expand = .025,
    con.cap = 1.6,
    label.fill = "#e9ecef",
    label.colour = color_ink,
    label.fontsize = 11,
    label.family = "Atkinson Hyperlegible Next",
    label.fontface = "plain",
    show.legend = FALSE,
    ...
  )
}

# Chart
final_chart <- monthly_data |>
  ggplot(aes(x = month, y = word, size = n, color = last_post)) +
  geom_point() +
  add_circle_mark("Geminids", "Meteor shower peaking around December 14") +
  add_circle_mark(
    "Neowise",
    "A Comet, closest approach to the sun on july 3, 2020",
    x0 = 4
  ) +
  add_circle_mark(
    "Perseids",
    "Meteor shower peaking around August 12-13",
    x0 = 5,
    y0 = 39,
  ) +
  cols4all::scale_color_discrete_c4a_seq(
    palette = "stevens.florida_keys",
    reverse = TRUE,
    name = str_wrap("Most recent Year of publication", 14)
  ) +
  scale_size_binned(
    name = str_wrap("Usage number", 14),
    range = c(1.6, 6.55),
  ) +
  scale_y_discrete(
    expand = expansion(add = c(1, 1))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Seasonal Patterns in APOD Archive Title Terms",
    subtitle = "Displaying the 15 most frequent terms per month in Astronomy Picture of the Day (APOD) titles. Point size indicates number of appearance, color shows most recent year of publication (latest). **Peaks in certain terms align with real astronomical events**.",
    y = "Terms (ordered by first monthly appearance)\n",
    caption = caption_text
  ) +
  theme_classic(
    paper = color_paper,
    ink = color_ink,
    base_size = 14,
    base_family = "Atkinson Hyperlegible Next"
  ) +
  theme_sub_plot(
    title.position = "plot",
    title = element_textbox_simple(
      size = 24,
      face = "bold",
      margin = margin(b = 6),
    ),
    subtitle = element_textbox_simple(
      size = 14,
      color = colorspace::lighten(color_ink, .1),
      lineheight = 1.2,
      margin = margin(b = 16)
    ),
    caption.position = "plot",
    caption = element_textbox_simple(
      size = 9,
      color = colorspace::lighten(color_ink, .2),
      margin = margin(t = 10)
    ),
    background = element_rect(fill = color_paper, color = NA),
    margin = margin(25, 25, 10, 25)
  ) +
  theme_sub_panel(
    background = element_rect(fill = color_paper, color = NA),
    grid.major.y = element_line(
      color = "#dee2e6",
      linetype = "dashed",
      linewidth = .3
    ),
    grid.minor.x = element_line(linetype = "dashed", linewidth = .8, color = )
  ) +
  theme_sub_axis_y(
    text = element_text(hjust = 0),
  ) +
  theme_sub_axis_x(
    title = element_blank()
  ) +
  theme_sub_legend(
    position = "right",
    key.height = unit(28, "pt"),
    key.width = unit(8, "pt")
  ) +
  guides(
    size = guide_legend(
      override.aes = list(shape = 1),
      keyheight = unit(.5, "cm")
    ),
    color = guide_legend(
      override.aes = list(size = 5)
    )
  )

ggh4x::save_plot(
  plot = final_chart,
  "2026_w03-apod_archive.png",
  height = 29.1,
  width = 21.7,
  units = "cm"
)
