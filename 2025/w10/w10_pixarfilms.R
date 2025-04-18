# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(ggforce)
library(showtext)

# data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 10)
pixar_films <- tt$pixar_films
public_response <- tt$public_response

# Inside Out 2 was released in 2024
inside_out2 <- tibble(
  number = 28,
  film = "Inside Out 2",
  year = 2024,
  film_rating = "PG",
  run_time = 97
)

data <- pixar_films |>
  left_join(public_response, by = "film") |>
  mutate(year = year(release_date)) |>
  select(number, film, year, film_rating, run_time) |>
  # add Inside Out 2 to the data
  rbind(inside_out2)


plot_data <- data |>
  mutate(
    # The movie number 27 is Elemental
    film = if_else(number == 27, "Elemental", film),
    # Fill runtime
    run_time = case_when(
      # Luca 95 min not 151 min
      number == 24 ~ 95,
      # Turning Red (100 min)
      number == 25 ~ 100,
      # Lightyear(105 min)
      number == 26 ~ 105,
      # Elemental 101 min not 155 min
      number == 27 ~ 101,
      TRUE ~ run_time
    ),
    # The other films without rating are rated PG
    film_rating = case_when(
      film_rating %in% c("G", "PG") ~ film_rating,
      TRUE ~ "PG"
    ),
    decade = (year %/% 10) * 10,
    # Needed for the segments for each decade
    decade_end = if_else(decade < max(decade), decade + 9.9, decade + 6),
    year = case_when(
      year %in% decade ~ year + 0.15,
      TRUE ~ year
    )
  ) |>
  mutate(
    avg_runtime = mean(run_time),
    .by = decade
  )

# colors -----------------------------------------------------------------

color_light_blue <- "#c6d6d7"
color_blue <- "#2a4b5f"
color_red <- "#e35c38"
color_black <- "#080813"
color_white <- "#f8f4ed"

# Texts ------------------------------------------------------------------

labels <- c(
  "G" = glue::glue(
    "**G (General Audiences)**<br>",
    "<span style='font-size:11pt'>All Ages Admitted</span>"
  ),
  "PG" = glue::glue(
    "**PG (Parental Guidance Suggested)**<br>",
    "<span style='font-size:11pt'>Some Material May Not Be Suitable<span><br>",
    "<span style='font-size:11pt'>for Children</span>"
  )
)

title <- "Pixar's Content Maturation? Consistent Runtimes with Rising PG Ratings"

# Fonts ------------------------------------------------------------------

font_add_google("Montserrat", family = "montserrat", bold.wt = 800)
font_add(
  "fa6-brands",
  "w01/fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 300)

# captions ---------------------------------------------------------------

data <- glue::glue("**Data**: {{pixarfilms}}, rating-system.fandom.com")
chart <- glue::glue("<b>#TidyTuesday</b>: 2025 Week 10 | {data}")
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart}<br>{author} | #rstats")


# Plot -------------------------------------------------------------------
theme_set(theme_minimal(
  base_size = 12,
  base_family = "montserrat"
))

p <- plot_data |>
  ggplot(aes(x = decade, y = avg_runtime)) +
  geom_segment(
    aes(
      x = year,
      xend = year,
      y = avg_runtime,
      yend = run_time,
      color = film_rating
    ),
    linewidth = 2.6,
    show.legend = FALSE
  ) +
  geom_segment(
    aes(x = decade, xend = decade_end),
    linewidth = 2.6,
    color = color_light_blue
  ) +
  geom_point(
    aes(x = year, y = run_time, color = film_rating),
    size = 4.8
  ) +
  scale_color_manual(
    values = c(
      "G" = color_blue,
      "PG" = color_red
    ),
    # Change the labels
    labels = labels
  ) +
  coord_cartesian(
    xlim = c(1993, 2025)
  ) +
  labs(
    title = title,
    y = "Runtime in minutes\n",
    caption = caption_text
  ) +
  guides(
    color = guide_legend(
      # Change the title of the legend
      title = "Motion Picture Association Rating"
    )
  ) +
  theme(
    text = element_text(color = color_black),
    plot.background = element_rect(fill = color_white, color = color_white),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 22,
      face = "bold",
      margin = margin(t = 8, b = 24)
    ),
    panel.grid = element_line(
      color = colorspace::lighten(color_black, 0.9),
      linetype = "dashed"
    ),
    panel.grid.minor = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.15, 0.9),
    legend.key.spacing.y = unit(2.8, "mm"),
    legend.background = element_rect(
      fill = color_white,
      color = colorspace::lighten(color_black, 0.8)
    ),
    legend.title = element_markdown(
      face = "bold",
      size = 14,
      margin = margin(b = 8)
    ),
    legend.text = element_markdown(size = 12, lineheight = 1),
    axis.title = element_text(
      color = colorspace::lighten(color_black, 0.3),
      size = 13
    ),
    axis.text = element_text(
      color = colorspace::lighten(color_black, 0.3),
      size = 11
    ),
    axis.title.x = element_blank(),
    plot.margin = margin(t = 20, r = 20, b = 12, l = 20),
    plot.caption.position = "plot",
    plot.caption = element_markdown(
      color = color_black,
      hjust = 0,
      lineheight = 1.2,
      size = 8,
      margin = margin(t = 12)
    )
  )

circle_mark <- function(data, title = "", x0 = NA, y0 = NA) {
  geom_mark_circle(
    data = data,
    aes(
      x = year,
      y = run_time,
      y0 = if_else(is.na(y0), run_time, y0),
      x0 = if_else(is.na(x0), year, x0),
      label = glue::glue(
        "{title}:\n",
        "{film} ({year})\n",
        "Runtime: {run_time} min"
      )
    ),
    label.fontface = "plain",
    label.family = "montserrat",
    label.fontsize = 11,
    label.fill = color_white,
    label.colour = color_black,
    con.colour = colorspace::lighten(color_black, 0.2),
    colour = colorspace::lighten(color_black, 0.2),
    con.cap = 12
  )
}

final_plot <- p +
  geom_mark_circle(
    data = plot_data |>
      filter(
        year == min(year) &
          run_time == min(run_time)
      ),
    aes(
      x = decade_end - 0.5,
      y = avg_runtime,
      x0 = 2005,
      y0 = 86,
      label = "Average runtime per decade"
    ),
    colour = "transparent",
    label.fontface = "plain",
    label.family = "montserrat",
    label.fontsize = 11,
    label.fill = color_white,
    label.colour = color_black,
    con.colour = colorspace::lighten(color_black, 0.2),
    expand = 0,
    con.cap = 0
  ) +
  circle_mark(
    data = plot_data |>
      filter(run_time == max(run_time)),
    x0 = 2022,
    title = "Longest Movie"
  ) +
  circle_mark(
    data = plot_data |>
      filter(film_rating == "G") |>
      arrange(desc(year)) |>
      slice(1),
    y0 = 87,
    x0 = 2018,
    title = "Last movie rated 'G'"
  )


# Saving the plot --------------------------------------------------------

ggsave(
  "2025_w10-pixarfilms.png",
  final_plot,
  width = 29.7,
  height = 21,
  units = "cm"
)
