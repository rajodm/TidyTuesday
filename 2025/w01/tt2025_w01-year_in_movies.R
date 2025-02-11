
# Packages needed --------------------------------------------------------


library(tidyverse)
library(ggtext)
library(patchwork)
library(showtext)


# Data -------------------------------------------------------------------


movies <- readxl::read_xlsx(here::here("data/movies24.xlsx")) |>
  janitor::clean_names() |>
  mutate(month = month(log, label = TRUE)) |>
  select(!log)

movie_stats <- movies |>
  summarize(
    movies_watched = length(title),
    hours_watched = ceiling(sum(length)/60),
    rating_avg = median(rating)
  )

movie_per_month <- movies |>
  select(starts_with("genre"), title, month) |>
  rowwise() |>
  mutate(
    num_genres = sum(!is.na(across(starts_with("genre"))))
  ) |>
  pivot_longer(
    cols = starts_with("genre"),
    names_to = "genre_number",
    values_to = "genre",
    values_drop_na = TRUE
  ) |>
  mutate(
    genre = fct_lump_n(genre, n = 4),
    genre_prop = 1 / num_genres
  )

movie_summaries <- movie_per_month |>
  summarize(
    count = sum(genre_prop),
    .by = c(month, genre)
  )

# Fonts ----------------------------------------------------------


font_add_google("Space Grotesk", family = "sg", bold.wt = 600)
font_add_google("Work Sans", family = "ws", bold.wt = 600)
font_add("fa6-brands", "fonts/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# Caption ----------------------------------------------------------------


data <- glue::glue("**Data**: My year in movies (Letterboxd)")
chart <- glue::glue("<b>#TidyTuesday</b> : 2025 Week 01<br>{data}")
X_ic <- glue::glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart}<br>{author} | #rstats")

# Texts ------------------------------------------------------------------

bar_plot_title <- "A Year Full of Drama... and Comedy"

# Colors -----------------------------------------------------------------


color_bg <- "#f8f4ed"
color_main <- "#1d2432"
color_grey <- "#8e8e8e"
bar_plot_colors <- c(
  Other = color_main,
  Drama = "#8e3147",
  Comedy = "#b35124",
  Animation = "#3d9bd8",
  Adventure = "#90b346"
)


# Movie Stats ------------------------------------------------------------


value_box <- function(value, label, color = color_main, bg = color_bg) {
  df <- tibble(
    x = 0.5,
    y = 0.5,
    label = glue::glue(
      "<b style='font-size: 28pt;'>{value}</b>",
      "<br>",
      "<span style='font-size: 14pt;'>{str_to_upper(label)}</span>"
    )
  )

  df |>
    ggplot(aes(x, y)) +
    geom_richtext(
      aes(label = label),
      fill = NA,
      label.color = NA,
      color = color,
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 2,
      show.legend = FALSE
    ) +
    theme_void() +
    theme(
      plot.margin = margin(5, 5, 5, 5),
      plot.background = element_rect(fill = bg, color = NA)
    ) +
    coord_fixed(
      xlim = c(0, 1),
      ylim = c(0, 1),
      clip = "off"
    )
}

vb1 <- value_box(movie_stats$movies_watched, label = "Films Watched")
vb2 <- value_box(movie_stats$hours_watched, label = "Viewing Hours")
vb3 <- value_box(
  glue::glue("{movie_stats$rating_avg} / 5"),
  label = "Average rating"
)


movies_by_numbers <- (vb1 + vb2 + vb3) +
  plot_layout(
    nrow = 1,
    widths = c(1, 1, 1),
    guides = "collect"
  ) & theme(
    plot.background = element_rect(fill = color_bg, color = NA),
    plot.margin = margin(l = 0, 0, 0, 0)
  )



# Main Plot --------------------------------------------------------------

main_plot <- movie_summaries |>
  ggplot(aes(x = month, y = count, fill = genre)) +
  geom_bar(stat = "identity", width = 0.8) +
  labs(
    title = bar_plot_title,
    fill = "Movie genres",
    y = "Number of Films Watched\n"
  ) +
  scale_x_discrete(
    expand = c(0, 0),
    labels = str_sub(month.abb, 1, 1)
  ) +
  scale_fill_manual(values = bar_plot_colors) +
  guides(fill = guide_legend(ncol = 2)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    text = element_text(size = 14, family = "ws", color = color_main),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      family = "sg",
      face = "bold",
      size = 18,
      color = color_main,
      margin = margin(t =  20, b = 10)
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(color = color_grey),
    axis.text.x = element_text(
      face = "bold",
      color = color_main,
      margin = margin(t = -18)
    ),
    axis.text.y = element_text(
      size = 10,
      color = color_grey
    ),
    legend.title = element_text(
      family = "sg",
      face = "bold",
      size = 14
    ),
    legend.text = element_text(size = 12),
    # It looks like I shloud use 'legend.position.inside' instead
    legend.position = c(0.25, 0.8),
    legend.background = element_rect(
      fill = color_bg,
      color = color_main,
      linewidth = 0.3
    ),
    legend.key.spacing.y = unit(5, "pt"),
    legend.key.spacing.x = unit(15, "pt"),
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(
      color = color_grey,
      linewidth = 0.3
    ),
    plot.background = element_rect(fill = color_bg, color = color_bg),
    plot.margin = margin(r = 40, l = 40, b = 20, t = 10)
  )

# Final plot -------------------------------------------------------------


final_plot <- (movies_by_numbers / main_plot)+
  plot_layout(
    heights = c(0.5, 3),
  ) +
  plot_annotation(
    title = "MY 2024 IN MOVIES",
    caption = caption_text,
    theme = theme(
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        family = "sg",
        face = "bold",
        color = color_main,
        size = 32,
        # The title doesn't look centered
        halign = 0.66,
        margin = margin(t = 0, b = 20)
      ),
      plot.caption.position = "plot",
      plot.caption = element_markdown(
        family = "sg",
        color = color_main,
        hjust = 0,
        lineheight = 1.2,
        size = 8
      ),
      plot.margin = margin(t = 1.5, l = 0.5, r = 0.5, b = 1, unit = "cm"),
      plot.background = element_rect(fill = color_bg, color = NA)
    )
  )

ggsave(
  "2025_w01-year_in_movies.png",
  final_plot,
  width = 21,
  height = 29.7,
  units = "cm"
)
