# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggstream)
library(showtext)
library(ggtext)


# Load data --------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 12)
amz_words <- tt$report_words_clean


# Data wrangling ---------------------------------------------------------

plot_data <-
  amz_words |>
  summarize(
    count = n(),
    .by = c(year, word)
  ) |>
  # Extract the 5 most frequent words for each year
  slice_max(
    n = 5,
    by = year,
    order_by = count
  ) |>
  mutate(
    word = str_to_title(word),
    prop = count / sum(count),
    .by = year
  ) |>
  arrange(desc(year), desc(count))


# Colors -----------------------------------------------------------------

color_white <- "#f4f0ec"
color_black <- "#171717"
color_light_black <- colorspace::lighten(color_black, 0.3)

pal <- paletteer::paletteer_d("palettetown::squirtle") |>
  discard(\(x) x %in% c("#407060FF", "#902000FF", "#602800FF"))


# Fonts ------------------------------------------------------------------

font_add_google("Poppins", "poppins", bold.wt = 700)

font_add(
  "fa6-brands",
  "2025/w01/fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 300)

# Texts ------------------------------------------------------------------

plot_title <- "Word Frequency in Amazon Annual Reports"
plot_subtitle <- "Proportional representation of frequent terms in Amazon's annual reports from 2005 to 2023"


# captions ---------------------------------------------------------------

data <- glue::glue("**Data**: Amazon's Annual Reports")
chart <- glue::glue("<b>#TidyTuesday</b>: 2025 Week 12 | {data}")
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart}<br>{author} | #rstats")


# Plot -------------------------------------------------------------------

theme_set(theme_minimal(base_size = 14, base_family = "poppins"))

p <- plot_data |>
  ggplot(aes(year, prop, fill = word)) +
  geom_stream(
    type = "proportional",
    n_grid = 3000,
    bw = .78,
    # Just like that XD
    extra_span = 0.618
  ) +
  geom_stream_label(
    aes(label = word),
    type = "proportional",
    n_grid = 600
  ) +
  scale_x_continuous(breaks = seq(2005, 2023, 4)) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    labels = scales::label_percent(suffix = "")
  ) +
  scale_fill_manual(values = pal) +
  coord_cartesian(
    clip = "off",
    expand = FALSE
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    y = "Word usage proportion in %",
    caption = caption_text
  ) +
  theme(
    plot.background = element_rect(fill = color_white, color = color_white),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 22,
      face = "bold",
      margin = margin(b = 8)
    ),
    plot.subtitle = element_textbox_simple(
      color = colorspace::lighten(color_black, 0.2),
      size = 14,
      margin = margin(b = 24)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      size = 7.5,
      hjust = 1,
      halign = 1,
      margin = margin(t = 20, b = 10)
    ),
    axis.text.x = element_text(
      size = 14,
      color = color_light_black,
      margin = margin(t = 6)
    ),
    axis.title.x = element_blank(),
    axis.title.y = element_text(
      color = color_light_black,
      size = 14
    ),
    axis.text.y = element_text(
      color = color_light_black,
      size = 12,
      margin = margin(r = 6)
    ),
    plot.margin = margin(25, 25, 10, 25)
  )

update_geom_defaults(
  "text",
  list(
    size = 6,
    color = color_black,
    face = "bold",
    family = "poppins"
  )
)

ggsave(
  "2025_w12-amazon_s_annual_repports.png",
  p,
  width = 11,
  height = 8.5,
  units = "in"
)
