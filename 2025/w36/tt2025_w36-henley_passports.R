# Libraries ------------------------------------------------------

library(tidyverse)
library(ggflags)
library(ggbeeswarm)
library(ggtext)
library(systemfonts)


# Fonts ----------------------------------------------------------

register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

ahn <- "Atkinson Hyperlegible Next"
# Colors ---------------------------------------------------------

color_bg <- "#eeeae2"
color_black <- "#2d3c44"
color_gray <- "#c1c6bf"

# Data -----------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 36)

country_rank <- tt$country_rank

country_rank25 <- country_rank |>
  filter(year == 2025) |>
  arrange(desc(visa_free_count)) |>
  mutate(
    code = str_to_lower(code)
  )

#! I should probably use country_rank25 for this
top10 <- quantile(country_rank$visa_free_count, .9) |>
  as.numeric() |>
  round() #172
# top10 <- quantile(country_rank25$visa_free_count, .9) |>
#   as.numeric() |>
#   round() #186
top50 <- quantile(country_rank$visa_free_count, .5) #70
# top50 <- quantile(country_rank25$visa_free_count, .5) #94

# Captions -------------------------------------------------------

src <- glue::glue(
  "**Source**: Henley Passport Index Data"
)

chart <- glue::glue(
  "**#TidyTuesday**: 2025 Week 36"
)

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{src} <br> {chart} | {author} | #rstats")
# Plot -----------------------------------------------------------

p <- country_rank25 |>
  ggplot(aes(x = visa_free_count, y = region)) +
  geom_flag(
    aes(country = code),
    position = position_beeswarm(
      cex = 3,
      corral = "gutter",
      dodge.width = .9,
      corral.width = .9
    ),
    size = 10
  ) +
  geom_vline(xintercept = top10, linewidth = .9, color = color_black) +
  geom_vline(xintercept = top50, linewidth = .5, color = color_black) +
  scale_x_continuous(
    breaks = c(50, 100, 150, top10, 200),
    expand = expansion(add = c(15, 35))
  ) +
  labs(
    title = "Passport Strength by World Region",
    x = "Visa-free Access Count",
    caption = caption_text
  ) +
  coord_cartesian(clip = "off") +
  facet_wrap(~region, ncol = 1, scales = "free_y") +
  theme(
    text = element_text(color = color_black, size = 12, family = ahn),
    plot.background = element_rect(fill = color_bg, color = NA),
    panel.background = element_rect(fill = color_bg, color = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linetype = "31",
      linewidth = .5,
      color = color_gray
    ),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_text(hjust = 1, face = "bold", size = 12),
    strip.text = element_text(
      hjust = 0,
      face = "bold",
      size = 16,
      color = color_black
    ),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      size = 22,
      halign = .5,
      face = "bold",
      color = color_black,
      margin = margin(b = 12)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      size = 10,
      halign = 0,
      lineheight = 1,
      color = colorspace::lighten(color_black, .3),
      margin = margin(t = 8)
    ),
    plot.margin = margin(25, 15, 10, 15)
  )

annotation_data <- tibble(
  visa_free_count = top50,
  region = "AFRICA",
  label = glue::glue("Median Global Mobility →\n(median = {top50})")
)

top_countries <- tibble(
  visa_free_count = top10,
  region = 'AFRICA',
  label = str_wrap("Passports with Highest Global Mobility", width = 14)
)

final_plot <- p +
  geom_text(
    data = annotation_data,
    label = annotation_data$label,
    position = position_nudge(y = 1.3, x = 2),
    color = color_black,
    hjust = 0,
    lineheight = .9
  ) +
  geom_text(
    data = top_countries,
    label = top_countries$label,
    position = position_nudge(y = 1, x = 2),
    color = color_black,
    hjust = 0,
    lineheight = .9,
    fontface = "bold"
  )

update_geom_defaults(
  "text",
  list(family = ahn)
)

ggsave(
  "2025_w36-henley_passport_index.png",
  final_plot,
  width = 21,
  height = 29.7,
  units = "cm"
)
