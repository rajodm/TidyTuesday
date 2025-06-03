# Packages ---------------------------------------------------------------

library(tidyverse)
library(treemapify)
library(ggblanket)
library(ggtext)
library(showtext)
library(patchwork)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 22)
g_meta <- tt$gutenberg_metadata

bookshelf <-
  g_meta |>
  separate_wider_delim(
    cols = gutenberg_bookshelf,
    delim = "/",
    names_sep = "_",
    too_few = "align_end",
  ) |>
  mutate(
    gutenberg_bookshelf_13 = str_remove_all(
      gutenberg_bookshelf_13,
      "[Bb]rowsing: "
    ),
  ) |>
  separate_wider_delim(
    cols = gutenberg_bookshelf_13,
    delim = " - ",
    names = c("bookshelf"),
    too_many = "drop"
  ) |>
  drop_na(bookshelf) |>
  mutate(
    bookshelf = fct_lump(bookshelf, n = 9)
  ) |>
  mutate(
    .by = bookshelf,
    language = fct_lump_n(language, n = 3),
    language = str_to_title(language),
  ) |>
  summarize(
    .by = c(bookshelf, language),
    count = n(),
  ) |>
  mutate(
    .by = bookshelf,
    total = sum(count),
    label = glue::glue("{bookshelf} ({total})"),
    lang_labs = glue::glue("{language}, {count}")
  )

# Fonts ------------------------------------------------------------------

font_add_google("Spectral", "spectral")
font_add_google("Inter", "inter")

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

# Texts ------------------------------------------------------------------

src <- glue::glue(
  "**Source**: Project Gutenberg - {{gutenbergr}} R package"
)

chart <- glue::glue(
  "**#TidyTuesday**: 2025 Week 22"
)

bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{src} | {chart} | {author} | #rstats")

showtext_auto()
showtext_opts(dpi = 420)

# Colors -----------------------------------------------------------------

color_white <- "#f8f7f5"
color_black <- "#1a1c23"
color_gray <- "#4b4b54"

# Plot -------------------------------------------------------------------

gtm <- bookshelf |>
  ggplot(aes(
    area = count,
    fill = bookshelf,
    subgroup = label,
    subgroup2 = lang_labs
  )) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_subgroup2_border(
    color = color_white,
    size = .3,
    alpha = .1,
    show.legend = FALSE
  ) +
  geom_treemap_subgroup2_text(
    place = "bottomright",
    reflow = TRUE,
    fontface = "bold",
    size = 9,
    color = "#ffffff",
    alpha = .5
  ) +
  geom_treemap_subgroup_text(
    aes_contrast(dark = color_black, light = color_white),
    reflow = TRUE,
    size = 20,
    fontface = "bold",
    place = "centre",
    alpha = .8,
  ) +
  labs(
    title = "Project Gutenberg Collections",
    subtitle = "Book Counts by Collection* (Color) and Language (Subgroups)",
    caption = "*Books appearing in multiple collections were counted only in their final listed collection."
  ) +
  paletteer::scale_fill_paletteer_d(
    "impressionist.colors::les_nympheas"
  ) +
  theme(
    text = element_text(family = "inter"),
    plot.background = element_rect(fill = color_white, color = NA),
    plot.margin = margin(10, 6, 0, 6),
    plot.title.position = "plot",
    plot.title = element_text(
      size = 24,
      hjust = .5,
      face = "bold",
      family = "spectral",
      color = color_black
    ),
    plot.subtitle = element_text(
      hjust = .5,
      family = "inter",
      color = color_gray,
      size = 12
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      family = "inter",
      size = 10,
      halign = 0,
      color = color_black,
      margin = margin(t = 6)
    )
  )

final_plot <- gtm +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.caption.position = "plot",
      plot.caption = element_textbox_simple(
        family = "inter",
        size = 8,
        halign = 1,
        color = color_gray,
        margin = margin(t = 12)
      )
    )
  )

ggsave(
  "2025_w22-project_gutenberg.png",
  final_plot,
  height = 21,
  width = 29.7,
  units = "cm",
  dpi = 420
)
