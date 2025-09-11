# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggradar)
library(patchwork)
library(ggtext)
library(showtext)


# data -------------------------------------------------------------------

# Downloading the data
tt <- tidytuesdayR::tt_load(2025, week = 11)

palmtrees <- tt$palmtrees

# Growth form and habit
growth_data <-
  palmtrees |>
  summarise(
    # column names are formated because they will be used by ggradar
    # for the axis text
    # both and climbing are considered as TRUE
    Climbing = mean(climbing %in% c("climbing", "both"), na.rm = TRUE),
    "Acaulescent\ngrowth" = mean(
      acaulescent %in% c("acaulescent", "both"),
      na.rm = TRUE
    ),
    "Erect\nstem" = mean(erect %in% c("erect", "both"), na.rm = TRUE),
    "Solitary\nstem" = mean(
      stem_solitary %in% c("stem_solitary", "both"),
      na.rm = TRUE
    ),
    species_num = n(),
    .by = palm_subfamily
  ) |>
  mutate(
    across(
      !c(palm_subfamily, species_num),
      # Normalize data from 0 to 1
      \(x) scales::rescale(x, to = c(0, 1))
    )
  ) |>
  # nest data by palm_subfamily to create multiple plots
  nest(data = !c(palm_subfamily, species_num)) |>
  # Reorder subfamilies by number of species
  arrange(desc(species_num)) |>
  # Nest subfamilies and species so we can add the number of species later
  nest(subfamilies = !data)


# Colors -----------------------------------------------------------------

color_green <- "#5d9865"
color_green_darker <- "#1a3f2c"
color_black <- "#080813"
color_black1 <- colorspace::lighten(color_black, 0.7)
color_white <- "#f8f4ed"


# Text and Captions ------------------------------------------------------

# Texts

chart_title <- "Palm Trees' Growth Forms and Habits"
chart_subtitle <- "Each chart depicts the essential growth habits across five palm tree subfamilies, representing over 2500 species"

# Captions

data <- glue::glue("**Data**: {{palmtrees}}")
chart <- glue::glue("<b>#TidyTuesday</b>: 2025 Week 11 | {data}")
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart} | {author} | #rstats")
# Fonts ------------------------------------------------------------------

showtext_auto()
showtext_opts(dpi = 300)


font_add_google("Space Grotesk", "space_gr")

font_add(
  "fa6-brands",
  "w01/fonts/Font Awesome 6 Brands-Regular-400.otf"
)

# Plot -------------------------------------------------------------------

theme_set(theme_minimal(base_family = "space_gr"))

# A function to create a radar chart
make_radar <- function(dataset, ...) {
  # ggradar specs
  ggradar(
    dataset,
    font.radar = "space_gr",
    base.size = 12,
    grid.line.width = 0.3,
    axis.line.colour = color_black1,
    axis.label.offset = 1.28,
    axis.label.size = 3.8,
    plot.extent.y.sf = 1.5,
    legend.position = "none",
    label.gridline.min = FALSE,
    label.gridline.mid = FALSE,
    label.gridline.max = FALSE,
    background.circle.colour = color_white,
    gridline.min.colour = color_black1,
    gridline.mid.colour = color_black1,
    gridline.max.colour = color_black1,
    group.line.width = 1,
    group.point.size = 2.6,
    background.circle.transparency = 0,
    fill = TRUE,
    ...
  ) +
    # !important for the result
    coord_equal(
      clip = "off"
    ) +
    theme(
      plot.background = element_rect(color = color_white, fill = color_white),
      panel.background = element_rect(color = color_white, fill = color_white),
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        face = "bold",
        color = color_green_darker,
        lineheight = 1,
        halign = 0.5,
        size = 18
      ),
      plot.margin = margin(20, 60, 20, 20)
    )
}

# Create a plot for each subfamilies
plots <- growth_data |>
  mutate(
    plot = map2(data, subfamilies, function(df, grp) {
      df |>
        mutate(
          # ggradar uses the first column for grouping
          x = row_number(),
          .before = 1
        ) |>
        make_radar(
          plot.title = glue::glue(
            "{grp$palm_subfamily}<br>",
            "<span style='font-size:12pt'>({grp$species_num} species)</span>"
          ),
          group.colours = color_green
        )
    })
  ) |>
  pull(plot)

# ggradar uses geom_text() for the axis labels
# this update them
update_geom_defaults(
  "text",
  list(
    color = color_green_darker,
    family = "space_gr",
    lineheight = 1
  )
)

# Final plot
final_plot <- wrap_plots(plots, nrow = 2, ncol = 3) +
  plot_annotation(
    title = chart_title,
    subtitle = chart_subtitle,
    caption = caption_text,
    theme = theme(
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        face = "bold",
        halign = 0.5,
        size = 28,
        color = color_green_darker,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_textbox_simple(
        color = color_green_darker,
        halign = 0.5,
        size = 12,
        margin = margin(b = 12)
      ),
      plot.caption.position = "plot",
      plot.caption = element_textbox_simple(
        color = color_green_darker,
        size = 9,
        halign = 0.5,
        margin = margin(t = 12)
      ),
      plot.margin = margin(20, 20, 20, 20)
    )
  ) +
  plot_layout(
    # todo: This need to be updated (replacement length warning)
    design = c(
      area(1, 1),
      area(1, 2),
      area(1, 3),
      area(2, 1.5, r = 2),
      area(2, 2.5, r = 3)
    )
  )


ggsave(
  "2025_w11_palmtrees.png",
  final_plot,
  units = "cm",
  width = 29.1,
  height = 21
)
