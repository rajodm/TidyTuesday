# Heavily inspired from Milos Agathon tutorial on Tanaka elevation maps
# Code here: https://github.com/milos-agathon/tanaka-elevation-maps
# Packages -------------------------------------------------------

library(tidyverse)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggspatial)
library(metR)
library(colorspace)
library(ggtext)
library(ggforce)
library(systemfonts)

# Fonts ----------------------------------------------------------

register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

# Colors ---------------------------------------------------------
color_black <- "#100f0f"
color_white <- "#fffcf0"
color_brown_dark <- "#2a1f1a"
color_brown_light <- "#5a4a42"
color_brown_light1 <- "#6b5b52"

# data -----------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 33)

sc_munros <- tt$scottish_munros

# UK Boundaries
gbr_bounds <- ne_states(country = "United Kingdom", returnclass = "sf")

# Get Scotland boundaries
sct_bounds <- gbr_bounds |>
  filter(gu_a3 == "SCT")

# Download Scotland elevation
dem_rast <- elevatr::get_elev_raster(
  sct_bounds,
  z = 7,
  clip = "location"
)

# Projection
proj <- "EPSG:27700"

sct_rast_proj <-
  dem_rast |>
  rast() |>
  project(proj)

sc_munros_sf <- sc_munros |>
  janitor::clean_names() |>
  pivot_longer(
    cols = x1891:x2021,
    names_to = "year",
    values_to = "classification",
    names_transform = parse_number
  ) |>
  filter(year == 2021 & !is.na(xcoord)) |>
  st_as_sf(coords = c("xcoord", "ycoord"), crs = st_crs(27700))

top_munros <- sc_munros_sf |>
  slice_max(order_by = height_m, n = 3)

top_munros_df <- top_munros |>
  mutate(
    .by = do_bih_number,
    name = str_trim(str_remove(name, "\\[.*\\]")),
    elevation = height_m,
    x = st_coordinates(geometry)[1],
    y = st_coordinates(geometry)[2],
    .keep = "used"
  ) |>
  mutate(row_num = row_number()) |>
  st_drop_geometry()

# Setting border box
bbox <- st_bbox(sc_munros_sf)
# Add 30000km buffer
buffer <- 3e4
new_bbox <- bbox |>
  st_as_sfc() |>
  st_buffer(dist = buffer) |>
  st_bbox()

new_rast <- sct_rast_proj |>
  crop(new_bbox)

# Transform to a Data Frame
sct_df <-
  as.data.frame(new_rast, xy = TRUE)

colnames(sct_df)[3] <- "elevation"

# Convert to a tibble (optional?)
sct_tibble <-
  sct_df |>
  as_tibble()


# Texts ----------------------------------------------------------

src <- glue::glue(
  "**Source**: Database of British and Irish hills"
)

chart <- glue::glue(
  "**#TidyTuesday**: 2025 Week 33"
)

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{src} <br> {chart} | {author} | #rstats")
# Limits ---------------------------------------------------------

lim <- range(sct_tibble$elevation, na.rm = TRUE)
# breaks for the color scale
breaks <- seq(
  floor(lim[1] / 50) * 50,
  ceiling((lim[2] + 10) / 50) * 50,
  by = 250
)

# Theme ----------------------------------------------------------
# Set themes to element blank
# Trick from @jdonland.bsky.social
# https://bsky.app/profile/jdonland.bsky.social/post/3lrto2da6tk24

blank_elements <- function(theme_elements) {
  rep(alist(element_blank()), length(theme_elements)) |>
    set_names(theme_elements) |>
    do.call(theme, args = _)
}

# Theme elements to remove
el_to_rmv <- c("axis.line", "panel.grid", "axis.title", "axis.text")

# Custom theme for the chart
custom_theme <- function(unwanted_elements) {
  theme_minimal(base_size = 11, base_family = "Atkinson Hyperlegible Next") +
    theme(
      text = element_text(color = color_black),
      plot.background = element_rect(fill = color_white, color = NA),
      plot.title.position = "plot",
      plot.title = element_textbox_simple(
        size = 18,
        family = "Work Sans",
        face = "bold",
        color = color_brown_dark,
        margin = margin(t = 10, b = 3, l = 20)
      ),
      plot.subtitle = element_textbox_simple(
        size = 10,
        color = color_brown_light,
        margin = margin(b = 10, l = 20)
      ),
      plot.caption.position = "plot",
      plot.caption = element_textbox_simple(
        size = 6,
        margin = margin(t = 10),
        halign = 1,
        lineheight = 1,
        color = color_brown_light1
      ),
      plot.margin = margin(t = 10, r = 3, b = 5, l = 3),
    ) +
    blank_elements(unwanted_elements)
}

# Palettes -------------------------------------------------------
# Generate a color palette with 6 colors
pal <- flexoki::flex(
  col = c("blue", "green", rep("yellow", 2), rep("orange", 3)),
  c(50, 100, seq(200, 800, 200), 950)
)

# Colors to use for the shadows in the plot
light_col <- lighten(pal[2], amount = .1)
dark_col <- darken(pal[5], amount = .25)

# Helper function for tooltips
add_circle_mark <- function(data, row_num, x0, y0) {
  geom_mark_circle(
    data = data[row_num, ],
    aes(
      x = x,
      y = y,
      x0 = x0,
      y0 = y0,
      label = glue::glue("{row_num}. {name}\n({height_m} m)")
    ),
    label.fontface = "plain",
    label.family = "Atkinson Hyperlegible Next",
    label.hjust = .5,
    label.fontsize = 11,
    label.fill = "white",
    label.colour = color_black,
    con.colour = "white",
    con.size = .6,
    colour = "white",
    expand = 0,
    con.cap = unit(.4, "mm"),
  )
}

# plot -----------------------------------------------------------

tanaka_plt <- sct_tibble |>
  ggplot(aes(x = x, y = y, z = elevation)) +
  geom_contour_fill(breaks = breaks) +
  geom_contour_tanaka(
    breaks = breaks,
    sun.angle = 45,
    light = light_col,
    dark = dark_col,
    range = c(.01, .3),
    smooth = .8
  ) +
  add_circle_mark(top_munros_df, 1, 216600, 770000) +
  add_circle_mark(top_munros_df, 2, 298900, 798000) +
  add_circle_mark(top_munros_df, 3, 295300, 850000) +
  # Add a scale bar
  annotation_scale(
    pad_x = unit(20, "pt"),
    pad_y = unit(.15, "cm"),
    height = unit(.2, "cm"),
    style = "ticks",
    location = "bl",
    bar_cols = c(color_black, "white"),
    text_cex = .6,
    width_hint = .2
  ) +
  scale_fill_gradientn(
    name = "Elevation (m)",
    colors = pal,
    limits = lim,
    guide = guide_colorbar(
      title.position = "top",
      title.just = .5,
      ticks = FALSE,
      barheight = unit(5, "cm"),
      frame.colour = NA
    )
  ) +
  coord_sf(crs = proj) +
  labs(
    title = "Scottish Munros",
    subtitle = "Scotland's Three Highest Peaks",
    caption = caption_text
  ) +
  # Add the custom theme
  custom_theme(el_to_rmv)

ggsave(
  "2025_w33-scottish_munros.png",
  tanaka_plt,
  height = 7,
  width = 7,
  dpi = 900
)
