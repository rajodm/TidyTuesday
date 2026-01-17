# Packages ---------------------------------------------------------------

library(tidyverse)
library(rnaturalearth)
library(ggrepel)
library(sf)
library(patchwork)
library(ggtext)
library(systemfonts)

# Misc -------------------------------------------------------------------
# Add fa-brands
register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

# Colors
color_light_blue <- "#f0f4f9"
color_blue <- "#d4e4f0"
color_white <- "#f8fafb"
color_light_gray <- "#e8ecf1"
color_gray <- "#b8c5cd"
color_black <- "#333333"

# Caption
chart <- glue::glue(
  "**#TidyTuesday**: 2026 Week 02"
)

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
notes <- glue::glue(
  "**Notes**: Language data for the two Congo countries are combined & ",
  "only the inner islands of the Seychelles are displayed.",
  "<br>",
  "Inset maps are enlarged for visibility and not shown at the same scale as the main map.",
  "<br>",
  "**Base map**: {{rnaturalearth}} | **projection**: EPSG: 27701",
  "<br>",
  "**Source**: Languages of Africa, Wikipedia |",
)

caption_text <- glue::glue("{notes} {chart} | {author} | #rstats")

# Base Theme
set_theme(theme_void(paper = color_blue, ink = color_black))

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, week = 2)
africa <- tt$africa

af_lang_fam <- africa |>
  summarize(
    .by = c(country, family),
    n_lang = n(),
  ) |>
  slice_max(n_lang, n = 1, by = country, with_ties = FALSE) |>
  mutate(
    country = case_match(
      country,
      # Match geounit in from the base map: {rnaturalearth}
      "Eswatini" ~ "eSwatini",
      "Cape Verde" ~ "Cabo Verde",
      .default = country
    )
  )

world <- ne_countries(10)

bbox <- st_bbox(
  c(
    xmin = -38,
    xmax = 68,
    ymin = -38,
    ymax = 43
  ),
  crs = st_crs(4326)
)

world_proj <- world |>
  st_transform(crs = 27701)

bbox_proj <- bbox |>
  st_as_sfc() |>
  st_transform(crs = 27701) |>
  st_bbox()

world_cropped <- world_proj |>
  st_crop(bbox_proj)

non_af <- world_cropped |>
  filter(region_un != "Africa")

af <- world_cropped |>
  filter(region_un == "Africa")

congo_lang_fam <- af_lang_fam |>
  filter(country == "Congo") |>
  pull(family)

map_af_lang <- af |>
  left_join(af_lang_fam, by = c("geounit" = "country")) |>
  select(geounit, family) |>
  mutate(
    family = case_when(
      str_detect(geounit, "[Cc]ongo") ~ congo_lang_fam,
      is.na(family) ~ "No data",
      TRUE ~ family
    )
  )

# bbox for the inset maps

# Seychelles bbox
syc_bb_inner <-
  st_bbox(
    c(
      xmin = 55.2,
      ymin = -5,
      xmax = 55.9,
      ymax = -4
    ),
    crs = 4326
  ) |>
  st_as_sfc() |>
  st_transform(crs = st_crs(27701)) |>
  st_bbox()

# Cabo Verde bbox
cpv_bb <- af |>
  filter(geounit == "Cabo Verde") |>
  st_bbox()

# Mauritius bbox
mus_bb <- af |>
  filter(geounit == "Mauritius") |>
  st_bbox()

# Comoros bbox
com_bb <- af |>
  filter(geounit == "Comoros") |>
  st_bbox()

# Get the centroid for positioning labels
map_labs <-
  map_af_lang |>
  st_centroid() |>
  st_coordinates() |>
  as_tibble() |>
  bind_cols(
    map_af_lang |>
      st_drop_geometry()
  )

# Maps -------------------------------------------------------------------

# Palette
pal <- c(
  "Niger–Congo" = "#6b9bc3", #use an em-dash
  "Afroasiatic" = "#e8a87c",
  "Nilo-Saharan" = "#a3be8c",
  "Austronesian" = "#d4a5a5",
  "Portuguese" = "#9d8fb5",
  "French" = "#558b93",
  "Indo-European" = "#b88a6f",
  "No data" = "#d0d8e0"
)

# Add levels for key order in the legend
levels <- c(
  "Niger–Congo",
  "Afroasiatic",
  "Nilo-Saharan",
  "Austronesian",
  "Portuguese",
  "French",
  "Indo-European",
  "No data"
)

# base map
base_map <- ggplot() +
  geom_sf(
    data = non_af,
    fill = color_white,
    color = color_light_gray,
    linewidth = .1,
    show.legend = FALSE
  ) +
  geom_sf(
    data = map_af_lang,
    aes(fill = factor(family, levels = levels)),
    color = color_gray,
    linewidth = .08,
  ) +
  scale_fill_manual(values = pal)

# Helper function for the inset maps
make_inset_map <- function(xmin, xmax, ymin, ymax) {
  base_map +
    coord_sf(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
    ) +
    theme_sub_plot(
      background = element_rect(fill = color_light_blue, color = "#333333")
    ) +
    theme_sub_panel(
      background = element_rect(fill = color_light_blue, color = "#333333")
    ) +
    theme_sub_legend(position = "none")
}

# Cabo Verde
cpv_map <- make_inset_map(
  cpv_bb["xmin"],
  cpv_bb["xmax"],
  cpv_bb["ymin"],
  cpv_bb["ymax"]
)

# Mauritius (use a custom ymax)
mus_map <- make_inset_map(
  mus_bb["xmin"],
  mus_bb["xmax"],
  mus_bb["ymin"],
  2815000
)

# Seychelles' inner islands
syc_map_inner <- make_inset_map(
  syc_bb_inner["xmin"],
  syc_bb_inner["xmax"],
  syc_bb_inner["ymin"],
  syc_bb_inner["ymax"]
)

# Comoros
com_map <- make_inset_map(
  com_bb["xmin"],
  com_bb["xmax"],
  com_bb["ymin"],
  com_bb["ymax"]
)

# Africa
map <- base_map +
  geom_text_repel(
    data = map_labs,
    aes(
      x = X,
      y = Y,
      label = str_wrap(geounit, 16)
    ),
    family = "Inter",
    lineheight = .8,
    segment.size = .2,
    direction = "y",
    max.overlaps = 6,
    alpha = .8,
    min.segment.length = .3,
    color = color_black,
    size = 2,
    seed = 123
  ) +
  labs(
    caption = caption_text,
    fill = str_wrap("Dominant Language Families in Africa", 24)
  ) +
  coord_sf(expand = FALSE, clip = "on") +
  guides(fill = guide_legend(ncol = 2)) +
  theme_sub_legend(
    title = element_text(
      size = 16,
      face = "bold",
      family = "Atkinson Hyperlegible Next",
      color = color_black,
      hjust = .5,
      margin = margin(b = 12)
    ),
    text = element_text(
      size = 9,
      color = color_black,
      family = "Atkinson Hyperlegible Next"
    ),
    key.spacing.y = unit(2.5, "pt"),
    key.height = unit(12, "pt"),
    key.width = unit(18, "pt"),
    position = "inside",
    position.inside = c(.2, .32)
  ) +
  theme_sub_plot(
    background = element_rect(fill = color_blue, color = NA),
    caption.position = "panel",
    caption = element_textbox_simple(
      family = "Atkinson Hyperlegible Next",
      size = 6,
      halign = 0,
      margin = margin(b = 2, l = 3.4),
      color = colorspace::lighten(color_black, .2)
    ),
    margin = margin(0, 0, 0, 0)
  ) +
  theme_sub_panel(background = element_rect(fill = color_blue, color = NA))

# Combined maps
final_map <- map +
  # Mauritius
  inset_element(
    mus_map,
    left = .94,
    right = .96,
    top = .265,
    bottom = .225,
    align_to = "full",
    clip = FALSE
  ) +
  # Comoros
  inset_element(
    com_map,
    left = .79,
    right = .84,
    bottom = .35,
    top = .42,
    align_to = "full",
    clip = FALSE
  ) +
  # Seychelles Inner Islands
  inset_element(
    syc_map_inner,
    left = .906,
    right = .996,
    bottom = .36,
    top = .423,
    align_to = "full",
    clip = FALSE
  ) +
  # Cabo Verde
  inset_element(
    cpv_map,
    left = .05,
    right = .12,
    bottom = .58,
    top = .652,
    align_to = "full",
    clip = FALSE
  ) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = color_blue, color = NA),
      panel.background = element_rect(fill = color_blue, color = NA),
    )
  )

ggh4x::save_plot(
  plot = final_map,
  "2026_w02-languages_of_africa.png",
  height = 8,
  width = 7.5,
  units = "in",
  dpi = 300
)
