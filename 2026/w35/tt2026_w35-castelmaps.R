# Packages ---------------------------------------------------------------

library(dplyr)
library(purrr)
library(stringr)
library(sf)
library(grid)
library(tmap)

source(here::here("cap_func.R"))
tmap_options(component.autoscale = FALSE)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, week = 35)
castles <- tt$world_castles

castles_fr <-
  castles |>
  filter(country == "France") |>
  st_as_sf(coords = c("lon", "lat"), crs = st_crs(4326))

map_base_fr <- giscoR::gisco_get_nuts(
  country = "FR",
  nuts_level = 2,
  resolution = 10
)

map_base_fr3 <-
  giscoR::gisco_get_nuts(
    country = "FR",
    nuts_level = 3,
    resolution = 10
  )

map_base_fr_metrop <- st_crop(
  map_base_fr,
  xmin = -5,
  xmax = 10,
  ymin = 41,
  ymax = 51
)

map_base_fr_metrop3 <- st_crop(
  map_base_fr3,
  xmin = -5,
  xmax = 10,
  ymin = 41,
  ymax = 51
)

castles_fr_metrop <-
  st_crop(
    castles_fr,
    map_base_fr_metrop
  ) |>
  st_join(map_base_fr_metrop["NUTS_NAME"]) |>
  mutate(
    region = NUTS_NAME,
    region = replace_when(
      region,
      name == "Château d'If" ~ "Provence-Alpes-Côte d’Azur"
    )
  ) |>
  st_transform(crs = st_crs(3035))

n_fr_metrop <- nrow(castles_fr_metrop)

frames <-
  castles_fr_metrop |>
  filter(fame_rank <= 100) |>
  group_by(region) |>
  group_modify(
    ~ {
      bbox <- st_bbox(.x)
      st_sf(geometry = st_as_sfc(bbox))
    }
  ) |>
  ungroup() |>
  st_as_sf() |>
  st_buffer(20000)

map_base_fr_metrop <- map_base_fr_metrop |>
  st_transform(st_crs(3035))

map_base_fr_metrop3 <- map_base_fr_metrop3 |>
  st_transform(st_crs(3035))

# Miscs ------------------------------------------------------------------

col_paper <- "#faf7f0"
col_black <- "#04050a"
col_ink <- "#13315f"
col_green <- "#b1a1cc"
col_orange <- "#B24422"
col_yellow <- "#59386c"

caption_text <- generate_caption("Castlemap, curated from Wikidata", 35) |>
  stringr::str_replace_all("<br>", "\n") |>
  stringr::str_remove_all("\\*")

# Map --------------------------------------------------------------------

region_to_map <- unique(frames$region)

insets <-
  region_to_map |>
  set_names() |>
  map(\(r) {
    bbox_r <- frames |>
      filter(region == r) |>
      st_bbox()
    pts_r <- castles_fr_metrop |>
      filter(region == r, fame_rank <= 100)

    pts_r_info <- pts_r |>
      mutate(lat = st_coordinates(geometry)[, "Y"]) |>
      arrange(desc(lat)) |>
      st_drop_geometry() |>
      summarise(
        .by = region,
        n = n(),
        names = stringr::str_flatten(name, collapse = "\n")
      )

    tm_shape(map_base_fr_metrop3, bbox = bbox_r) +
      tm_polygons(
        fill = col_green,
        col = col_yellow,
        lwd = 1
      ) +
      tm_shape(pts_r) +
      tm_symbols(
        shape = 42,
        col = col_orange,
        size = 1,
        lwd = 0.5
      ) +
      tm_title(
        glue::glue("{r} ({pts_r_info$n})") |>
          str_wrap(28),
        size = 0.8,
        color = col_orange,
        padding = 0,
        fontfamily = "Recursive Casual",
        fontface = "bold",
        just = "left",
        position = tm_pos_in("left", "top")
      ) +
      tm_title(
        pts_r_info$names,
        size = 0.62,
        col = col_black,
        fontfamily = "Atkinson Hyperlegible Next",
        position = tm_pos_in("left", "bottom")
      ) +
      tm_layout(
        bg.color = col_paper
      )
  })

tm <- tm_shape(map_base_fr_metrop) +
  tm_polygons(
    fill = col_green,
    col = col_yellow,
    lwd = 1
  ) +
  tm_shape(castles_fr_metrop) +
  tm_symbols(
    shape = 20,
    fill = col_ink,
    size = 0.35,
    fill_alpha = 0.7
  ) +
  tm_shape(
    castles_fr_metrop |>
      filter(fame_rank <= 100)
  ) +
  tm_symbols(
    col = col_yellow,
    fill = col_orange,
    size = 0.45,
    lwd = 0.5
  ) +
  tm_title(
    "3 Regions\n13 famous places",
    position = tm_pos_out("right", "center"),
    size = 1.1,
    fontface = "bold",
    fontfamily = "Recursive Casual",
    color = col_orange
  ) +
  tm_inset(
    insets$`Ile-de-France`,
    frame = FALSE,
    box_frame.color = col_orange,
    height = 10,
    width = 10,
    position = tm_pos_out("right", "center")
  ) +
  tm_inset(
    insets$`Centre — Val de Loire`,
    frame = FALSE,
    box_frame.color = col_orange,
    height = 10,
    width = 10,
    position = tm_pos_out("right", "center")
  ) +
  tm_inset(
    insets$`Provence-Alpes-Côte d’Azur`,
    frame = FALSE,
    box_frame.color = col_orange,
    height = 10,
    width = 10,
    position = tm_pos_out("right", "center")
  ) +
  tm_title_out(
    "France has 13 places listed on Castlemap's global top 100 ranking",
    fontface = "bold",
    fontfamily = "Recursive Casual",
    stack = "vertical",
    color = col_black,
    size = 1.3,
    width = 200
  ) +
  tm_title_out(
    str_wrap(
      glue::glue(
        "France (metropolitan region) has {n_fr_metrop} castles/fortresses/ruins listed on Castelmap. Thirteen of them (more than any other country) are in the top 100 ranking. Each dot on this map is one of them, and the famous ones are highlighted."
      ),
      110
    ),
    fontfamily = "Atkinson Hyperlegible Next",
    color = col_black,
    size = 0.9
  ) +
  tm_credits(
    caption_text,
    color = col_black,
    size = 0.6,
    fontfamily = "Atkinson Hyperlegible Next",
    position = tm_pos_in("LEFT", "BOTTOM")
  ) +
  tm_layout(
    bg.color = col_paper,
    outer.bg.color = col_paper,
    panel.show = FALSE,
    frame = FALSE
  )

tmap_save(
  tm,
  "tt/2026/w35/2026_w35-castelmaps.png",
  device = ragg::agg_png,
  width = 25,
  height = 21,
  units = "cm",
  dpi = 600
)
