# Packages ---------------------------------------------------------------

library(tidyverse)
library(rnaturalearth)
library(terra)
library(sf)
library(marmap)
library(magick)

# Data -------------------------------------------------------------------

wreck_inventory <- readr::read_csv(here::here(
  "project/tt2026_w26/data/wreck_inventory.csv"
))

wreck_loc <- wreck_inventory |>
  # keep only the columns with available coordinates
  dplyr::filter_out(dplyr::if_any(c(latitude, longitude), is.na)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# World map
shp_world <- rnaturalearth::ne_countries(scale = 10) |>
  sf::st_make_valid()

# Set a borderbox based on the locations in the data
bbox <- sf::st_bbox(wreck_loc) |>
  sf::st_as_sfc() |>
  sf::st_buffer(5e5)

map_bb <- sf::st_bbox(bbox)

# Projected bbox
map_bb_proj <- map_bb |>
  sf::st_crs(2157)

# Crop to the region to map
base_map <- shp_world |>
  sf::st_crop(bbox)

# Bathymetry data
bathy <- marmap::getNOAA.bathy(
  map_bb$xmin,
  map_bb$xmax,
  map_bb$ymin,
  map_bb$ymax,
  keep = TRUE,
  resolution = 1,
  # path = "bathy/"
)

# Convert to raster
bathy_rast <- marmap::as.raster(bathy)
# Convert to spatraster
bathy_rast <- terra::rast(bathy_rast)
# Add crs
terra::crs(bathy_rast) <- "EPSG:4326"


# Miscs ------------------------------------------------------------------

color_black <- "#002233"
color_green <- "#ddff55"
color_white <- "#f6f2e8"
# Map projection ---------------------------------------------------------

bathy_proj <- terra::project(bathy_rast, "EPSG:2157")

base_map_proj <- base_map |>
  st_transform("EPSG:2157")

wreck_loc_proj <- wreck_loc |>
  st_transform("EPSG:2157")

# bathy_proj[bathy_proj >= 0] <- NA

# Map --------------------------------------------------------------------

tm <- tmap::tm_shape(bathy_proj) +
  tmap::tm_raster(
    col.scale = tmap::tm_scale_continuous(
      values = cols4all::c4a("kovesi.ternary_blue")
    ),
    col.legend = tmap::tm_legend_hide(),
  ) +
  tmap::tm_shape(base_map_proj) +
  tmap::tm_polygons(fill = color_black, col = color_black) +
  tmap::tm_shape(
    wreck_loc_proj |>
      mutate(
        rank = row_number(),
        rank = max(rank) - rank
      )
  ) +
  tmap::tm_symbols(
    shape = 20,
    fill = color_green,
    size = 0.08,
    fill_alpha = "rank",
    fill_alpha.scale = tmap::tm_scale_continuous(values.range = c(0.4, 0.8)),
    fill.legend = tmap::tm_legend_hide(),
    fill_alpha.legend = tmap::tm_legend_hide()
  ) +
  tmap::tm_layout(
    bg.color = color_black,
    outer.bg.color = color_black,
    inner.margins = rep(0, 4),
    outer.margins = rep(0, 4),
    frame = FALSE,
    panel.show = FALSE
  )

tmap::tmap_save(
  tm,
  "base.svg",
  device = svglite::svglite,
  height = 15.45,
  width = 25,
  units = "cm",
  dpi = 600
)


# {magick} ---------------------------------------------------------------

# Scale for 600dpi
scale_fct <- 600 / 72

img_width <- round(708 * scale_fct) # 708 is the original width

img <- magick::image_read_svg("base.svg", width = img_width)

# for a 320x250 format
crop_w <- round(320 * scale_fct)
crop_h <- round(250 * scale_fct)

cropped_img <- magick::image_crop(
  img,
  glue::glue("{crop_w}x{crop_h}+10+60"),
  gravity = "center"
)

caption <- glue::glue(
  "Location of {nwreck} shipwrecks across Ireland's coastal waters between {min(wreck_loc_proj$year, na.rm = TRUE)} & {max(wreck_loc_proj$year, na.rm = TRUE)}. ",
  "Points color indicates wreck date (brighter = more recent), water color indicates depth (darker blue = deeper).",
  nwreck = nrow(wreck_loc_proj) |>
    scales::number(big.mark = ",")
)

final_chart <- cropped_img |>
  magick::image_annotate(
    text = "IRELAND SHIPWRECKS",
    size = 82,
    font = "Fraunces",
    decoration = "underline",
    color = color_green,
    gravity = "southwest",
    location = "+50+650"
  ) |>
  magick::image_annotate(
    stringr::str_wrap(caption, 55),
    size = 38,
    font = "Atkinson Hyperlegible Mono",
    gravity = "southwest",
    color = color_white,
    location = "+50+400"
  ) |>
  magick::image_annotate(
    stringr::str_wrap(
      "Bathymetric data: NOAA (via the {marmap} R package).",
      55
    ),
    size = 38,
    font = "Atkinson Hyperlegible Mono",
    gravity = "southwest",
    color = color_white,
    location = "+50+300"
  ) |>
  magick::image_annotate(
    stringr::str_wrap("Coastline: {rnaturalhearth}.", 55),
    size = 38,
    font = "Atkinson Hyperlegible Mono",
    gravity = "southwest",
    color = color_white,
    location = "+50+250"
  ) |>
  magick::image_annotate(
    stringr::str_wrap("Projection:  ITM (EPSG:2157).", 55),
    size = 38,
    font = "Atkinson Hyperlegible Mono",
    gravity = "southwest",
    color = color_white,
    location = "+50+200"
  ) |>
  magick::image_annotate(
    stringr::str_wrap("Data:  Wreck Inventory of Ireland Database (WIID).", 55),
    size = 38,
    font = "Atkinson Hyperlegible Mono",
    gravity = "southwest",
    color = color_white,
    location = "+50+150"
  ) |>
  magick::image_annotate(
    stringr::str_wrap(
      "#Tidytuesday: 2026 week 26 | Visualization: Andriambelo Rajo | #rstat | #tmap",
      65
    ),
    size = 38,
    font = "Atkinson Hyperlegible Mono",
    gravity = "southwest",
    color = color_white,
    location = "+50+50"
  )

magick::image_write(
  final_chart,
  "2026/w26/2026_w26-ireland_shipwrecks.png",
  quality = 100
)

# fs::file_delete("base.svg")
