# Packages ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(geomtextpath)
library(ggtext)
library(showtext)

# Data -------------------------------------------------------------------
events <- read_csv(here::here("project/tt2025_w19/data/vesuvius.csv"))
italy <- ne_countries(scale = "large", country = "italy", returnclass = "sf")

mt_vesuvius <- c(14.4260, 40.8219)
vesuvius_pt <- st_point(mt_vesuvius)
vesuvius_geom <- st_sfc(vesuvius_pt, crs = 4326)
buffer_distance <- 10000
vesuvius_buffer <- st_buffer(vesuvius_geom, dist = buffer_distance)

seismic_data_sf <- events |>
  drop_na(c(longitude, latitude, duration_magnitude_md)) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  mutate(
    distance = st_distance(geometry, vesuvius_geom),
    distance_km = as.numeric(distance) / 1000
  )

surface <- tibble(
  y = 0,
  label = "Surface"
)

# Fonts ------------------------------------------------------------------

font_add(
  "roboto",
  "fonts/Roboto-Regular.ttf",
  "fonts/RobotoBold.ttf"
)

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 300)

# Colors -----------------------------------------------------------------

color_gray <- "#4b4d54"
color_lightgray <- "#9aa0a6"
color_black <- "#1a1c23"
color_grid <- "#e2e3e7"
color_bg <- "#f8f7f5"
color_axis_text <- "#7e8087"

# Texts ------------------------------------------------------------------

src <- glue::glue(
  "**Source**: Italian Istituto Nazionale di Geofisica e Vulcanologia"
)
chart <- glue::glue(
  "**#TidyTuesday**: 2025 Week 19"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{src}<br>{chart} | {author} | #rstats")

vesuvius_desc <- tibble(
  x = 0,
  y = 0,
  label = glue::glue(
    "<span style = 'font-size: 14pt'>**Seismic Events at Mount Vesuvius**</span><br><br>",
    "Mount Vesuvius is a **quiescent stratovolcano** located in **Campania, Italy**. Rising **1,281m above sea level**, its **last eruption occurred in 1944**. While showing no current eruptive activity, some ongoing seismic events indicate the volcano remains active beneath the surface<br><br>",
    "From 2011 to 2024, over 12,000 seismic events were recorded at Mt. Vesuvius. These charts display epicenter locations across region, with colors indicating depth below surface and circle size representing duration magnitude (negative values indicating microearthquakes).<br><br>",
    "<span style = 'font-size: 8pt;'>{caption_text}</span>"
  )
)

# Plot -------------------------------------------------------------------

theme_set(theme_minimal(base_size = 12, base_family = "roboto"))

# Textbox for title and description
desc_box <- vesuvius_desc |>
  ggplot() +
  geom_textbox(
    aes(x = 0, y = 0, label = label),
    width = unit(.99, units = "npc"),
    fill = color_bg,
    box.color = color_gray,
    text.color = color_black
  ) +
  coord_cartesian(
    clip = "off"
  ) +
  theme_void()

depth_palette <- scale_color_viridis_c(
  option = "inferno",
  direction = -1
)

magnitude_scale <- scale_size_continuous(range = c(1, 5), name = "Magnitude")

p1 <- seismic_data_sf |>
  ggplot(aes(x = time, y = -depth_km)) +
  geom_point(aes(color = depth_km, size = duration_magnitude_md), alpha = .7) +
  geom_texthline(
    color = "#1a1c23",
    yintercept = surface$y,
    label = surface$label,
    linetype = "dashed"
  ) +
  depth_palette +
  magnitude_scale +
  scale_x_datetime(
    breaks = scales::date_breaks("year"),
    labels = scales::label_date_short(format = "%Y")
  ) +
  coord_cartesian(
    expand = FALSE,
    clip = "off"
  ) +
  labs(
    title = "Vesuvius Seismic Events (2011-2024)",
    y = "Depth (Km)"
  ) +
  theme(
    panel.grid = element_line(color = color_grid, linetype = "31"),
    panel.grid.major.x = element_line(linetype = "solid"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10, color = color_axis_text),
    axis.title.x = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.18, .14),
    legend.text.position = "bottom",
    legend.title = element_text(color = color_gray),
    legend.text = element_text(color = color_gray),
    legend.justification.top = 0,
    legend.direction = "horizontal"
  ) +
  guides(
    color = "none",
    size = guide_bins(
      title = "Duration magnitude (Md)",
      title.position = "top",
      label.theme = element_text(size = 8, face = "bold"),
      keywidth = unit(.8, "cm"),
      keyheight = unit(.4, "cm"),
      override.aes = list(
        color = color_lightgray
      )
    )
  )

map1 <- italy |>
  ggplot() +
  geom_sf(fill = color_lightgray, color = color_gray) +
  geom_sf(
    data = seismic_data_sf,
    aes(color = depth_km, size = duration_magnitude_md),
    alpha = .7
  ) +
  geom_sf(
    data = vesuvius_geom,
    shape = 24,
    size = 4,
    fill = "#ea4335",
    color = color_black
  ) +
  geom_sf_text(
    data = vesuvius_geom,
    label = "Mt. Vesuvius",
    nudge_y = -.025,
    alpha = .8
  ) +
  depth_palette +
  magnitude_scale +
  labs(
    title = "Geographic distribution of seismic events"
  ) +
  coord_sf(
    crs = st_crs(vesuvius_buffer)
  ) +
  xlim(st_bbox(vesuvius_buffer)[c(1, 3)]) +
  ylim(st_bbox(vesuvius_buffer)[c(2, 4)]) +
  annotation_scale(
    location = "bl",
    width_hint = .25,
    style = "bar",
    pad_x = unit(.1, "cm"),
    pad_y = unit(.08, "cm")
  ) +
  theme_void() +
  theme(
    aspect.ratio = .618,
    text = element_text(family = "roboto"),
    legend.position = "none"
  )

map2 <- italy |>
  ggplot() +
  geom_sf(fill = color_lightgray, color = color_gray) +
  geom_sf(
    data = vesuvius_geom,
    shape = 24,
    size = 2,
    fill = "#ea4335",
    color = color_black
  ) +
  geom_sf_text(
    aes(label = italy$name)
  ) +
  depth_palette +
  magnitude_scale +
  coord_sf(
    crs = st_crs(vesuvius_buffer),
  ) +
  theme_void() +
  theme(
    legend.position = "none"
  )

p2 <- seismic_data_sf |>
  ggplot(aes(x = distance_km, y = -depth_km)) +
  geom_point(aes(color = depth_km, size = duration_magnitude_md), alpha = .7) +
  geom_texthline(
    color = "#1a1c23",
    yintercept = surface$y,
    label = surface$label,
    linetype = "dashed"
  ) +
  depth_palette +
  magnitude_scale +
  coord_cartesian(
    expand = FALSE,
    clip = "off"
  ) +
  labs(
    title = "Depth profile by distance from volcano center",
    x = "Distance (km)",
    y = "Depth (Km)"
  ) +
  theme(
    panel.grid = element_line(color = color_grid, linetype = "31"),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10, color = color_axis_text),
    legend.position = "none",
  )

plot <- (desc_box + p1 + plot_layout(widths = c(1.5, 3))) /
  ((map1 + inset_element(map2, .6, .64, 1.15, .98, align_to = "plot")) + p2) +
  plot_annotation(
    theme = theme(
      plot.margin = margin(25, 25, 10, 25)
    )
  ) &
  theme(
    text = element_text(color = "#1a1c23"),
    plot.background = element_rect(fill = color_bg, color = NA),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      color = color_black,
      size = 12,
      face = "bold",
      margin = margin(b = 12)
    ),
    plot.margin = margin(10, 10, 0, 10)
  )

ggsave(
  "2025_w19-mt_vesuvius.png",
  plot,
  height = 21,
  width = 29.7,
  units = "cm"
)
