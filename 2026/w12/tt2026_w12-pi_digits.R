# Packages ---------------------------------------------------------------

library(tidyverse)
library(rayshader)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 12)
pi_digits <- tt$pi_digits

n <- 2001 # Number of digits
golden_ang <- 137.508 * (pi / 180)
cell_size <- 1.6
half_cell <- cell_size * .618

# Create a spiral
spiral_coords <-
  pi_digits |>
  dplyr::filter(digit_position < n) |>
  dplyr::mutate(
    angle = digit_position * golden_ang,
    radius = sqrt(angle),
    x = radius * cos(angle),
    y = radius * sin(angle)
  )

# Maze walls for each digits
# # Initial project but...
wall_config <-
  tibble::tribble(
    ~digit , ~top  , ~right , ~bottom , ~left ,
         0 , FALSE , FALSE  , FALSE   , FALSE ,
         1 , TRUE  , FALSE  , FALSE   , FALSE ,
         2 , FALSE , TRUE   , FALSE   , FALSE ,
         3 , TRUE  , TRUE   , FALSE   , FALSE ,
         4 , FALSE , FALSE  , TRUE    , FALSE ,
         5 , TRUE  , FALSE  , TRUE    , FALSE ,
         6 , FALSE , TRUE   , TRUE    , FALSE ,
         7 , TRUE  , TRUE   , TRUE    , FALSE ,
         8 , FALSE , TRUE   , TRUE    , TRUE  ,
         9 , TRUE  , TRUE   , TRUE    , TRUE
  )

spiral_coords <-
  spiral_coords |>
  dplyr::left_join(wall_config)

# Seg coord to build the walls
wall_seg <-
  spiral_coords |>
  dplyr::mutate(
    top_x1 = x - half_cell,
    top_y1 = y + half_cell,
    top_x2 = x + half_cell,
    top_y2 = top_y1,
    right_x1 = top_x2,
    right_y1 = top_y1,
    right_x2 = right_x1,
    right_y2 = y - half_cell,
    bottom_x1 = top_x1,
    bottom_y1 = right_y2,
    bottom_x2 = top_x2,
    bottom_y2 = bottom_y1,
    left_x1 = top_x1,
    left_y1 = top_y1,
    left_x2 = top_x1,
    left_y2 = right_y2
  )


seg_long <-
  wall_seg |>
  tidyr::pivot_longer(
    cols = top:left,
    names_to = "wall_side",
    values_to = "draw"
  ) |>
  dplyr::filter(draw) |>
  dplyr::mutate(
    x1 = dplyr::recode_values(
      wall_side,
      "top" ~ top_x1,
      "right" ~ right_x1,
      "bottom" ~ bottom_x1,
      "left" ~ left_x1
    ),
    x2 = dplyr::recode_values(
      wall_side,
      "top" ~ top_x2,
      "right" ~ right_x2,
      "bottom" ~ bottom_x2,
      "left" ~ left_x2
    ),
    y1 = dplyr::recode_values(
      wall_side,
      "top" ~ top_y1,
      "right" ~ right_y1,
      "bottom" ~ bottom_y1,
      "left" ~ left_y1
    ),
    y2 = dplyr::recode_values(
      wall_side,
      "top" ~ top_y2,
      "right" ~ right_y2,
      "bottom" ~ bottom_y2,
      "left" ~ left_y2
    )
  ) |>
  dplyr::select(digit_position, digit, wall_side, x1, x2, y1, y2)

# Miscs ------------------------------------------------------------------

# Caption
chart <- glue::glue("#TidyTuesday: 2026 Week 12")

caption <-
  glue::glue(
    "Source: One Million Digits of Pi (Eve Andersson collection)"
  )


author <- "@rajodm.bsky.social"

caption_text <- glue::glue(
  "{caption}\n{chart} | {author} | #rstats | #rayshader"
)

# Plot -------------------------------------------------------------------

ggplot2::theme_set(ggplot2::theme_classic(
  base_size = 12,
  base_family = "Atkinson Hyperlegible Next",
  ink = "#1a252f",
  paper = "#f8f9fa"
))

pi_maze <- ggplot2::ggplot() +
  ggplot2::geom_tile(
    data = spiral_coords,
    ggplot2::aes(x, y, fill = digit),
    width = cell_size,
    height = half_cell
  ) +
  ggplot2::geom_segment(
    data = seg_long,
    ggplot2::aes(x1, y1, xend = x2, yend = y2),
    color = "#8896a5",
    linewidth = .2
  ) +
  ggplot2::scale_fill_gradientn(
    colors = cols4all::c4a("bivario.blade_runner")
  ) +
  ggplot2::theme_sub_plot(
    background = ggplot2::element_rect(fill = "#f8f9fa", color = NA)
  ) +
  ggplot2::theme_sub_panel(
    background = ggplot2::element_rect(fill = "#f8f9fa", color = NA),
    grid.major = ggplot2::element_blank(),
    grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_axis(
    title = ggplot2::element_blank(),
    text = ggplot2::element_blank(),
    line = ggplot2::element_blank(),
    ticks = ggplot2::element_blank()
  ) +
  ggplot2::theme_sub_legend(position = "none")

rayshader::plot_gg(
  pi_maze,
  width = 6,
  height = 6,
  shadow = FALSE,
  shadow_darkness = .01,
  windowsize = c(1200, 1200),
  zoom = .78,
  phi = 65,
  theta = 45,
  solidcolor = "#f8f9fa",
  solidlinecolor = "#f8f9fa",
  background = "#F8F9FA",
  multicore = TRUE
)

Sys.sleep(5)

# Dowload scene

url <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/white_studio_01_4k.hdr"

hdri_file <- basename(url)
download.file(
  url = url,
  destfile = hdri_file,
  mode = "wb"
)

plot_path <- here::here("2026/w12/", "test.png")

rayshader::render_highquality(
  plot_path,
  sample = 260,
  sample_method = "sobol",
  interactive = FALSE,
  preview = TRUE,
  environment_light = hdri_file,
  intensity_env = 1.2,
  rotate_env = 90,
  parallel = TRUE,
  width = 1200,
  height = 1200,
)


pi_png <- magick::image_read(plot_path)

png_taged <- pi_png |>
  magick::image_annotate(
    caption_text,
    size = 16,
    font = "Atkinson Hyperlegible Next",
    color = "#4a5568",
    gravity = "southeast",
    location = "+15+20"
  ) |>
  magick::image_annotate(
    "2k digits of \u03c0",
    size = 36,
    font = "Atkinson Hyperlegible Next",
    weight = 900,
    color = "#1a252f",
    gravity = "southeast",
    location = "+15+60"
  )

magick::image_write(
  png_taged,
  here::here("2026/w12/2026_w12-pi_digits.png"),
  quality = 100
)

# fs::file_delete(plot_path)
# fs::file_delete(hdri_file)
