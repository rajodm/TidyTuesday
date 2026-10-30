# Packages ---------------------------------------------------------------

library(dplyr)
library(stringr)
library(ggplot2)
library(treemapify)

source("cap_func.R")

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 43)

places <- tt$ikea_places
seating <- tt$ikea_seating

product_names <- seating |>
  dplyr::left_join(
    places,
    dplyr::join_by(product_name == name),
    relationship = "many-to-many"
  ) |>
  dplyr::filter(country == "de") |>
  dplyr::select(place_type, category) |>
  tidyr::drop_na(place_type) |>
  dplyr::mutate(
    place_gp = dplyr::recode_values(
      place_type,
      from = places_gp$from,
      to = places_gp$to
    )
  )

# Create categories
places_gp <- tibble::tribble(
  ~from                                                                    , ~to                   ,
  c("city", "town", "suburb", "neighbourhood", "residential")              , "Urban place"         ,
  c("village", "hamlet", "locality", "isolated_dwelling", "farm")          , "Small settlement"    ,
  c("municipality", "administrative", "statistical")                       , "Administrative area" ,
  c("lake", "bay", "wetland", "peak", "cape", "forest", "island", "islet") , "Natural feature"     ,
  "archaeological_site"                                                    , "Other"
)


# Misc -------------------------------------------------------------------

# Fonts
systemfonts::require_font("Atkinson Hyperlegible Next")
systemfonts::require_font("Schibsted Grotesk")
systemfonts::register_variant(
  "SG semibold",
  "Schibsted Grotesk",
  "semibold"
)

# Colors
col_ink <- "#161820"
col_paper <- "#fcfbfa"

# palette: carto.pastel
pal <- c(
  "Urban place" = "#66c5cc",
  "Small settlement" = "#f6cf71",
  "Administrative area" = "#dcb0f2",
  "Natural feature" = "#87c55f",
  "Other" = "#f89c74"
)

# Texts
title_text <- "**Most IKEA sofa and armchair names in Germany come from small Swedish settlements**"

subtitle_text <- "Since 2020, IKEA has given its new products Swedish names that are easier to remember. Sofas and armchairs are named after Swedish places. This treemap shows which place types and categories appear most often among products sold in Germany."

caption_text <- generate_caption(
  "product listings from ikea.com; places from OpenStreetMap via Nominatim, © OpenStreetMap contributors (ODbL); exchange rates from the fawazahmed0 currency API (CC0).",
  43,
  note = "**Top-level categories were assigned manually**",
  src_txt = "Sources"
)

# Plot -------------------------------------------------------------------

p <- product_names |>
  dplyr::summarise(
    .by = c(category, place_gp, place_type),
    n = dplyr::n()
  ) |>
  dplyr::mutate(
    .by = category,
    prop = n / sum(n),
    place_type = stringr::str_replace_all(place_type, "_", " ") |>
      stringr::str_to_sentence(),
    category = stringr::str_to_upper(category)
  ) |>
  dplyr::mutate(
    .by = c(category, place_gp),
    gp_prop = sum(prop),
    gp_labs = glue::glue(
      "{place_gp}\n{scales::percent(gp_prop)}"
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(
    area = prop,
    fill = place_gp,
    subgroup = gp_labs,
    subgroup2 = place_type,
    label = gp_labs
  )) +
  treemapify::geom_treemap(show.legend = FALSE) +
  treemapify::geom_treemap_subgroup2_border(
    size = 0.65,
    color = col_ink
  ) +
  treemapify::geom_treemap_subgroup_text(
    family = "SG semibold",
    place = "centre",
    grow = TRUE
  ) +
  treemapify::geom_treemap_subgroup2_text(
    alpha = 0.8,
    size = 7,
    reflow = TRUE,
    place = "centre",
    color = colorspace::lighten(col_ink, 0.15),
    family = "Atkinson Hyperlegible Next"
  ) +
  ggplot2::scale_fill_manual(values = pal) +
  ggplot2::facet_wrap(~category) +
  ggplot2::coord_equal(expand = FALSE) +
  ggplot2::labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  ggplot2::theme_minimal(
    base_family = "Atkinson Hyperlegible Next",
    base_size = 11,
    ink = col_ink,
    paper = col_paper
  ) +
  ggplot2::theme_sub_plot(
    title.position = "plot",
    title = ggtext::element_textbox_simple(
      width = unit(1, "npc"),
      lineheight = 1.05,
      size = rel(1.3),
      margin = ggplot2::margin(b = 8, t = 0)
    ),
    subtitle = ggtext::element_textbox_simple(
      width = unit(0.97, "npc"),
      hjust = 0,
      margin = ggplot2::margin(t = 0, b = 16),
      size = rel(1)
    ),
    caption.position = "plot",
    caption = ggtext::element_textbox_simple(
      size = rel(0.7),
      lineheight = 1.3,
      margin = margin(t = 8)
    ),
    margin = ggplot2::margin(15, 15, 8, 15)
  ) +
  ggplot2::theme_sub_panel(spacing.x = unit(1.62, "lines")) +
  ggplot2::theme_sub_strip(
    text = ggplot2::element_text(
      family = "SG semibold",
      size = rel(1.05)
    ),
    background = ggplot2::element_blank()
  )

ggh4x::save_plot(
  "2026/w43/2026_w43-ikea_sofas_and_armchairs.png",
  plot = p,
  width = 25,
  height = 16,
  units = "cm",
  dpi = 600
)
