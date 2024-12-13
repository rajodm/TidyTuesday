
# Libraries --------------------------------------------------------------

library(tidyverse)
library(ggfittext)
library(ggtext)
library(showtext)
library(treemap)

# Fonts ------------------------------------------------------------------

font_add_google("Italiana", family = "italiana")
font_add_google("Merriweather", family = "mwt")
font_add("fa6-brands", "fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()
showtext_opts(dpi = 300)

# Visualisation Parameters -----------------------------------------------

title_font <- "italiana"
text_font <- "mwt"

# Colors
bg_color <- "#fff8ee"
theme_color_1 <- "#2e0b0b"
theme_color_2 <- "#7a1d1d"
theme_color_3 <- "#c62f2f"

# Theme
theme_set(theme_void(base_size = 20, base_family = title_font))
theme_update(
    plot.background = element_rect(fill = bg_color, color = bg_color),
    plot.title = element_text(color = theme_color_1, size = 40, face = "bold", hjust = 0.5),
    plot.subtitle = element_textbox_simple(family = text_font, color = theme_color_1, size = 11, halign = 0.5, margin = margin(t = 1, b = 2)),
    plot.caption.position = "plot",
    plot.caption = element_markdown(
      hjust = 1,
      family = text_font,
      color = theme_color_1,
      size = 7,
      margin = margin(t = 5, l = 20)
    ),
    plot.margin = margin(20, 20, 10, 20)
)

# Captions ------------------------------------------------------------------

data <- glue::glue("**Data**: The Scent of Data")
chart <- glue::glue("<b>#TidyTuesday</b> : 2024 Week 50 | {data}")
X_ic <- glue::glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @r4j0")

caption_text <- glue::glue("{chart}<br>{author} | #rstats")



# Texts ------------------------------------------------------------------

title_text <- "Perfume Brands on Parfumo"
subtitle_text <- "Top 50 Brands by Number of Perfumes (perf.) and Average Rating."

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2024, week = 50)
perfumes <- tt$parfumo_data_clean

top_brands <- perfumes |>
  summarise(
    perfume_number = n(),
    avg_rating = median(Rating_Value, na.rm = TRUE),
    .by = Brand
  ) |>
  arrange(Brand, desc(perfume_number)) |>
  mutate(row_id = row_number()) |>
  arrange(desc(perfume_number)) |>
  slice_head(n = 50)

# treemap() returns a list
# tm is the element of the list we will use for our visualization
brands_data <- top_brands |>
  treemap(
    index = c("Brand"),
    vSize = "perfume_number",
    stdErr = "avg_rating",
    sortID = "row_id",
    aspRatio = 1.618,
    draw = FALSE
  )

# tm is a data.frame containing information about the rectangles:
# indices, sizes, original color values, derived color values, depth
# level, position (x0, y0, w, h), and color (from the documentation).
brands_data <- brands_data$tm |>
  as_tibble() |>
  mutate(avg_rating = stdErr)

# We need values for rating categories
p25 <- round(quantile(brands_data$avg_rating, probs = 0.25), 2)
p75 <- round(quantile(brands_data$avg_rating, probs = 0.75), 2)

# Data for ploting
plot_data <- brands_data |>
  mutate(
    # Rescaling the rectangles
    scale_factor = sqrt(vSize/max(vSize)),
    scaled_width = w * scale_factor,
    scaled_height = h * scale_factor,
    scaled_x0 = x0 + (w - scaled_width)/2,
    scaled_y0 = y0 + (w - scaled_width)/2,
    # we will need this for the geom_rect
    xmin = scaled_x0,
    xmax = scaled_x0 + scaled_width,
    ymin = scaled_y0,
    ymax = scaled_y0 + scaled_height,
    # Aspect ratio to determine if the reclange is taller or wider
    aspr = scaled_height/scaled_width,
    # Categories for the ratings
    rating = cut(
      avg_rating,
      breaks = c(-Inf, p25, p75, Inf),
      labels = c(
        glue::glue("< {p25}"),
        glue::glue("{p25} - {p75}"),
        glue::glue("> {p75}")
      )
    ),
    # Remove text after a '/' then extraspaces
    Brand = str_trim(str_extract(Brand, "^[^/]+")),
    # Escape `'` for a better rendering in markdown
    Brand = str_replace_all(Brand, "'", "\\\\'"),
    # Create a label
    brand_label = case_when(
      vSize > quantile(vSize, probs = 0.75) ~ glue::glue(
        "**{Brand}**",
        "<br>",
        # I don't know if this is working
        "<span style='font-weight: lighter;'>{vSize} perf.</span>"
      ),
      TRUE ~ glue::glue("{Brand}")
    )
  )

# Plot -------------------------------------------------------------------


plot <- plot_data |>
  ggplot() +
  geom_rect(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      fill = rating
    ),
    color = bg_color
  ) +
  geom_fit_text(
    aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      label = brand_label,
      # Rotate if the rectangle is talle than wide
      angle = case_when(
        aspr >= 3 ~ 90,
        TRUE ~ 0
      )
    ),
    color = bg_color,
    # Enable markdown formatting
    rich = TRUE,
    lineheight = 1.2,
    min.size = 0,
    reflow = TRUE,
    fullheight = TRUE
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  scale_fill_manual(values = c(theme_color_3, theme_color_2, theme_color_1)) +
  guides(
    fill = guide_legend(
      title = "Average Rating",
      position = "top",
      theme = theme(
        legend.title.position = "top",
        legend.title = element_text(family = text_font, size = 9, margin = margin(b = 2)),
        legend.key.height = unit(0.6, "lines"),
        legend.key.width = unit(4.75, "lines"),
        legend.text.position = "bottom",
        legend.text = element_text(family = text_font, size = 9, margin = margin(1, 0, 0, 0)),
        legend.margin = margin(t = 1, b = 0)
      )
    )
  )

ggsave(
  "2024_w50-the_scent_of_data.png",
  plot,
  width = 29.7,
  height = 21,
  units = "cm"
)
