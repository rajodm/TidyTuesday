# Packages ---------------------------------------------------------------

library(tidyverse)
library(showtext)
library(geomtextpath)
library(gghighlight)
library(ggtext)

# Load data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 13)
pokemon <- tt$pokemon_df

# Data Wranglin ----------------------------------------------------------

att_data <- pokemon |>
  drop_na(generation_id) |>
  summarize(
    .by = c(generation_id),
    phy_att = median(attack, na.rm = TRUE),
    spe_att = median(special_attack, na.rm = TRUE)
  ) |>
  pivot_longer(
    cols = 2:3,
    names_to = "attack_type",
    values_to = "value"
  ) |>
  mutate(
    attack_type = case_when(
      attack_type == "phy_att" ~ "Physical Attack",
      TRUE ~ "Special Attack"
    )
  )

# Colors -----------------------------------------------------------------

color_blue <- "#4385be"
color_red <- "#d14d41"
color_black <- "#101A24"
color_white <- "#f4f0ec"
pal <- c(
  "Physical Attack" = color_red,
  "Special Attack" = color_blue
)
color_light_black <- colorspace::lighten(color_black, .35)

# Fonts ------------------------------------------------------------------

font_add_google("Press Start 2P", family = "psp")

font_add_google("Quicksand", family = "quickS")

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 320)

# Texts ------------------------------------------------------------------

title <- "Physical vs. Special Attacks Across Pokémon Generations"

subtitle <- "In generation IV, Pokémon underwent a fundamental combat system change known as the **'Physical/Special Split'**. Before Gen IV, attack types were categorized as either physical or special based solely on their elemental type. Starting with Gen IV, each individual move was classified as physical or special based on its nature rather than its type."

label1 <- "**Moves classified** as physical or special<br>**by their elemental type**."

label2 <- "**Moves classified** as physical or special<br>**based on the move\\'s attributes**."

# Captions ---------------------------------------------------------------

data <- glue::glue("**Data**: {{pokemon}} R package")
chart <- glue::glue("<b>#TidyTuesday</b>: 2025 Week 13 | {data}")
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart}<br>{author} | #rstats")

# Plot -------------------------------------------------------------------

theme_set(theme_minimal(base_size = 14, base_family = "quickS"))

add_annotation <- function(
  x,
  y,
  label,
  fill = NA,
  label.colour = NA,
  text.colour = color_light_black,
  size = 4.2,
  lineheight = 1.3,
  ...
) {
  annotate(
    "richtext",
    x = x,
    y = y,
    size = size,
    fill = fill,
    lineheight = lineheight,
    label.colour = label.colour,
    text.colour = text.colour,
    label = label,
    ...
  )
}

p <- att_data |>
  ggplot(aes(generation_id, value, group = attack_type)) +
  geom_line(
    aes(color = attack_type),
    linewidth = 2.4,
    lineend = "round",
    show.legend = FALSE,
  ) +
  gghighlight(
    generation_id >= 4,
    use_group_by = FALSE,
    unhighlighted_params = list(
      linetype = "longdash",
      linewidth = 2.1,
      colour = NULL
    ),
    keep_scales = TRUE,
    line_label_type = "text_path",
    label_params = list(
      offset = unit(4, "pt"),
      hjust = 0,
      lineend = "round",
      gap = FALSE,
      size = 5.2
    )
  ) +
  geom_textvline(
    aes(label = "Physical/Special Split", xintercept = 4),
    color = color_light_black,
    linewidth = 0.35,
    size = 4.2,
    hjust = 0.25
  ) +
  scale_color_manual(values = pal) +
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 5, 6, 7),
    labels = c("I", "II", "III", "IV", "V", "VI", "VII")
  ) +
  scale_y_continuous(breaks = scales::breaks_pretty()) +
  add_annotation(
    x = 2,
    y = 16,
    label = label1,
  ) +
  add_annotation(
    x = 6,
    y = 16,
    label = label2,
  ) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption_text,
    x = "Generation",
    y = "Average Points"
  ) +
  coord_cartesian(
    clip = "off",
    ylim = c(0, 100)
  ) +
  theme(
    text = element_text(color = color_black, family = "quickS"),
    plot.background = element_rect(fill = color_white, color = color_white),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linetype = "dashed",
      color = colorspace::darken(color_white, 0.03)
    ),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      family = "psp",
      size = 18,
      color = color_black,
      lineheight = 1.5,
      maxwidth = 98,
      margin = margin(b = 10)
    ),
    plot.subtitle = element_textbox_simple(
      size = 13.5,
      color = color_light_black,
      lineheight = 1.3,
      margin = margin(b = 24)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      color = color_light_black,
      halign = 1,
      size = 9,
      lineheight = 1.3,
      margin = margin(t = 12)
    ),
    axis.title = element_text(
      family = "psp",
      size = 10,
      color = color_light_black
    ),
    axis.text = element_text(
      color = color_light_black,
      family = "psp",
      size = 9
    ),
    axis.title.x = element_text(margin = margin(t = 10)),
    plot.margin = margin(25, 25, 10, 25)
  )


ggsave(
  "2025_w13-pokemon.png",
  p,
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 320
)
