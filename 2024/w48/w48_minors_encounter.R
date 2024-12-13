
# Libraries -------------------------------------------------------------------------

library(tidyverse)
library(geomtextpath)
library(patchwork)
library(ggtext)
library(epoxy)
library(showtext)



# Fonts -----------------------------------------------------------------------------

font_add("fa6-brands", "fonts/Font Awesome 6 Brands-Regular-400.otf")
font_add_google("Roboto", family = "roboto")
showtext_auto()
showtext_opts(dpi = 300)

# Visualization parameters ----------------------------------------------------------

# Font
text_font <- "Roboto"

# Colors
bg_color  <- "#f8f4ed"
main_color <- "#060609"
main_color_1 <- "#181b26"
main_color_2 <- "#eb7b07"
grid <- "#e0d6c8"
subtitle_color <- "#2e3035"

# Theme
theme_set(theme_minimal(base_size = 16, base_family = text_font))
theme_update(
  panel.background = element_rect(fill = bg_color, color = bg_color),
  plot.background = element_rect(fill = bg_color, color = bg_color),
  axis.title = element_blank(),
  axis.text = element_text(color = main_color),
  panel.grid.minor.x = element_blank(),
  panel.grid.major = element_line(color = grid),
  panel.grid.minor.y = element_line(color = grid),
  plot.title.position = "plot",
  plot.title = element_text(size = 16, face = "bold", color = main_color),
  plot.subtitle = element_textbox_simple(color = subtitle_color , margin = margin(t = 5)),
  plot.caption.position = "plot",
  plot.caption = element_markdown(hjust = 0, margin = margin(t = 30), size = 10),
  plot.margin = margin(t = 20, b = 20, 20, 20)
)


# Caption -----------------------------------------------------------------------------

data <- epoxy("**Data**: www\\.cbp\\.gov - Encounter Data")
chart <- epoxy("<b>#TidyTuesday</b> : 2024 Week 48 | {data}")
X_ic <- epoxy("<span style='font-family:fa6-brands'>&#xe61b;</span>")
bsky <- epoxy("<span style='font-family:fa6-brands'>&#xe671; </span>")
author <- epoxy("**Graphic**: {bsky} @r4j0")

caption_text <- epoxy("{chart} | {author} | #rstats")

# Texts -----------------------------------------------------------------------------

main_title <- "Minors Encountered at U.S. Borders, Fiscal Years 2020-2024"
subtitle <- epoxy("Over a 5-year period, the proportion of <span style= 'color:{main_color_2}'>**unaccompanied minors**</span> at the U.S. Borders has declined, while the proportion of <span style= 'color:{main_color_1}'>**accompanied minors**</span> has remained relatively constant.")
p1_title <- "Proportion of Minors Encountered at U.S Borders"
p2_title <- "Number of Minors Encountered at U.S Borders"
caption1 <- "UC: Unaccompanied Children"

# DATA -------------------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2024, week = 48)
cbp_resp <- tt$cbp_resp

cbp_prop <- cbp_resp |>
  group_by(fiscal_year, demographic) |>
  summarize(
    encounter = sum(encounter_count),
    .groups = "drop"
  ) |>
  group_by(fiscal_year) |>
  mutate(
    total_encounter = sum(encounter),
    prop = encounter/total_encounter
  )

# Plots -----------------------------------------------------------------------------

p1 <- cbp_prop |>
  filter(str_detect(demographic, pattern = "Minors")) |>
  ggplot(aes(x = fiscal_year, y = prop, color = demographic)) +
  geom_textline(
    aes(label = demographic),
    show.legend = FALSE,
    linewidth = 1.2,
    size = 5.47
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    labels = scales::percent_format()
  ) +
  scale_color_manual(values = c(main_color_1, main_color_2)) +
  labs(
    title = p1_title
  )

p2 <- cbp_prop |>
  filter(str_detect(demographic, pattern = "Minors")) |>
  ggplot(aes(x = fiscal_year, y = encounter, color = demographic)) +
  geom_textline(
    aes(label = demographic),
    show.legend = FALSE,
    linewidth = 1.2,
    size = 5.47
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    labels = scales::number_format(scale_cut = scales::cut_short_scale())
  ) +
  scale_color_manual(values = c(main_color_1, main_color_2)) +
  labs(
    title = p2_title
  )

# Final plot
final_plot <- p1 / p2 +
  labs(
    caption = caption1
  ) +
  plot_annotation(
    caption = caption_text,
    title = main_title,
    subtitle = subtitle,
    theme = theme(
      plot.title = element_textbox_simple(face = "bold", size = 22, padding = margin(b = 15, t = 30)),
      plot.caption = element_markdown(
        hjust = 0.5,
        color = subtitle_color,
        family = text_font,
        size = 10,
        margin = margin(t = 0)
      )
    )
  )


# ggsave ----------------------------------------------------------------------------

ggsave(
  "Minor_encounters.png",
  final_plot,
  units = "cm",
  height = 29.7,
  width = 21
)
