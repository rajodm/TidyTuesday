# Packages ---------------------------------------------------------------

library(dplyr)
library(stringr)
library(ggplot2)
library(ggsankey)
library(ggtext)

source(here::here("cap_func.R"))

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 33)
ielts_reasons <- tt$demo_by_reasons

levels <- ielts_reasons |>
  distinct(band) |>
  pull()

sankey_df <-
  ielts_reasons |>
  filter(
    year == "2024-2025",
    stringr::str_detect(reason, "education")
  ) |>
  mutate(
    weight = round(percent * 100),
    type = str_replace_all(type, "_", " ")
  ) |>
  tidyr::uncount(weight) |>
  make_long(reason, type, band)

node_labs <- sankey_df |>
  summarize(
    .by = c(x, node),
    n = n()
  ) |>
  mutate(
    .by = x,
    pct = n / sum(n) * 100,
    labs = glue::glue(
      "{str_wrap(node, 24)}\n",
      "{scales::percent(pct, scale = 1)}"
    )
  ) |>
  select(x, node, labs)

plot_data <- sankey_df |>
  left_join(node_labs) |>
  # throw a warning b/c the levels contains only the values for the bands
  mutate(node = forcats::fct_relevel(node, levels))

# Miscs ------------------------------------------------------------------
color_ink <- "#1a1a1a"
color_paper <- "#f8f8f8"
color_gray <- "#787878"
color_green <- "#7f947e"

pal <- c(
  "General Training" = "#18537f",
  "Academic" = "#04263c",
  "For other education purposes" = colorspace::lighten(
    color_green,
    amount = 0.3
  ),
  "For higher education short course (three months or less)" = color_green,
  "For higher education extended course (three months or more)" = colorspace::darken(
    color_green,
    amount = 0.3
  ),
  "<4" = "#AA8030",
  "4" = "#A2792D",
  "4.5" = "#9B732A",
  "5" = "#946C28",
  "5.5" = "#8C6625",
  "6" = "#856023",
  "6.5" = "#7E5920",
  "7" = "#76531D",
  "7.5" = "#6F4C1B",
  "8" = "#684618",
  "8.5" = "#614016"
)

# Texts ------------------------------------------------------------------

plot_title <- "Most IELTS Applicants for Educational Purposes Score Between 5 and 7"

plot_subtitle <- "The International English Language Testing System (IELTS) exam consists of 4 parts: Listening, Speaking, Reading and Writing. Each part is scored using a \"_band_\" systemn from 1 to 9, and then the overall average is used as the \"band score\". This chart shows how 2024-2025 test-takers for educational purposes performed on IELTS exams (Academic or General Training)."

caption <- generate_caption("IELTS official statistics", 33)

# Plot -------------------------------------------------------------------

p <- plot_data |>
  ggplot(aes(
    x = x,
    next_x = next_x,
    node = node,
    next_node = next_node,
    fill = node,
    label = labs
  )) +
  ggscribe::panel_shade(
    xmin = c(0.35, 1.7, 2.7),
    xmax = c(1.1, 2.3, 3.3),
    ymin = -725,
    ymax = 725,
    fill = color_gray,
    alpha = 0.06
  ) +
  geom_sankey(
    flow.alpha = 0.6,
    show.legend = FALSE
  ) +
  geom_sankey_text(
    fontface = "bold",
    lineheight = 0.85,
    position = position_nudge(
      x = c(rep(-0.35, 3), rep(0, 2), rep(0.16, 11)),
      y = c(rep(0, 3), -205, 205, rep(0, 11))
    )
  ) +
  ggscribe::panel_shade(
    xmin = 3.06,
    xmax = 3.3,
    ymin = -400,
    ymax = 400,
    alpha = 0.3,
    fill = color_gray
  ) +
  ggscribe::axis_bracket(xintercept = 3.3, breaks = c(-400, 400)) +
  annotate(
    "richtext",
    x = 3.32,
    y = 0,
    fill = NA,
    label.colour = NA,
    hjust = 0,
    family = "Atkinson Hyperlegible Next",
    label = str_wrap(
      "Approximately 76% of people who take the IELTS exam for academic purposes score between bands 5 and 7",
      24
    ) |>
      str_replace_all("\\\n", "<br>")
  ) +
  scale_x_discrete(
    sec.axis = ggscribe::sec_axis_text(
      breaks = c(0.725, 2, 3),
      labels = c(
        "Stated reason to take exam\n(%)",
        "Type of IELTS exam\n(%)",
        "Band Score\n(%)"
      )
    ),
    expand = expansion(add = c(0.6, 0.9))
  ) +
  scale_fill_manual(values = pal) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = caption
  ) +
  coord_cartesian(clip = "off") +
  theme_void(
    ink = color_ink,
    paper = color_paper,
    base_family = "Atkinson Hyperlegible Next",
    base_size = 12
  ) +
  theme_sub_plot(
    title.position = "plot",
    title = element_textbox_simple(
      family = "Bricolage Grotesque",
      size = rel(1.4),
      face = "bold",
      margin = margin(b = 6)
    ),
    subtitle = element_textbox_simple(
      margin = margin(b = 24),
      size = rel(1)
    ),
    caption.position = "plot",
    caption = element_textbox_simple(
      size = rel(0.75),
      margin = margin(t = 10),
      halign = 0
    ),
    margin = margin(25, 25, 10, 25)
  ) +
  theme_sub_axis_top(text = element_text())

ggh4x::save_plot(
  here::here("2026/w33", "2026_w33-ielts_scores.png"),
  plot = p,
  height = 21,
  width = 29.7,
  units = "cm",
  dpi = 600
)
