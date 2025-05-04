# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggtext)
library(waffle)
library(showtext)

# Data -------------------------------------------------------------------

grant <-
  read_csv(here::here("project/tt2025_w18/data/nsf_terminations.csv"))

# tt <- tidytuesdayR::tt_load(2025, week = 18)
# grant <- tt$nsf_terminations

grant_data <- grant |>
  mutate(
    remaining_t_cut_short = nsf_expected_end_date - termination_letter_date,
    directorate = str_remove_all(directorate, '"')
  )

plot_data <- grant_data |>
  mutate(
    expected_end = case_when(
      remaining_t_cut_short < 0 ~ "Past expected end date",
      remaining_t_cut_short <= 30 ~ "up to 30 days",
      remaining_t_cut_short <= 100 ~ "up to 100 days",
      remaining_t_cut_short <= 365 ~ "up to 1 year",
      remaining_t_cut_short <= 730 ~ "up to 2 years",
      TRUE ~ "> 2 years"
    ),
    expected_end = factor(
      expected_end,
      levels = c(
        "Past expected end date",
        "up to 30 days",
        "up to 100 days",
        "up to 1 year",
        "up to 2 years",
        "> 2 years"
      )
    ),
    directorate = replace_na(directorate, "Not Specified"),
    directorate = fct_infreq(directorate)
  ) |>
  summarize(
    .by = c(directorate, expected_end),
    n = n()
  ) |>
  arrange(expected_end)


skimr::skim(grant_data$remaining_t_cut_short)
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
showtext_opts(dpi = 600)

# Texts ------------------------------------------------------------------

# Caption
data <- glue::glue(
  "**Data**: National Science Foundation Grant Terminations - Grant Watch"
)
chart <- glue::glue(
  "**#TidyTuesday**: 2025 Week 18"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")
caption_text <- glue::glue("{data}<br>{chart} | {author} | #rstats")

# Texts
subtitle <- "Since April 18, 2025, the National Science Foundation terminated over 1,000 grants, affecting research initiatives at various stages of their projected timelines. This chart displays each canceled project as an individual square, with colors indicating the time remaining before expected completion."

# Colors -----------------------------------------------------------------
color_black <- "#1a1c23"
color_bg <- "#f8f7f5"
color_gray <- "#4b4d54"
color_gray1 <- "#7e8087"
color_grid <- "#e2e3e7"
color_cap <- "#9a9da5"

# Plot -------------------------------------------------------------------

p <- plot_data |>
  ggplot(aes(fill = expected_end, values = n)) +
  geom_waffle(flip = TRUE, color = color_black, alpha = .8) +
  facet_wrap(~directorate, nrow = 1, strip.position = "bottom") +
  scale_y_continuous(
    breaks = scales::breaks_pretty(),
    labels = function(x) x * 10
  ) +
  labs(
    title = "Research Grants Cut Short",
    subtitle = subtitle,
    caption = caption_text,
    x = "Directorate",
    y = "Count",
    fill = "Time remaining before<br>expected completion"
  ) +
  theme_minimal(base_size = 12, base_family = "roboto") +
  theme(
    plot.background = element_rect(fill = color_bg, color = NA),
    panel.background = element_rect(fill = color_bg, color = NA),
    text = element_text(color = color_black),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = color_gray1),
    axis.title.x = element_text(
      color = color_gray,
      margin = margin(t = 24),
      face = "bold",
      hjust = 1
    ),
    axis.title.y = element_text(color = color_gray, face = "bold", hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = color_grid, linewidth = .6),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.76, 0.64),
    legend.key.spacing.y = unit(.8, "mm"),
    legend.background = element_rect(
      fill = color_bg,
      color = color_cap,
      linewidth = .4
    ),
    legend.title = element_markdown(
      lineheight = 1.2,
      color = color_black,
      face = "bold"
    ),
    strip.text = element_textbox_simple(
      color = color_gray,
      halign = 0.5,
      valign = 0.5,
      size = 10,
      margin = margin(t = 24)
    ),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      color = color_black,
      size = 22,
      face = "bold",
      margin = margin(b = 4)
    ),
    plot.subtitle = element_textbox_simple(
      color = color_gray,
      size = 12,
      margin = margin(b = 72)
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(
      size = 8,
      color = color_cap,
      halign = 1,
      lineheight = 1.2,
      margin = margin(t = 48)
    ),
    plot.margin = margin(25, 25, 10, 25)
  ) +
  paletteer::scale_fill_paletteer_d("ggsci::light_uchicago") +
  coord_equal(
    clip = "off",
    expand = FALSE
  )


ggsave(
  "2025_w18-nsf_grant_termination.png",
  p,
  height = 21,
  width = 29.7,
  units = "cm",
  dpi = 600
)
