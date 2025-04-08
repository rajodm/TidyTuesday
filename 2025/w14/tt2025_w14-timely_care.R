# Packages ---------------------------------------------------------------

library(tidyverse)
library(ggdist)
library(patchwork)
library(showtext)
library(ggtext)

# Load data --------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 14)
care_state <- tt$care_state


# Fonts ------------------------------------------------------------------

font_add_google("Lato", "lato")

font_add(
  "fa6-brands",
  "fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 300)

# Colors -----------------------------------------------------------------

sep_1 <- "#1A4F8C"
sep_sh <- "#B74583"
sev_sep <- "#1C6C66"
color_bg <- "#f5f7fa"
color_panel <- "#ebf0f6"
color_grey <- "#d6dce4"
color_black <- "#101a24"
color_h1 <- "#4f1b39"

# Texts ------------------------------------------------------------------

title <- "Timely and Effective Care: A Path to Better Outcomes in Sepsis"

subtitle <-
  glue::glue(
    "**Sepsis is a life-threatening condition** caused by dysregulated response to infection, potentially leading to organ dysfunction (<span style= 'color: {sev_sep}'>**severe sepsis**</span>) and <span style= 'color: {sep_sh}'>**septic shock**</span> with dangerous blood pressure drops. This chart shows the distribution of appropriate sepsis care and highlights compliance with management guidelines (**3-hour (h3) & 6-hour bundles (h6)**)."
  )


# Captions ---------------------------------------------------------------

data <- glue::glue(
  "**Data**: Timely and Effective Care by US State - Centers for Medicare & Medicaid Services"
)
chart <- glue::glue(
  "**#TidyTuesday**: 2025 Week 14 | **#30DayChartChallenge 2025, Day 8**: Distributions - Histogram"
)
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart}<br>{data} | {author} | #rstats")

# Data Wrangling ---------------------------------------------------------

plot_data <-
  care_state |>
  filter(str_detect(measure_id, "SEP")) |>
  select(starts_with("measure"), score) |>
  drop_na(score) |>
  mutate(
    .by = measure_id,
    count = n()
  ) |>
  nest(data = !c(starts_with("measure"), count)) |>
  mutate(
    title = case_when(
      str_detect(measure_name, "Percentage") ~
        "US distribution of Adequate Care",
      TRUE ~ measure_name
    ),
    title = str_replace_all(title, "3-Hour", "h3"),
    title = str_replace_all(title, "6-Hour", "h6"),
    color = case_when(
      measure_id == "SEP_1" ~ sep_1,
      str_detect(measure_id, "SEP_SH") ~ sep_sh,
      TRUE ~ sev_sep
    ),
    measure_id = factor(
      measure_id,
      levels = c(
        "SEP_1",
        "SEV_SEP_3HR",
        "SEV_SEP_6HR",
        "SEP_SH_3HR",
        "SEP_SH_6HR"
      )
    )
  ) |>
  nest(misc = !c(data, starts_with("measure"))) |>
  arrange(measure_id)

# Plots ------------------------------------------------------------------

theme_set(theme_minimal(base_size = 11, base_family = "lato"))

plots <- plot_data |>
  mutate(
    plot = map2(data, misc, function(df, misc) {
      df |>
        ggplot() +
        stat_histinterval(
          aes(x = score),
          fill = misc$color,
          color = color_black,
          size = 4
        ) +
        annotate(
          "text",
          x = 12.5,
          y = 0.95,
          size = rel(3),
          label = glue::glue("n = {misc$count}")
        ) +
        labs(
          title = misc$title,
          x = "Appropriate Care (%)"
        ) +
        coord_cartesian(
          xlim = c(0, 100),
          clip = "off",
          expand = FALSE
        ) +
        theme(
          plot.margin = margin(10, 10, 0, 10)
        )
    })
  ) |>
  pull(plot)

p1 <- wrap_plots(plots) +
  plot_layout(
    design = "
    AAAAAAAA
    AAAAAAAA
    BBCCDDEE
    "
  ) +
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption_text,
    theme = theme(
      plot.title = element_textbox_simple(
        size = 22,
        color = color_h1,
      ),
      plot.subtitle = element_textbox_simple(
        size = 12,
        margin = margin(t = 4, b = 8)
      ),
      plot.caption = element_textbox_simple(
        color = colorspace::lighten(color_black, 0.5),
        size = 8,
        margin = margin(t = 10),
        halign = 1
      ),
      plot.margin = margin(12, 12, 5, 12)
    )
  ) &
  theme(
    text = element_text(color = "#101a24"),
    plot.background = element_rect(fill = color_bg, color = NA),
    panel.background = element_rect(fill = color_panel, color = NA),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(face = "bold"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(
      color = "#272727",
      margin = margin(t = 5.5)
    ),
    axis.text.y = element_blank(),
    panel.grid.major = element_line(
      color = color_grey,
      linewidth = 0.5,
      linetype = "dashed"
    ),
    panel.grid.minor = element_blank(),
  )


# Save -------------------------------------------------------------------

ggsave(
  "2025_w14-timely_care.png",
  p1,
  height = 8.5,
  width = 11,
  units = "in",
  dpi = 300
)

showtext_auto(FALSE)
