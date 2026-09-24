# Packages ---------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggtext)

source("cap_func.R")

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 38)

urban <- tt$urban |>
  filter_out(year == 2025)

global_stats <- urban |>
  filter(is.na(cityCode))

city_stats <- urban |>
  filter_out(is.na(cityCode))

plot_data <- city_stats |>
  # Filter to African cities
  filter(stringr::str_detect(
    sdgSubRegion,
    stringr::regex("africa", TRUE)
  )) |>
  select(
    cityName,
    year,
    ga_avg_share = averageShareOfGreenAreaInCityUrbanAreaPct
  ) |>
  mutate(year = as.character(year))

median_ga_share <- plot_data |>
  summarize(
    .by = year,
    median_ga = median(ga_avg_share)
  )

median_ga_2020 <-
  median_ga_share |>
  filter(year == 2020) |>
  pull(median_ga) |>
  round(2)

# Misc -------------------------------------------------------------------

col_ink <- "#e6e6e4"
col_paper <- "#171717"
col_red <- "#C93312"

systemfonts::require_font("Atkinson Hyperlegible Next")
systemfonts::require_font("Atkinson Hyperlegible Mono")

title_text <- "African Cities Are Losing Their Green Space"

subtitle_text <- "Green space proportion fell decade over decade, and green-rich cities became far less common"

caption_text <- generate_caption(
  source = "Open Spaces and Green Areas",
  week = 38
) |>
  stringr::str_replace_all("<br>", " | ")

annotation1 <- glue::glue(
  "By 2020, the typical African city had<br>just ",
  "<span style='color: {col_red};'>**{median_ga_2020}%**</span> (median) ",
  "green space,<br>lower than the 3 previous decades<br>(fainter lines)."
)

annotation2 <- "The pool of cities with unusually high green coverage has shrunk sharply by the 90s."

annotation3 <- "Cities with more than 25% green space were always rare. Now they are almost gone."

# Plot -------------------------------------------------------------------

mgnf_from <- list(
  xmin = 25,
  xmax = 80,
  ymin = 0,
  ymax = 10
)

mgnf_to <- list(
  xmin = 25,
  xmax = 80,
  ymin = -10,
  ymax = -40
)

vgrids_pos <- tibble(x_val = seq(0, 80, 20))

inset_y_text <- tibble(
  x = 24,
  y = c(mgnf_to$ymax, median(c(mgnf_to$ymax, mgnf_to$ymin)), mgnf_to$ymin),
  label = as.character(c(mgnf_from$ymin, mgnf_from$ymax / 2, mgnf_from$ymax))
) |>
  mutate(
    label = replace_values(
      label,
      as.character(mgnf_from$ymax) ~ stringr::str_c(
        mgnf_from$ymax,
        "cities",
        sep = "\n"
      )
    )
  )

p <- plot_data |>
  ggplot(aes(x = ga_avg_share)) +
  geom_segment(
    data = vgrids_pos,
    aes(x = x_val, xend = x_val, y = 0, yend = Inf),
    linewidth = 0.15,
    color = col_ink,
    linetype = "31",
    alpha = 0.5
  ) +
  geom_histogram(
    aes(fill = year),
    binwidth = 2.5,
    position = "identity"
  ) +
  geom_segment(
    data = median_ga_share,
    aes(
      x = median_ga,
      xend = median_ga,
      y = 0,
      yend = Inf,
      alpha = year
    ),
    color = col_red,
    linewidth = 0.65,
    linetype = "FF",
    show.legend = FALSE
  ) +
  cols4all::scale_fill_discrete_c4a_seq(
    name = "Decade",
    "scico.nuuk"
  ) +
  scale_alpha_manual(
    values = c(0.2, 0.4, 0.6, 0.95)
  ) +
  theme_minimal(
    base_family = "Atkinson Hyperlegible Next",
    paper = col_paper,
    ink = col_ink,
    base_size = 12
  ) +
  theme_sub_panel(
    grid = element_line(
      linewidth = 0.35,
      linetype = "dashed"
    ),
    grid.minor = ggplot2::element_blank(),
    grid.major.x = ggplot2::element_blank()
  ) +
  theme_sub_axis(
    text = element_text(size = rel(0.95))
  ) +
  theme_sub_axis_y(title = ggplot2::element_blank()) +
  ggmagnify::geom_magnify(
    from = mgnf_from,
    to = mgnf_to,
    colour = col_red,
    linewidth = 0.65,
    expand = 0
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text,
    x = "Average Share of Green Area\n"
  ) +
  scale_x_continuous(
    labels = scales::percent_format(scale = 1),
    position = "top"
  ) +
  scale_y_continuous(
    limits = c(70, -45),
    breaks = seq(0, 100, 25),
    labels = \(x) {
      if_else(x != 0, stringr::str_c(x, "\ncities"), as.character(x))
    }
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  annotate(
    "richtext",
    x = 8,
    y = Inf,
    label = annotation1,
    color = col_paper,
    fill = col_ink,
    hjust = 0,
    vjust = 1
  ) +
  geom_text(
    data = inset_y_text,
    aes(x = x, y = y, label = label),
    hjust = 1,
    alpha = 0.6
  ) +
  annotate(
    "richtext",
    x = mean(c(mgnf_from$xmin, mgnf_from$xmax)) * 0.618,
    y = mgnf_from$ymax + 7,
    label = stringr::str_wrap(
      annotation2,
      38
    ) |>
      stringr::str_replace_all("\\n", "<br>"),
    hjust = 0,
    color = col_paper,
    fill = col_ink
  ) +
  annotate(
    "richtext",
    x = I(0),
    y = mean(c(mgnf_to$ymax, mgnf_to$ymin)),
    hjust = 0,
    vjust = 0.5,
    family = "Atkinson Hyperlegible Next",
    label = stringr::str_wrap(
      annotation3,
      24
    ) |>
      stringr::str_replace_all("\\n", "<br>"),
    color = col_paper,
    fill = col_ink
  ) +
  theme_sub_plot(
    margin = margin(20, 20, 20, 10),
    title.position = "plot",
    title = element_textbox_simple(
      size = rel(1.8),
      face = "bold",
      halign = 0.5,
      margin = margin(b = 8)
    ),
    subtitle = element_textbox_simple(
      halign = 0.5,
      margin = margin(b = 24),
      size = rel(1)
    ),
    caption.position = "plot",
    caption = element_textbox_simple(
      halign = 0.5,
      size = rel(0.8),
      color = colorspace::lighten(col_ink, 0.5)
    )
  ) +
  theme_sub_legend(
    justification.inside = c(1, 0),
    text = element_text(size = rel(1), family = "Atkinson Hyperlegible Mono"),
    title = element_text(size = rel(1.2), face = "bold"),
    position = "inside",
    position.inside = c(I(0.9), I(0.7)),
    key.height = unit(24, "pt"),
    key.width = unit(38.8, "pt"),
    key.spacing.y = unit(4, "pt"),
    text.position = "left"
  )

ggh4x::save_plot(
  plot = p,
  "2026/w38/2026_w38_updated-green_african_cities.png",
  width = 21,
  height = 25,
  units = "cm"
)
