# Packages ---------------------------------------------------------------

library(tidyverse)
library(treemapify)
library(ggtext)
library(systemfonts)

# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2026, 4)
companies <- tt$companies

capital_brackets <-
  companies |>
  mutate(
    capital_bracket = case_when(
      capital_stock <= quantile(capital_stock, .25) ~ "Lowest capital",
      capital_stock <= quantile(capital_stock, .75) ~ "Mid capital",
      capital_stock <= quantile(capital_stock, .95) ~ "High capital",
      TRUE ~ "Highest capital"
    ),
    capital_bracket = fct_relevel(
      capital_bracket,
      c("Lowest capital", "Mid capital", "High capital", "Highest capital")
    )
  )

tm_data <- capital_brackets |>
  count(company_size, capital_bracket) |>
  mutate(
    company_size_lab = str_remove(company_size, "-.*") |>
      str_to_title(),
    company_size = case_match(
      company_size,
      "small-enterprise" ~ "small\nenterprise",
      "micro-enterprise" ~ "micro\nenterprise",
      "other" ~ "other\nenterprise",
      .default = company_size
    ),
    label = glue::glue(
      "{capital_bracket}\n",
      "{scales::comma(n)} {company_size}"
    )
  )

# Miscs ------------------------------------------------------------------

register_variant(
  "fa7-brands",
  "Font Awesome 7 Brands"
)

# Caption
chart <- glue::glue(
  "**#TidyTuesday**: 2026 Week 04"
)

# Helper function to format the numbers
fmt <- function(x) {
  scales::number(x, scale_cut = scales::cut_short_scale(), accuracy = .1)
}

# Notes & caption
vals <- list(
  q25 = fmt(quantile(companies$capital_stock, .25)),
  q75 = fmt(quantile(companies$capital_stock, .75)),
  q95 = fmt(quantile(companies$capital_stock, .95))
)

caption <- glue::glue(
  "**\\*Capital brackets (BRL)**: lowest ≤ {vals$q25}, middle ≤ {vals$q75}, next 20% ≤ {vals$q95}, Top 5% > {vals$q95}<br>",
  "**Source**: Cadastro National de Pessoa Jurídica - Dec 2025 | Brazilian Ministry of Finance/Receita Federal via dados.gov.br"
)

bsky <- glue::glue("<span style='font-family:fa7-brands'>&#xe671;</span>")

author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{caption}<br>{chart} | {author} | #rstats")

# Colors
color_paper <- "#f8f9fa"
color_ink <- "#1a252f"
color_gray <- "#5a6c7d"

tm_pal <- c(
  "Highest capital" = "#c0392b",
  "High capital" = "#d97742",
  "Mid capital" = "#7cb342",
  "Lowest capital" = "#16a085"
)

# Subtitle
subtitle <- glue::glue(
  "Each Box represents companies grouped by size (**Micro**/**Small**/**Other**),",
  " and **capital brackets***: ",
  "<span style=' color:{tm_pal[['Lowest capital']]};'>**lowest 25%**</span>, ",
  "<span style=' color:{tm_pal[['Mid capital']]};'>**middle 50%**</span>, ",
  "<span style=' color:{tm_pal[['High capital']]};'>**next 20%**</span>, and ",
  "<span style=' color:{tm_pal[['Highest capital']]};'>**Top 5%**</span>. ",
  "Box size show number of companies."
)

# Plot -------------------------------------------------------------------

chart <- viz_data |>
  ggplot(aes(
    area = n,
    fill = capital_bracket,
    subgroup = company_size_lab,
    label = label
  )) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_subgroup_border(
    color = color_paper,
    size = 6,
    show.legend = FALSE
  ) +
  geom_treemap_subgroup_text(
    color = color_ink,
    alpha = .6,
    fontface = "bold",
    family = "Atkinson Hyperlegible Next",
    place = "centre",
    grow = TRUE,
    show.legend = FALSE
  ) +
  geom_treemap_text(
    color = color_paper,
    place = "centre",
    size = 14,
    family = "Atkinson Hyperlegible Next"
  ) +
  scale_size_continuous(range = c(18, 32)) +
  scale_fill_manual(values = tm_pal) +
  theme_void(base_family = "Atkinson Hyperlegible Next") +
  coord_fixed() +
  labs(
    title = "Capital Stock Clustering in Brazilian Enterprises",
    subtitle = subtitle,
    caption = caption_text
  ) +
  theme_sub_panel(background = element_rect(fill = color_paper, color = NA)) +
  theme_sub_plot(
    background = element_rect(fill = color_paper, color = NA),
    margin = margin(20, 62, 6, 62),
    title.position = "plot",
    title = element_textbox_simple(
      halign = .5,
      face = "bold",
      size = 36,
      margin = margin(b = 12),
      color = color_ink,
      lineheight = .95
    ),
    subtitle = element_textbox_simple(
      halign = .5,
      size = 14,
      margin = margin(b = 38),
      color = color_gray,
    ),
    caption = element_textbox_simple(
      size = 9.5,
      halign = .5,
      color = color_gray,
      margin = margin(t = 76)
    )
  )

ggh4x::save_plot(
  here::here("2026/w04", "2026_w04-cpnj_enterprises.png"),
  plot = chart,
  height = 29.1,
  width = 21.7,
  units = "cm"
)
