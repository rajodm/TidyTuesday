# Libraries ---------------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(viridis)




# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2024, week = 21)
emission <- tuesdata$emissions


# fonts & color -----------------------------------------------------------

# fonts
font_add_google("Lato")

plot_font <- "Lato"
font_add("fa6-brands", "fonts/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()

bg <- "#fbf5f1"

# Texts -------------------------------------------------------------------

# Caption
data <- str_glue("**Data**: Carbon Majors &bull;")
chart <- str_glue("#TidyTuesday: { 2024 } Week { 21 } &bull; {data}")
X_ic <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")
autor <- str_glue("**Graphic**: {X_ic} @AndriambeloRajo ")

caption_text <- str_glue("{chart} {autor} &bull; #rstat")

# title
title_text <- "Commodity production CO2 emissions (1972-2022)"

# subtitle
subtitle_text <- str_glue(
  "Carbon Majors is a database of historical production ",
  "data from 122 of the world’s largest oil, gas, coal, and cement producers.<br>",
  "This data is used to quantify the direct operational emissions and emissions ",
  "from the combustion of marketed products that can be attributed to these entities."
)


# y axis order ------------------------------------------------------------

order <- emission |>
  group_by(year, commodity) |>
  filter(year == 2022) |>
  summarise(
    total_emissions = sum(total_emissions_MtCO2e),
    .groups = "keep"
  ) |>
  arrange(total_emissions) |>
  pull(commodity)


# data --------------------------------------------------------------------

emission_df <- emission |>
  group_by(year, commodity) |>
  filter(year >= 1972) |>
  mutate(commodity = fct(commodity, levels = order)) |>
  summarize(
    total_emission = sum(total_emissions_MtCO2e),
    .groups = "keep",
  )

# Plot --------------------------------------------------------------------

emission_df |>
  ggplot(aes(x = year, y = commodity, fill = total_emission)) +
  geom_tile(color = "white", lwd = .1) +
  scale_fill_viridis(option = "D", direction = -1) +
  coord_fixed() +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(
    breaks = c(1972, 1982, 1992, 2002, 2012, 2022),
    expand = c(0, 0),
    position = "top"
  ) +
  labs(
    title = title_text,
    subtitle = subtitle_text,
    caption = caption_text
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(color = bg, fill = bg),
    panel.border = element_blank(),
    plot.margin = margin(20, 20, 20, 20),

    # Axis
    axis.title = element_blank(),
    axis.text = element_text(family = plot_family, size = 10),

    # Legend
    legend.position = "bottom",
    legend.title.position = "top",
    legend.title = element_text(color = "grey19", size = 10),
    legend.text = element_text(color = "grey19", size = 10),
    legend.ticks = element_line(color = "white"),

    # Title
    plot.title.position = "plot",
    plot.title = element_markdown(
      size = 16,
      family = plot_font,
      face = "bold",
      lineheight = 1.2,
      margin = margin(10, 0, 5, 0)
    ),


    # Subtitle
    plot.subtitle = element_markdown(
      size = 10,
      family = plot_font,
      face = "plain",
      colour = "grey30",
      lineheight = 1.2,
      margin = margin(5, 0, 20, 0),
    ),


    # Caption
    plot.caption.position = "panel",
    plot.caption = element_markdown(
      family = plot_font,
      size = 8,
      lineheight = 1.2,
      hjust = .5,
      color = "grey35",
      margin = margin(20, 0, 0, 0)
    ),
  ) +
  guides(
    fill = guide_colorbar(
      barheight = .5,
      barwidth = 20,
      title = "\nCO2 emissions (MtCO2e)",
    )
  )


# Saving the plot ---------------------------------------------------------

showtext_opts(dpi = 320)

ggsave(
  "emission.png",
  bg = bg,
  height = 6,
  width = 12,
  dpi = 320
)

showtext_auto(FALSE)
