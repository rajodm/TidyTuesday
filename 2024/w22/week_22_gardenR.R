# Libraries --------------------------------------------------------


library(tidyverse)
library(ggtext)
library(showtext)
library(patchwork)


# Data --------------------------------------------------------------------

harvest_2020 <- read_csv(here::here("w22/data", "harvest_2020.csv"))
harvest_2021 <- read_csv(here::here("w22/data", "harvest_2021.csv"))
planting_2020 <- read_csv(here::here("w22/data", "planting_2020.csv"))
planting_2021 <- read_csv(here::here("w22/data", "planting_2021.csv"))



# fonts and colors ------------------------------------------------------------

font_add_google("Lato", "Lato")
font_add("fa6-brands", "fonts/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()

plot_font <- "Lato"

# Colors
bg <- "#fffffc"
red <- "salmon3"
green <- "palegreen4"


# Texts and captions ------------------------------------------------------

data <- str_glue("**Data**: Lisa's Vegetable garden, {{gardenR}}")
chart <- str_glue("**#TidyTuesday** 2024 Week 22 | {data}")
X_ic <- str_glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")
autor <- str_glue("**Graphic**: {X_ic} @AndriambeloRajo ")


title_text <- "Lisa Lendway's vegetable garden"
caption_text <- str_glue("{chart} <br> {autor} | #Rstats")


# Harvest ----------------------------------------------------------

harv_20 <- harvest_2020 |>
  group_by(vegetable) |>
  summarize(
    harvested_2020 = sum(weight),
  )

harv_21 <- harvest_2021 |>
  group_by(vegetable) |>
  summarise(
    harvested_2021 = sum(weight),
  )

harv <- left_join(harv_20, harv_21)

harv <- harv |>
  mutate(
    diff = harvested_2021 - harvested_2020,
    evo = round(diff * 100 / harvested_2020),
    up_down = if_else(evo > 0, "up", "down")
  )


# Planting ----------------------------------------------------------------

plant_20 <- planting_2020 |>
  group_by(vegetable) |>
  summarise(
    seeds_2020 = sum(number_seeds_planted)
  )


plant_21 <- planting_2021 |>
  group_by(vegetable) |>
  summarise(
    seeds_2021 = sum(number_seeds_planted)
  )

plant <- left_join(plant_20, plant_21)

plant <- plant |>
  group_by(vegetable) |>
  mutate(max = max(seeds_2021, seeds_2020))



# final data --------------------------------------------------------------

harv_2y <- left_join(plant, harv)

harv_2y <- harv_2y |>
  drop_na()


harv_2y <- harv_2y |>
  pivot_longer(
    cols = !c(vegetable, diff, max, evo, up_down),
    names_to = c(".value", "year"),
    names_sep = "_"
  ) |>
  mutate(
    vegetable = str_to_title(vegetable)
  )



# left plot ---------------------------------------------------------------


nudge_value <- 4.3


seed_plot <- harv_2y |>
  ggplot(aes(x = seeds, y = reorder(vegetable, max))) +
  geom_line(aes(group = vegetable), color = "#b6b3b1", linewidth = 1.5) +
  geom_point(aes(color = year), size = 4) +

  # year
  geom_text(
    data = harv_2y |>
      filter(vegetable == "Carrots"),
    aes(label = str_glue("Seeds {year}"), color = year),
    nudge_y = .6
  ) +

  # Seeds number
  geom_text(
    aes(label = seeds, color = year),
    nudge_x = if_else(
      harv_2y$seeds == harv_2y$max,
      nudge_value,
      -nudge_value
    ),
    hjust = if_else(
      harv_2y$seeds == harv_2y$max,
      0,
      1
    )
  ) +
  scale_color_manual(values = c(red, green)) +
  coord_cartesian(
    ylim = c(1, 18.5)
  ) +

  # arrow
  annotate(
    geom = "curve",
    x = 245,
    xend = 170,
    y = 15,
    yend = 16 + .1,
    curvature = .35,
    angle = 45,
    linewidth = .4,
    color = green,
    arrow = arrow(type = "closed", length = unit(.08, "inches"))
  ) +
  annotate(
    geom = "text",
    x = 240, y = 14.4,
    label = "Same number of seeds\nplanted in 2020 and 2021",
    color = green,
    family = plot_font
  ) +


  # Title and Caption
  labs(
    title = title_text,
    caption = caption_text
  ) +
  theme_minimal() +
  theme(

    # Plot
    plot.background = element_rect(color = bg, fill = bg),
    plot.title = element_text(color = "grey20", size = 18, face = "bold"),
    plot.margin = margin(10, 5, 10, 10),


    # Grids
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "#dcdddd",
      linewidth = .2,
      linetype = "dashed",
    ),

    # Axis
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold", color = "grey20"),

    # Texts
    text = element_text(family = plot_font, size = 12),
    plot.caption = element_markdown(
      hjust = 0,
      lineheight = 1.4,
      family = "Lato",
      size = rel(.75),
      color = "grey60",
      margin = margin(t = 15)
    ),

    # Legend
    legend.position = "none",
  )



# right plot --------------------------------------------------------------

harv_change <- harv_2y |>
  ggplot(aes(x = diff, y = reorder(vegetable, max))) +
  geom_text(
    aes(
      x = 0,
      label = if_else(
        evo > 0,
        paste0("+", evo, "%"),
        paste0(evo, "%")
      ),
      color = up_down,
    )
  ) +

  # Column Title
  annotate(
    geom = "text",
    x = 0,
    y = 18.6,
    label = "Harvest 2021",
    family = plot_font,
    fontface = "bold",
    color = "grey20"
  ) +
  theme_void() +
  coord_cartesian(
    xlim = c(-.05, 0.05),
    ylim = c(1, 18.5) # This needs to be the same as in the first plot
  ) +
  scale_color_manual(values = c(red, green)) +
  theme(
    text = element_text(family = plot_font, size = 10),
    plot.margin = margin(0, 0, 0, 0),
    panel.background = element_rect(fill = bg, color = bg),
    legend.position = "none"
  )


# final plot -------------------------------------------------------------

seed_plot + harv_change +
  plot_layout(
    widths = c(3, .3),
  )

# exporting plot ---------------------------------------------------------

showtext_opts(dpi = 300)

ggsave(
  "Lisa_garden.png",
  bg = bg,
  height = 7,
  width = 12,
  units = "in",
  dpi = 300
)

showtext_auto(FALSE)
