# Libraries --------------------------------------------------------------


library(tidyverse)
library(ggbump)
library(ggimage)
library(ggtext)
library(patchwork)
library(showtext)
library(glue)


# Data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2024, week = 29)

ewf_matches <- tt$ewf_matches

# Data wrangling ---------------------------------------------------------

# The chart is about Women's Super League 2023-2024
wsl_23 <- ewf_matches |>
  filter(str_detect(division, "WSL") & str_starts(season, "2023")) |>
  select(!c(division, starts_with("season"), tier, starts_with("match"), attendance, result, note))


# Creating the main stats for each team for all home_games
home_game <- wsl_23 |>
  group_by(team_name = home_team_name, id = home_team_id, year = year(date), month = month(date)) |>
  summarize(
    gp = n(), # game palyed
    win = sum(home_team_win),
    draw = sum(draw),
    gs = sum(home_team_score), # goal scored
    gd = sum(home_team_score_margin), # goal difference
    pts = (win * 3) + (draw), # points
    .groups = "drop"
  ) |>
  select(!c(year, win, draw))


# monthly stats
home_game <- home_game |>
  group_by(team_name) |>
  mutate(
    pts = cumsum(pts),
    gp = cumsum(gp),
    gs = cumsum(gs),
    gd = cumsum(gd),
    period_month = case_when(
      month == 10 ~ "m1",
      month == 11 ~ "m2",
      month == 12 ~ "m3",
      month == 1 ~ "m4",
      month == 2 ~ "m5",
      month == 3 ~ "m6",
      month == 4 ~ "m7",
      month == 5 ~ "m8",
    ),
  ) |>
  select(!month)

# we do the same things for Away games
away_game <- wsl_23 |>
  group_by(team_name = away_team_name, id = away_team_id, year = year(date), month = month(date)) |>
  summarize(
    gp = n(),
    win = sum(away_team_win),
    draw = sum(draw),
    gs = sum(away_team_score),
    gd = sum(away_team_score_margin),
    pts = (win * 3) + (draw),
    .groups = "drop"
  ) |>
  select(!c(year, win, draw))


away_game <- away_game |>
  group_by(team_name) |>
  mutate(
    pts = cumsum(pts),
    gp = cumsum(gp),
    gs = cumsum(gs),
    gd = cumsum(gd),
    period_month = case_when(
      month == 10 ~ "m1",
      month == 11 ~ "m2",
      month == 12 ~ "m3",
      month == 1 ~ "m4",
      month == 2 ~ "m5",
      month == 3 ~ "m6",
      month == 4 ~ "m7",
      month == 5 ~ "m8",
    ),
  ) |>
  select(!c(month))

# Adding a column for a link to the logo
home_game$logo <- paste0("assets/", home_game$id, ".png")
away_game$logo <- paste0("assets/", away_game$id, ".png")

# removing the id column
home_game <- home_game |>
  select(!id)
away_game <- away_game |>
  select(!id)

# Joining the two datasets
wsl_23_games <- home_game |>
  full_join(away_game, by = c("team_name", "period_month", "logo")) |>
  arrange(team_name, period_month) |>
  fill(!c(gp.x, gp.y))


# Computing the final stats
wsl_23_games <- wsl_23_games |>
  rowwise() |>
  mutate(
    gp = sum(gp.x, gp.y, na.rm = TRUE),
    gs = sum(gs.x, gs.y, na.rm = TRUE),
    gd = sum(gd.x, gd.y, na.rm = TRUE),
    points = sum(pts.x, pts.y, na.rm = TRUE),
  ) |>
  select(!c(ends_with("x"), ends_with("y")))

# Adding Rank
rank_wsl_23 <- wsl_23_games |>
  arrange(period_month, desc(points), desc(gd), desc(gs)) |>
  group_by(period_month) |>
  mutate(
    rank = row_number(),
  )

m1_rank <- rank_wsl_23 |>
  filter(period_month == "m1") |>
  select(team_name, rank, logo)
m8_rank <- rank_wsl_23 |>
  filter(period_month == "m8") |>
  select(team_name, rank, logo)



# Fonts and text -------------------------------------------------------------

# Fonts
font_add_google(family = "spaceG", "Sapce Grotesk")

font_add_google(family = "roboto", "Roboto")

font_add("fa6-brands", "fonts/Font Awesome 6 Brands-Regular-400.otf")
# Colors
bg_color <- "#e5eae6"


text_font <- "roboto"
caption_font <- "spaceG"

# Title/Subtitle/Caption
# Caption

data <- glue("**Data**: English Women\\'s Football")
chart <- glue("**#TidyTuesday**: 2024 Week 29 | {data}")
X_ic <- glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")
author <- glue("**Graphic**: {X_ic} @AndriambeloRajo ")

caption_text <- glue("{chart} | {author} | #Rstats")

# Title and subtitle
title_text <- tibble(
  x = 0, y = 0,
  label = "**Chelsea Women WSL Champions 2023-2024**<br>"
)

sub_text <- tibble(
  x = 0, y = 0,
  label = "The **Women\\'s Super League (WSL)** is the highest league of women's football in England.<br>This chart shows the **monthly** evolution of the **13th WSL standing**. The numbers are the points earned based on their match results for each month."
)

annotation <- "Points awarded: 3 pts for a win, 1 pt for a draw, 0 pt for a loss.<br>In case of a tie, the rules for the ranking are as follows:<br>1) Points, 2) Goal difference, 3) Number of goals scored."

# Chart ------------------------------------------------------------------

# Title
title <- title_text |>
  ggplot(aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = bg_color,
    fill = bg_color,
    color = "#001489",
    family = text_font,
    width = unit(22, "line"),
    size = 13,
    lineheight = 1.2,
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color = bg_color, fill = bg_color))


# Subtitle
subtitle <- sub_text |>
  ggplot(aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = bg_color,
    fill = bg_color,
    color = "#132257",
    family = text_font,
    width = unit(17, "line"),
    size = 4.9,
    lineheight = 1.5
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(color = bg_color, fill = bg_color))


# Main_plot
plot <- rank_wsl_23 |>
  ggplot(aes(x = period_month, y = rank)) +
  geom_bump(
    aes(group = team_name, color = team_name),
    linewidth = 8,
    smooth = 8,
    alpha = 0.85,
    show.legend = FALSE
  ) +
  geom_label(
    aes(x = period_month, y = rank, label = points, color = team_name),
    size = 4.45,
    alpha = 0.85,
    show.legend = FALSE
  ) +
  geom_image(
    data = m1_rank,
    aes(x = 0.5, y = rank, group = team_name, image = logo),
    size = 0.05,
    asp = 1
  ) +
  geom_image(
    data = m8_rank,
    aes(x = 8.5, y = rank, group = team_name, image = logo),
    size = 0.05,
    asp = 1
  ) +
  scale_color_manual(
    values = c(
      "#ef0107", "#93bde4", "#005daa",
      "#e21a23", "#001489", "#00009e",
      "#fdbe11", "#bb0216", "#6caddf",
      "#d8020e", "#132257", "#7c2c3b"
    )
  ) +
  labs(
    caption = annotation
  ) +
  scale_x_discrete(
    expand = c(0.1, 0.1),
    position = "top"
  ) +
  scale_y_reverse(
    breaks = seq(1, 12, 1),
  ) +
  theme_minimal(base_family = text_font, base_size = 12) +
  theme(
    text = element_text(family = text_font),
    plot.background = element_rect(color = bg_color, fill = bg_color),
    panel.background = element_rect(color = bg_color, fill = bg_color),
    plot.caption = element_markdown(
      family = text_font,
      color = "#132257",
      hjust = 0.0,
      lineheight = 1.2,
      size = 9,
      margin = margin(t = 0, b = 0)
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(
      family = text_font,
      size = 14,
      face = "bold",
      color = "#132257"
    )
  )


# final_plot -------------------------------------------------------------

final_plot <- (title + subtitle) / plot +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(color = bg_color, fill = bg_color),
      plot.margin = margin(t = 20, r = 15, b = 10, l = 15),
      plot.caption = element_markdown(
        family = caption_font,
        color = "#1e3442",
        hjust = 0.5,
        lineheight = 1.2,
        size = 9,
        margin = margin(t = 10, b = 0)
      ),
    )
  )


# Saving the final_plot --------------------------------------------------


showtext_opts(dpi = 300)
showtext_auto()

ggsave(
  "wsl_23_24.png",
  final_plot,
  height = 29,
  width = 21,
  units = "cm",
  dpi = 300
)
