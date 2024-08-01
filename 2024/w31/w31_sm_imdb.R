
# packages ---------------------------------------------------------------


library(tidyverse)
library(ggtext)
library(ggarrow)
library(showtext)
library(patchwork)

# Data -------------------------------------------------------------------

summer_movies <- read_csv(here::here("tt2024_w31/data/summer_movies.csv"))


# Fonts & Colors ------------------------------------------------------------------

font_add_google("Lora", family = "Lora")
font_add_google("Poppins", family = "Poppins")
font_add("fa6-brands", "fonts/Font Awesome 6 Brands-Regular-400.otf")


title_fonts <- "Lora"
info_fonts <- "Poppins"
sm_green <- "#04e058"
sm_orange <- "#ff7f02"
sm_blue <- "#3fbcf6"
sm_bg_dark <- "#14171c"
sm_grey <- "#d4d4d4"


# Caption ----------------------------------------------------------------


data <- glue::glue("**Data**: IMDb")
chart <- glue::glue("**#TidyTuesday**: 2024 week 31 | {data}")
X_icon <- glue::glue("<span style='font-family:fa6-brands'>&#xe61b;</span>")
author <- glue::glue("**Graphic**: {X_icon} @AndriambeloRajo")

caption_text <- glue::glue("{chart} | {author} | #RStats")



# Useful stuffs -----------------------------------------------------------


ratings <- skimr::skim(summer_movies$average_rating)
votes <- skimr::skim(summer_movies$num_votes)

summer_movies_rating <- tribble(
  ~rating_cat, ~score,
  "Great", ratings$numeric.p75,
  "Good", ratings$numeric.p50,
  "Bad", ratings$numeric.p25,
  "Terrible", ratings$numeric.p0
)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

as_breaks <- normalize(c(5649, 11298, 16947, 28245, 45192, 73437, 118629, 192066, 310695, 502761))

# Data Preparation -------------------------------------------------------


# filter and categories
summer_movies <- summer_movies |>
  filter(num_votes > votes$numeric.p75) |>
  mutate(
    rating_cat = case_when(
      average_rating < ratings$numeric.p25 ~ "Bad",
      average_rating > ratings$numeric.p75 ~ "Great",
      TRUE ~ "Good"
    )
  )

# scaling and formating data
summer_movies <- summer_movies |>
  arrange(year) |>
  mutate(
    yearly_num = row_number(),
    tconst = fct_reorder(tconst, yearly_num),
    norm_votes = normalize(num_votes),
    genres = str_replace_all(genres, ",",", ")
  )


# Popular movies to display
popular_sm <- summer_movies |>
  arrange(desc(num_votes)) |>
  slice_head(n = 5) |>
  mutate(
    num = row_number()
  )


# Texts ------------------------------------------------------------------


main_title_text <- glue::glue("<span style ='font-family: Lora; font-size: 26pt'>**Rating of Popular Summer Movies\\***</span>")
main_subtitle_text <- glue::glue("<span style = 'font-family: Poppins; font-size: 8pt'>\\*Movies with the word \\'summer\\' in the title</span>")



title_text <- tribble(
  ~x, ~y, ~label,
  -7.5, 10, main_title_text,
  -7.5, 8.5, main_subtitle_text
)


description_text <- glue::glue(
  "<span style = 'font-family: Poppins; font-size: 11pt'>This chart shows the **most popular movies** (by number of votes) on IMDb with the word \\'summer\\' in the title.<br>There are **{length(summer_movies$tconst)} movies** with **at least {votes$numeric.p75} votes**. They are **arranged by year of release** from 1935 to 2024.<br>The **colors** are their **categorie** and more popular movies are represented with a brighter line:<br>- <span style = 'color: {sm_green}; font-family: Lora'>**great movies**</span> (rating > {ratings$numeric.p75}),<br>- <span style = 'color: {sm_orange}; font-family: Lora'>**good movies**</span> (rating ≥ {ratings$numeric.p25}),<br>- <span style = 'color: {sm_blue}; font-family: Lora'>**average - bad movies**</span> (rating < {ratings$numeric.p25}).<br><br>The Labels show the five most popular movies by number of votes, along with the movie descriptions.<br><span style = ' font-family: Poppins; font-size: 8pt'>Theme inspiration:</span><span style = 'color: {sm_green}; font-family: Poppins; font-size: 8pt'>#Letterboxd</span></span>"
)

plot_description <- tibble(
  x = 0, y = 0, label = description_text
)

# Plots ------------------------------------------------------------------

side_text <- plot_description |>
  ggplot(aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    color = sm_grey,
    family = info_fonts,
    box.color = sm_bg_dark,
    fill = sm_bg_dark,
    width = unit(16, "lines"),
    lineheight = 1.5
  ) +
  coord_cartesian(
    expand = FALSE,
    clip = "off"
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(color = sm_bg_dark, fill = sm_bg_dark)
  )



# Main Plot
main_plot <- summer_movies |>
  ggplot(aes(x = tconst, y = average_rating)) +
  # Segments
  geom_segment(
    aes(y = ratings$numeric.p0, yend = average_rating, color = rating_cat, alpha = norm_votes),
    linewidth = 1.4,
    lineend = "butt",
    show.legend = FALSE
  ) +
  # Points
  geom_point(
    aes(y = average_rating, color = rating_cat, alpha = norm_votes, size = norm_votes),
    show.legend = FALSE
  ) +
  # Convert_to_circular_plot
  coord_polar() +
  # Center_circle
  geom_rect(
    aes(ymin = 0, ymax = 0, xmin = 0, xmax = 0),
    color = sm_bg_dark, fill = sm_bg_dark
  ) +
  # Label for the most popular movies
  geom_textbox(
    data = popular_sm,
    aes(x = tconst, y = average_rating,
      label = glue::glue(
        "<span style ='font-family: Lora; font-size: 12.2pt'>**{num}. {original_title}**</span> ",
        "<span style = 'font-family: Poppins; font-size: 10pt'><br>{genres}</span><br>",
        "<span style = 'font-size: 10pt'>&starf;</span>\b<span style = 'font-family: Poppins; font-size: 10pt'>**{average_rating}**</span> ",
        "<span style = 'font-family: Poppins; font-size: 10pt'>({scales::number(num_votes)} votes) &bull; </span>",
        "<span style ='font-family: Poppins; font-size: 10pt'>{year}</span>"
      ),
    ),
    lineheight = 1.5,
    alpha = 0.8,
    color = sm_bg_dark,
    width = unit(2.2, "inch"),
    box.padding = unit(c(4.5, 4.5, 4.5, 4.5), "pt"),
    box.r = unit(4.5, units = "pt"),
    size = 3.2,
    nudge_y = 7,
    nudge_x = 0,
    show.legend = FALSE
  ) +
  # Adding arrows pointing to the labels
  geom_arrow_segment(
    data = popular_sm,
    aes(x = tconst, y = average_rating + 0.3,yend = average_rating + 6.2),
    arrow_fins = arrow_fins_minimal(),
    arrow_head = arrow_cup(),
    color = sm_grey,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    expand = c(0,0)
  ) +
  # Yaxis lines
  geom_hline(
  data = summer_movies_rating,
  aes(yintercept = score),
  color = sm_grey,
  linewidth = 0.2,
  linetype = "dashed",
  alpha = 0.4
  ) +
  # Yaxis Labels
  geom_richtext(
    data = summer_movies_rating,
    aes(x = -7.6, y = score, label = glue::glue("&starf;<span style = 'font-family: Poppins; font-size: 6pt'>{score}</span>")),
    fill = NA,
    label.color = NA,
    color = sm_grey,
    size = 2.3,
  ) +
  # Title
  geom_richtext(
    data = title_text,
    aes(x = x, y = y, label = label),
    fill = NA,
    label.color = NA,
    color = sm_grey,
  ) +
  # Rescaling Xaxis for a better aesthetic
  scale_x_discrete(
    expand = c(0.04, 0.04)
  ) +
  scale_color_manual(values = c(sm_blue, sm_orange,sm_green)) +
  scale_size_binned(
    breaks = as_breaks
  ) +
  scale_alpha_binned(
    breaks = as_breaks
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = sm_bg_dark, color = sm_bg_dark),
    panel.background = element_rect(fill = sm_bg_dark, color = sm_bg_dark),
  )



# Final plot
final_plot <- main_plot + side_text +
  plot_layout(
    widths = c(3,1)
  ) +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.background = element_rect(fill = sm_bg_dark, color = sm_bg_dark),
      plot.margin = margin(0, 30, 0, 0),
      plot.caption = element_markdown(
        color = sm_grey,
        family = title_fonts,
        size = 8,
        hjust = 0.5,
        margin = margin(b = 10)
      )
    )
  )




# end --------------------------------------------------------------------

showtext_auto()
showtext_opts(dpi = 300)
ggsave(
  "sm_imdb.png",
  final_plot,
  height = 21,
  width = 29,
  units = "cm"
)
