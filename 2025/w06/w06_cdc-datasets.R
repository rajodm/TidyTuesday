# Packages needed --------------------------------------------------------

library(tidyverse)
library(ggtext)
library(tidytext)
library(showtext)
library(gghighlight)

# data -------------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2025, week = 6)

cdc_ds <- tt$cdc_datasets

# A list of words to remove from the data
cdc_stopwords <- c(
  # Structural terms
  "table",
  "data",
  "format",
  "friendly",
  "gis",
  "system",
  "metrics",
  "interactive",
  "selected",
  # Time-related
  "weekly",
  "monthly",
  "week",
  "months",
  "month",
  "days",
  "weekly",
  "monthly",
  "week",
  "months",
  "month",
  "days",
  "ah",
  # Org/systems
  "ii.",
  "cdc",
  "nndss",
  "dqs",
  "nchs",
  "brfss",
  "nhis",
  "nhanes",
  "hhs",
  "zcta",
  "provisional",
  "cumulative",
  "estimates",
  "art",
  # Measurements/Stats
  "counts",
  "rates",
  "rate",
  "percent",
  "percentage",
  "reported",
  "probable",
  "confirmed",
  "total",
  "updated",
  "estimates",
  "provisional",
  "cumulative",
  # Geographic/Administrative
  "county",
  "region",
  "national",
  "jurisdiction",
  "local",
  "territory",
  "territorial",
  "u.s.",
  # Demographic terms
  "age",
  "sex",
  "race",
  "hispanic",
  "place",
  "origin",
  "ethnicity"
)

title_words <- cdc_ds |>
  select(dataset_url) |>
  # Remove the url and keep dataset title
  separate_wider_regex(
    cols = dataset_url,
    patterns = c(
      "^https://.*/",
      ds_name = ".*",
      extension = "\\..*"
    )
  ) |>
  # Replace underscores with a space
  mutate(ds_name = str_replace_all(ds_name, "_", " ")) |>
  # I don't really understand how the "token = 'regex' works but it
  # keeps word like 'covid-19'
  unnest_tokens(word, ds_name, token = "regex") |>
  anti_join(stop_words) |>
  filter(
    # Remove words in the list we created
    !word %in% cdc_stopwords &
      # Remove words ending with '-meta'
      !str_detect(word, "-meta") &
      # Remove numbers and dates
      !str_detect(word, "^[0-9]+$")
  ) |>
  mutate(
    # normalize variation of some frequent words
    word = case_match(
      word,
      "diseases" ~ "disease",
      "adult" ~ "adults",
      "vaccinations" ~ "vaccination",
      c("covid", "covid19") ~ "covid-19",
      "deaths" ~ "death",
      "influenzae" ~ "influenza",
      "infections" ~ "infection",
      "surveys" ~ "survey",
      c("vaccines", "vaccines.gov") ~ "vaccine",
      "fevers" ~ "fever",
      "drugs" ~ "drug",
      "child" ~ "children",
      .default = word
    )
  ) |>
  count(word, sort = TRUE) |>
  mutate(topics = str_to_title(word))

# Fonts ------------------------------------------------------------------

font_add_google("Roboto", family = "roboto", bold.wt = 600)
font_add(
  "fa6-brands",
  "archive/2024/tt2024_w50/fonts/Font Awesome 6 Brands-Regular-400.otf"
)

showtext_auto()
showtext_opts(dpi = 300)

# Captions ---------------------------------------------------------------

data <- glue::glue("**Data**: CDC Datasets backup on archive.org")
chart <- glue::glue("<b>#TidyTuesday</b>: 2025 Week 06 | {data}")
bsky <- glue::glue("<span style='font-family:fa6-brands'>&#xe671;</span>")
author <- glue::glue("**Graphic**: {bsky} @rajodm")

caption_text <- glue::glue("{chart}<br>{author} | #rstats")

# Texts ------------------------------------------------------------------

plot_title <- "Among the CDC datasets backed up on archive.org, Covid-19 is the most common topic"
plot_subtitle <- "NOTE: Demographic terms (e.g. age, race, gender), as well as geographical, measurement, and time-related terms have been excluded from the analysis."

# Colors -----------------------------------------------------------------

color_bg <- "#f8f4ed"
color_accent <- "#1d2432"
color_accent_lighter <- "#8e8e8e"
color_grey <- "#d0cdc7"

# Plot -------------------------------------------------------------------

word_frequency <- title_words |>
  slice_max(n = 15, order_by = n) |>
  ggplot(aes(x = reorder(topics, n), y = n)) +
  geom_col(fill = color_accent) +
  gghighlight(
    word == "covid-19",
    unhighlighted_params = list(
      fill = color_grey
    )
  ) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    y = "Word frequency",
    caption = caption_text
  ) +
  coord_flip(
    expand = FALSE,
    clip = "off"
  ) +
  theme_minimal(base_family = "roboto") +
  theme(
    plot.background = element_rect(fill = color_bg, color = color_bg),
    plot.title.position = "plot",
    plot.title = element_textbox_simple(
      color = color_accent,
      face = "bold",
      size = 16,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_textbox_simple(
      color = color_accent_lighter,
      size = 8,
      margin = margin(b = 16)
    ),
    plot.caption.position = "plot",
    plot.caption = element_markdown(
      color = color_accent_lighter,
      hjust = 0,
      lineheight = 1.2,
      size = 6
    ),
    axis.title = element_text(color = color_accent, face = "bold", size = 9),
    axis.title.y = element_blank(),
    axis.text = element_text(color = color_accent, size = 8),
    axis.text.y = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      linewidth = 0.2,
      linetype = "dashed",
      color = color_grey
    ),
    plot.margin = margin(20, 15, 10, 15)
  )

ggsave(
  "2025_w06-cdc_datasets.png",
  word_frequency,
  width = 21,
  height = 14.8,
  units = "cm"
)
