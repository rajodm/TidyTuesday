library(tidyverse)
library(sf)
library(rnaturalearth)
library(gt)
library(gtExtras)
library(htmltools)
library(patchwork)

pal <- c(
  "1" = "#9467BD",
  "2" = "#009988",
  "3" = "#F2B701",
  "4" = "#88CCEE",
  "5" = "#E65518"
)

tt <- tidytuesdayR::tt_load(2025, week = 42)
hist_station <- tt$historic_station_met
station_meta <- tt$station_meta

z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

get_avg_temp <- function(month_numbers, temp_var) {
  hist_station |>
    filter(month %in% c(month_numbers)) |>
    summarize(
      .by = c(station, year),
      avg = mean({{ temp_var }}, na.rm = TRUE)
    ) |>
    summarize(
      .by = station,
      "avg_{{temp_var}}" := mean(avg, na.rm = TRUE)
    )
}

summer_temp <- get_avg_temp(c(6:8), tmax)

winter_temp <- get_avg_temp(c(12, 1, 2), tmin)

sum_wint_temp <- summer_temp |>
  full_join(winter_temp, by = join_by(station))

station_summary <- hist_station |>
  summarize(
    .by = c(station, year),
    annual_rainfall = sum(rain, na.rm = TRUE),
    annual_sunshine = sum(sun, na.rm = TRUE),
    mean_tmax = mean(tmax, na.rm = TRUE),
    mean_tmin = mean(tmin, na.rm = TRUE),
    annual_af = sum(af, na.rm = TRUE)
  ) |>
  summarize(
    .by = station,
    avg_rainfall = mean(annual_rainfall, na.rm = TRUE),
    avg_sunshine = mean(annual_sunshine, na.rm = TRUE),
    mean_tmax = mean(mean_tmax, na.rm = TRUE),
    mean_tmin = mean(mean_tmin, na.rm = TRUE),
    avg_af = mean(annual_af, na.rm = TRUE)
  )

final_data <- sum_wint_temp |>
  full_join(station_summary)

final_data_scaled <- final_data |>
  mutate(
    across(where(is.numeric), \(x) z_score(x))
  )

set.seed(123)
clusters <- kmeans(final_data_scaled[, -1], centers = 5)

final_data$clust <- clusters$cluster

final_data <- final_data |>
  mutate(clust = factor(clust)) |>
  arrange(desc(clust)) |>
  mutate(clust = fct_inorder(clust))

station_pos <- station_meta |>
  st_as_sf(coords = c("lng", "lat"), crs = 4326) |>
  st_transform(crs = 27700)

stations <-
  station_pos |>
  full_join(final_data)

uk <- ne_states(
  country = "United Kingdom"
) |>
  st_transform(crs = 27700)

map <- uk |>
  ggplot() +
  geom_sf(
    aes(geometry = geometry),
    fill = "#d3d3d3",
    color = "#e5e5e5",
  ) +
  geom_sf(
    data = stations,
    color = "gray40",
    size = 4,
    shape = 16,
    show.legend = FALSE
  )

map1 <- uk |>
  ggplot() +
  geom_sf(
    aes(geometry = geometry),
    fill = "#d3d3d3",
    color = "#e5e5e5",
  ) +
  geom_sf(
    data = stations,
    aes(fill = clust),
    color = "gray20",
    size = 5,
    shape = 16,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = pal
  ) +
  theme_void()


final_table <- final_data |>
  summarize(
    .by = clust,
    n = as.character(n()),
    tmax = mean(mean_tmax),
    tmin = mean(mean_tmin),
    sun = round(mean(avg_sunshine)),
    rain = mean(avg_rainfall),
    af = mean(avg_af),
    temp = glue::glue("{round(max(tmax), 2)} - {round(min(tmin),2)}"),
    avg_summer_temp = mean(avg_tmax),
    avg_winter_temp = mean(avg_tmin),
  ) |>
  select(!c(tmax, tmin)) |>
  mutate(
    af = round(af),
    across(where(is.numeric), \(x) round(x, 2)),
  )

create_badge <- function(x, y) {
  map2(x, y, function(clust, n) {
    htmltools::div(
      style = glue::glue(
        "display: inline-block; padding: 2px 10px; border-radius: 8px;",
        "font-weight: 500; font-size: 12pt; foreground: '#000000';",
        "background:{pal[clust]}; "
      ),
      html(glue::glue("{n}<br>stations"))
    ) |>
      as.character() |>
      html()
  })
}

gt_table <- final_table |>
  mutate(
    ggplot = clust,
    clust_b = create_badge(as.character(clust), n),
    .before = 1
  ) |>
  select(!n) |>
  gt() |>
  text_transform(
    locations = cells_body(columns = ggplot),
    fn = function(x) {
      map(1:nrow(final_table), function(i) {
        current_clust <- final_table$clust[i]
        plot <- map +
          geom_sf(
            data = stations |>
              filter(
                clust == current_clust
              ),
            aes(fill = clust),
            color = "gray35",
            size = 9.2,
            shape = 21,
            show.legend = FALSE
          ) +
          scale_fill_manual(
            values = pal
          ) +
          coord_sf(expand = FALSE, clip = "off") +
          theme_void()

        plot |>
          ggplot_image(height = px(150))
      })
    }
  ) |>
  data_color(
    columns = c(rain, sun, af, avg_summer_temp, avg_winter_temp),
    fn = scales::col_numeric(
      palette = c("#ffffff", "#d9d9d9"),
      domain = NULL
    ),
    alpha = .7
  ) |>
  data_color(
    columns = c(avg_winter_temp),
    fn = scales::col_numeric(
      palette = c("#a5bed8", "#ffffff"),
      domain = NULL
    ),
    alpha = .6
  ) |>
  data_color(
    columns = c(avg_summer_temp),
    fn = scales::col_numeric(
      palette = c("#ffffff", "#d8bca5"),
      domain = NULL
    ),
    alpha = .6
  ) |>
  tab_spanner(
    label = "Temperature (°C)",
    columns = contains("temp")
  ) |>
  cols_label(
    ggplot = "",
    clust_b = "",
    af = html("Days of<br>air frost"),
    sun = html("Sunshine<br>(Hours)"),
    rain = html("Rainfall<br>(mm/year)"),
    avg_summer_temp = html("Summer max"),
    avg_winter_temp = html("Winter min"),
    temp = html("Mean (max - min)")
  ) |>
  tab_header(
    title = md("**UK Climate Zones Based on Historical Weather Patterns**"),
    subtitle = md(
      "Five distinct climate clusters identified through `k-means` analysis"
    )
  ) |>
  cols_align(align = "center") |>
  cols_hide(clust) |>
  opt_table_font(
    font = "Inter"
  ) |>
  tab_options(
    data_row.padding = px(5),
    table.border.top.color = "white",
    table.border.bottom.color = "gray80",
    column_labels.border.top.color = "white",
    column_labels.border.bottom.color = "gray80",
  ) |>
  tab_footnote(
    md(
      "**Note:** All values are averages. Actual conditions vary significantly by year and season."
    ),
    placement = "right"
  ) |>
  tab_source_note(
    source_note = md(
      glue::glue(
        "**Source**: UK Met Office Historical Station Data<br>",
        "**#TidyTuesday**: 2025 Week 42 | **Visualization**: @rajodm.bsky.social"
      )
    )
  )

gt_table |>
  gt::gtsave(
    "2025_w42-uk_weather.png",
    vwidth = 2480,
    vheight = 3507,
    expand = c(25, 15, 10, 15)
  )
