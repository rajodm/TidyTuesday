generate_caption <- function(source, week, tt_year = 2026) {
  glue::glue(
    "**Source**: {source}<br>",
    "**TidyTuesday**: {tt_year} Week {week} | ",
    "**Visualizastion**: Andriambelo Rajo | #rstats"
  )
}
