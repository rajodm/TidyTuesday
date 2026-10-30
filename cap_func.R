generate_caption <- function(
  source,
  week,
  tt_year = 2026,
  src_txt = "Source",
  note = NULL
) {
  glue::glue(
    "{if (!is.null(note)) paste0('**Note**: ', note)}<br>",
    "**{src_txt}**: {source}<br>",
    "**TidyTuesday**: {tt_year} Week {week} | ",
    "**Visualizastion**: Andriambelo Rajo | #rstats",
    .null = ""
  )
}
