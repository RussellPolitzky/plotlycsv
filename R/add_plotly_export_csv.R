#' Decorate a plotly htmlwidget with CSV export modebar buttons
#'
#' Adds a "Download CSV" button and (optionally) a "Copy to Clipboard" button
#' to the Plotly modebar. Works in static HTML contexts such as Quarto and
#' R Markdown — no Shiny server required.
#'
#' Column headers in the exported CSV are derived from the plot's axis titles
#' when available, falling back to `"x"`, `"y"`, and `"z"`.
#'
#' @param widget A plotly htmlwidget (e.g. from [plotly::ggplotly()] or
#'   [plotly::plot_ly()]).
#' @param filename Filename suggested to the browser for the download.
#' @param visible_only If `TRUE` (default), skip hidden / `"legendonly"` traces.
#' @param include_text If `TRUE` (default), include the trace `text` field.
#' @param include_z If `TRUE`, include the `z` axis data.
#' @param include_customdata If `TRUE`, include the trace `customdata` field.
#' @param selected_only If `TRUE`, export only currently-selected points.
#' @param sep Field separator character (default `","`).
#' @param clipboard If `TRUE` (default), also add a "Copy to clipboard" button.
#'
#' @return The modified widget.
#' @export
#'
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE) &&
#'     requireNamespace("plotly", quietly = TRUE)) {
#'   library(ggplot2)
#'   library(plotly)
#'
#'   p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
#'   ggplotly(p) |>
#'     add_plotly_export_csv(filename = "mtcars.csv", selected_only = TRUE)
#' }
add_plotly_export_csv <- function(
  widget,
  filename         = "plotly-data.csv",
  visible_only     = TRUE,
  include_text     = TRUE,
  include_z        = FALSE,
  include_customdata = FALSE,
  selected_only    = FALSE,
  sep              = ",",
  clipboard        = TRUE
) {
  stopifnot(inherits(widget, "htmlwidget"))

  js_str <- function(s) gsub("'", "\\'", s, fixed = TRUE)
  b      <- function(x) if (x) "true" else "false"

  icon_path <- "M150 0h700v100H150z M425 800h150v-400h175L500 150L250 400h175z"
  clip_path <- "M350 0h300v200h200v800H150V200h200z"

  # ── Download CSV button ────────────────────────────────────────────────────
  dl_button <- list(
    name  = "Download CSV",
    title = "Download data currently visible in the chart as a CSV file",
    icon  = list(width = 1000, ascent = 850, descent = -150, path = icon_path),
    click = htmlwidgets::JS(
      "function(gd) { if (gd._downloadText) gd._downloadText(gd._csvFilename, gd._tracesToCSV(gd)); }"
    )
  )

  # ── Copy-to-clipboard button ───────────────────────────────────────────────
  cp_button <- list(
    name  = "Copy CSV",
    title = "Copy data as CSV to clipboard",
    icon  = list(width = 1000, ascent = 850, descent = -150, path = clip_path),
    click = htmlwidgets::JS(
      "function(gd) { if (gd._copyToClipboard) gd._copyToClipboard(gd._tracesToCSV(gd)); }"
    )
  )

  # ── Add buttons to widget config (guard against duplicates) ───────────────
  if (is.null(widget$x$config)) widget$x$config <- list()

  current_buttons <- widget$x$config$modeBarButtonsToAdd
  button_names <- if (is.list(current_buttons)) {
    vapply(current_buttons,
           function(b) if (is.list(b) && !is.null(b$name)) b$name else "",
           character(1))
  } else {
    character(0)
  }

  if (!("Download CSV" %in% button_names)) {
    widget$x$config$modeBarButtonsToAdd <- c(
      widget$x$config$modeBarButtonsToAdd, list(dl_button)
    )
  }
  if (clipboard && !("Copy CSV" %in% button_names)) {
    widget$x$config$modeBarButtonsToAdd <- c(
      widget$x$config$modeBarButtonsToAdd, list(cp_button)
    )
  }

  # ── onRender JS: attach helper functions to gd ────────────────────────────
  js_file <- system.file("js/add_plotly_export_csv.js", package = "plotlycsv")
  js      <- paste(readLines(js_file, warn = FALSE), collapse = "\n")

  js <- gsub("{{SEP}}",               js_str(sep),           js, fixed = TRUE)
  js <- gsub("{{VISIBLE_ONLY}}",      b(visible_only),       js, fixed = TRUE)
  js <- gsub("{{INCLUDE_Z}}",         b(include_z),          js, fixed = TRUE)
  js <- gsub("{{INCLUDE_TEXT}}",      b(include_text),       js, fixed = TRUE)
  js <- gsub("{{INCLUDE_CUSTOMDATA}}", b(include_customdata), js, fixed = TRUE)
  js <- gsub("{{SELECTED_ONLY}}",     b(selected_only),      js, fixed = TRUE)
  js <- gsub("{{FILENAME}}",          js_str(filename),      js, fixed = TRUE)

  htmlwidgets::onRender(widget, js)
}
