#' Add a global auto-decorator for all Plotly charts in a document
#'
#' Injects a JavaScript payload that automatically adds "Download CSV" and
#' (optionally) "Copy to Clipboard" modebar buttons to every Plotly widget
#' rendered on the page. Useful for large Quarto / R Markdown documents where
#' you want consistent export functionality without calling
#' `add_plotly_export_csv()` for each individual plot.
#'
#' @param filename Default filename for downloads. When `NULL` (the default),
#'   each plot derives its name from the nearest ancestor `<div>` id set by
#'   Quarto / knitr (figure labels like `fig-*` are preferred over chunk
#'   names). Falls back to `"plotly-data.csv"`.
#' @param selected_only If `TRUE`, export only currently-selected points.
#' @param visible_only If `TRUE` (default), skip traces that are hidden or
#'   set to `"legendonly"`.
#' @param include_text If `TRUE` (default), include the trace `text` field.
#' @param include_z If `TRUE`, include the `z` axis data (heatmaps, 3-D
#'   scatter, surface plots, etc.).
#' @param include_customdata If `TRUE`, include the trace `customdata` field.
#' @param sep Field separator character (default `","`).
#' @param clipboard If `TRUE` (default), also add a "Copy to clipboard"
#'   modebar button that copies the CSV text to the clipboard.
#'
#' @return An `htmltools::tags$script` tag containing the JavaScript payload.
#' @export
#'
#' @examples
#' if (requireNamespace("htmltools", quietly = TRUE)) {
#'   # In a Quarto or R Markdown setup chunk:
#'   # use_plotly_csv()                          # auto filename, clipboard on
#'   # use_plotly_csv(filename = "report.csv")   # fixed filename
#'   # use_plotly_csv(include_z = TRUE)          # include z axis
#' }
use_plotly_csv <- function(
  filename         = NULL,
  selected_only    = FALSE,
  visible_only     = TRUE,
  include_text     = TRUE,
  include_z        = FALSE,
  include_customdata = FALSE,
  sep              = ",",
  clipboard        = TRUE
) {
  js_str <- function(s) gsub("'", "\\'", s, fixed = TRUE)
  b      <- function(x) if (x) "true" else "false"

  js_file <- system.file("js/use_plotly_csv.js", package = "plotlycsv")
  script  <- paste(readLines(js_file, warn = FALSE), collapse = "\n")

  script <- gsub("{{GLOBAL_SEP}}",              js_str(sep),                                                          script, fixed = TRUE)
  script <- gsub("{{GLOBAL_FILENAME}}",         if (is.null(filename)) "null" else paste0("'", js_str(filename), "'"), script, fixed = TRUE)
  script <- gsub("{{GLOBAL_CLIPBOARD}}",        b(clipboard),                                                          script, fixed = TRUE)
  script <- gsub("{{GLOBAL_SELECTED_ONLY}}",    b(selected_only),                                                      script, fixed = TRUE)
  script <- gsub("{{GLOBAL_VISIBLE_ONLY}}",     b(visible_only),                                                       script, fixed = TRUE)
  script <- gsub("{{GLOBAL_INCLUDE_TEXT}}",     b(include_text),                                                       script, fixed = TRUE)
  script <- gsub("{{GLOBAL_INCLUDE_Z}}",        b(include_z),                                                          script, fixed = TRUE)
  script <- gsub("{{GLOBAL_INCLUDE_CUSTOMDATA}}", b(include_customdata),                                               script, fixed = TRUE)

  htmltools::tags$script(htmltools::HTML(script))
}
