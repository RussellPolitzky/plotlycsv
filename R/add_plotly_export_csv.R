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
  js_str <- function(s) gsub("'", "\\\\'", s, fixed = TRUE)

  js <- sprintf(
    "function(el, x) {
  var gd = el.querySelector('.js-plotly-plot');
  if (!gd) {
    if (el.classList && el.classList.contains('js-plotly-plot')) gd = el;
  }
  if (!gd || !window.Plotly) return;

  var sep = '%s';

  function csvEscape(v){
    if (v === null || v === undefined) return '';
    var s = String(v);
    if (/[\\\",\\n\\r]/.test(s) || s.indexOf(sep) !== -1) {
      return '\"' + s.replace(/\"/g, '\"\"') + '\"';
    }
    return s;
  }
  function toRow(arr){ return arr.map(csvEscape).join(sep); }

  function traceIsVisible(tr){
    if (!tr) return false;
    if (tr.visible === false) return false;
    if (tr.visible === 'legendonly') return false;
    if ((!tr.x || !tr.x.length) && (!tr.y || !tr.y.length)) return false;
    return true;
  }

  function axisLabel(gd, axisKey, fallback) {
    var layout = (gd && gd.layout) || {};
    var axis   = layout[axisKey] || {};
    var title  = axis.title;
    if (!title) return fallback;
    if (typeof title === 'string') return title || fallback;
    return title.text || fallback;
  }

  function tracesToCSV(gd){
    var xLabel = axisLabel(gd, 'xaxis', 'x');
    var yLabel = axisLabel(gd, 'yaxis', 'y');
    var zLabel = axisLabel(gd, 'zaxis', 'z');
    var header = ['trace_index','trace_name','point_index', xLabel, yLabel%s%s%s];
    var lines  = [toRow(header)];

    (gd.data || []).forEach(function(tr, ti){
      if (%s && !traceIsVisible(tr)) return;

      var name = tr.name || '';
      var xs = Array.isArray(tr.x) ? tr.x : [];
      var ys = Array.isArray(tr.y) ? tr.y : [];
      var zs = Array.isArray(tr.z) ? tr.z : [];
      var ts = Array.isArray(tr.text) ? tr.text : [];
      var cd = Array.isArray(tr.customdata) ? tr.customdata : [];

      var selected     = tr.selectedpoints;
      var hasSelection = Array.isArray(selected) && selected.length > 0;

      var n = Math.max(xs.length, ys.length%s%s%s);
      for (var i = 0; i < n; i++){
        if (%s && hasSelection && !selected.includes(i)) continue;

        var xi = xs[i] !== undefined && xs[i] !== null ? xs[i] : '';
        var yi = ys[i] !== undefined && ys[i] !== null ? ys[i] : '';
        var row = [ti, name, i, xi, yi%s%s%s];
        lines.push(toRow(row));
      }
    });

    return lines.join('\\n');
  }

  function downloadText(filename, text){
    var blob = new Blob([text], {type: 'text/csv;charset=utf-8'});
    var url  = URL.createObjectURL(blob);
    var a    = document.createElement('a');
    a.href     = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    a.remove();
    URL.revokeObjectURL(url);
  }

  function copyToClipboard(text){
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(text);
    } else {
      var ta = document.createElement('textarea');
      ta.value          = text;
      ta.style.position = 'fixed';
      ta.style.opacity  = '0';
      document.body.appendChild(ta);
      ta.focus();
      ta.select();
      try { document.execCommand('copy'); } catch(e) {}
      document.body.removeChild(ta);
    }
  }

  gd._tracesToCSV     = tracesToCSV;
  gd._downloadText    = downloadText;
  gd._copyToClipboard = copyToClipboard;
  gd._csvFilename     = '%s';
}",
    js_str(sep),
    if (include_z)          ", zLabel"   else "",
    if (include_text)       ", 'text'"   else "",
    if (include_customdata) ", 'customdata'" else "",
    if (visible_only)       "true"       else "false",
    if (include_z)          ", zs.length" else "",
    if (include_text)       ", ts.length" else "",
    if (include_customdata) ", cd.length" else "",
    if (selected_only)      "true"       else "false",
    if (include_z)          ",\n          zs[i] !== undefined && zs[i] !== null ? zs[i] : ''" else "",
    if (include_text)       ",\n          ts[i] !== undefined && ts[i] !== null ? ts[i] : ''" else "",
    if (include_customdata) ",\n          cd[i] !== undefined && cd[i] !== null ? (Array.isArray(cd[i]) ? cd[i].join('|') : String(cd[i])) : ''" else "",
    js_str(filename)
  )

  htmlwidgets::onRender(widget, js)
}
