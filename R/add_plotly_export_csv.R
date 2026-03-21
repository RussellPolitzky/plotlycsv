#' Decorate a plotly htmlwidget with a Modebar "Download CSV" button
#'
#' Adds a custom Plotly modebar button that downloads the currently-rendered
#' trace data as a CSV file. This works in static HTML contexts such as Quarto
#' and R Markdown (no Shiny server required).
#'
#' The exported CSV is based on `gd.data` (Plotly traces) available in the
#' browser at render time, i.e. what the plotly widget is drawing.
#'
#' @param widget A plotly htmlwidget (e.g. from [plotly::ggplotly()] or [plotly::plot_ly()]).
#' @param filename Filename suggested to the browser for the downloaded CSV.
#' @param selected_only If `TRUE`, export only points currently selected on the chart.
#' @param sep Character used as the field separator (default: `","`).
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
  filename = "plotly-data.csv",
  visible_only = TRUE,
  include_text = TRUE,
  include_z = FALSE,
  include_customdata = FALSE,
  selected_only = FALSE,
  sep = ","
) {
  stopifnot(inherits(widget, "htmlwidget"))


  # Register a custom button in the Plotly modebar
  # Using a standard "disk" icon SVG path
  button <- list(
    name = "Download CSV",
    title = "Download data currently visible in the chart as a CSV file",
    icon = list(
      width = 1000,
      ascent = 850,
      descent = -150,
      path = "M850 300L580 30h-430c-44 0-80 36-80 80v780c0 44 36 80 80 80h660c44 0 80-36 80-80v-550L850 300zM610 80l160 160h-160V80z M710 850h-560v-740h360v200c0 22 18 40 40 40h200v500H710z M610 450c0-11-9-20-20-20H310c-11 0-20 9-20 20s9 20 20 20h280c11 0 20-9 20-20z M610 570c0-11-9-20-20-20H310c-11 0-20 9-20 20s9 20 20 20h280c11 0 20-9 20-20z M510 690c0-11-9-20-20-20H310c-11 0-20 9-20 20s9 20 20 20h180c11 0 20-9 20-20z"
    ),
    click = htmlwidgets::JS("function(gd) { if (gd._downloadText) gd._downloadText(gd._csvFilename, gd._tracesToCSV(gd)); }")
  )

  # Initialize config if missing and add button
  if (is.null(widget$x$config)) widget$x$config <- list()
  
  # Avoid duplicate buttons if the function is called multiple times on the same widget
  current_buttons <- widget$x$config$modeBarButtonsToAdd
  button_names <- if (is.list(current_buttons)) {
    vapply(current_buttons, function(b) if (is.list(b) && !is.null(b$name)) b$name else "", character(1))
  } else {
    character(0)
  }
  
  if (!("Download CSV" %in% button_names)) {
    widget$x$config$modeBarButtonsToAdd <- c(current_buttons, list(button))
  }

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

  function tracesToCSV(gd){
    var header = ['trace_index','trace_name','point_index','x','y'%s%s%s];
    var lines = [toRow(header)];

    (gd.data || []).forEach(function(tr, ti){
      if (%s && !traceIsVisible(tr)) return;

      var name = tr.name || '';
      var xs = Array.isArray(tr.x) ? tr.x : [];
      var ys = Array.isArray(tr.y) ? tr.y : [];
      var zs = Array.isArray(tr.z) ? tr.z : [];
      var ts = Array.isArray(tr.text) ? tr.text : [];
      var cd = Array.isArray(tr.customdata) ? tr.customdata : [];
      
      var selected = tr.selectedpoints;
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
    var url = URL.createObjectURL(blob);
    var a = document.createElement('a');
    a.href = url;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    a.remove();
    URL.revokeObjectURL(url);
  }

  gd._tracesToCSV = tracesToCSV;
  gd._downloadText = downloadText;
  gd._csvFilename = '%s';
}",
    js_str(sep),
    if (include_z) ", 'z'" else "",
    if (include_text) ", 'text'" else "",
    if (include_customdata) ", 'customdata'" else "",
    if (visible_only) "true" else "false",
    if (include_z) ", zs.length" else "",
    if (include_text) ", ts.length" else "",
    if (include_customdata) ", cd.length" else "",
    if (selected_only) "true" else "false",
    if (include_z) ",\n          zs[i] !== undefined && zs[i] !== null ? zs[i] : ''" else "",
    if (include_text) ",\n          ts[i] !== undefined && ts[i] !== null ? ts[i] : ''" else "",
    if (include_customdata) ",\n          cd[i] !== undefined && cd[i] !== null ? cd[i] : ''" else "",
    js_str(filename)
  )

  htmlwidgets::onRender(widget, js)
}
