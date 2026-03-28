#' Add a global auto-decorator for all Plotly charts in a document
#'
#' This function injects a small JavaScript payload into the document that
#' automatically adds a "Download CSV" modebar button to every Plotly widget
#' rendered on the page. This is useful for large Quarto or R Markdown
#' documents where you want consistent export functionality without calling
#' `add_plotly_export_csv()` for each individual plot.
#'
#' @param filename Default filename suggested for the download. When `NULL`
#'   (the default), each plot derives its filename from the `id` of the nearest
#'   ancestor `<div>` that carries one — which Quarto / knitr sets to the chunk
#'   or figure label — with `.csv` appended. If a figure label exists (e.g. `fig-my-plot`),
#'   it is strictly preferred over the chunk label. If no such ancestor is found,
#'   the name falls back to `"plotly-data.csv"`. Supply an explicit string to use
#'   the same filename for every plot in the document.
#' @param selected_only If `TRUE`, export only points currently selected on the chart.
#' @param sep Character used as the field separator (default: `","`).
#'
#' @return An [htmltools::tagList] containing the JavaScript payload.
#' @export
#'
#' @examples
#' if (requireNamespace("htmltools", quietly = TRUE)) {
#'   # In a Quarto or R Markdown setup chunk:
#'   # use_plotly_csv()           # auto filename from chunk label
#'   # use_plotly_csv(filename = "report-data.csv")  # fixed filename
#' }
use_plotly_csv <- function(filename = NULL, selected_only = FALSE, sep = ",") {
  js_str <- function(s) gsub("'", "\\\\'", s, fixed = TRUE)
  
  script <- sprintf(
    "(function() {
  var iconPath = 'M850 300L580 30h-430c-44 0-80 36-80 80v780c0 44 36 80 80 80h660c44 0 80-36 80-80v-550L850 300zM610 80l160 160h-160V80z M710 850h-560v-740h360v200c0 22 18 40 40 40h200v500H710z M610 450c0-11-9-20-20-20H310c-11 0-20 9-20 20s9 20 20 20h280c11 0 20-9 20-20z M610 570c0-11-9-20-20-20H310c-11 0-20 9-20 20s9 20 20 20h280c11 0 20-9 20-20z M510 690c0-11-9-20-20-20H310c-11 0-20 9-20 20s9 20 20 20h180c11 0 20-9 20-20z';

  var global_sep = '%s';

  function csvEscape(v, s_sep){
    var delim = s_sep || ',';
    if (v === null || v === undefined) return '';
    var s = String(v);
    if (/[\\\",\\n\\r]/.test(s) || s.indexOf(delim) !== -1) return '\"' + s.replace(/\"/g, '\"\"') + '\"';
    return s;
  }
  function toRow(arr, s_sep){ return arr.map(function(x){ return csvEscape(x, s_sep); }).join(s_sep || ','); }

  function traceIsVisible(tr){
    if (!tr) return false;
    if (tr.visible === false) return false;
    if (tr.visible === 'legendonly') return false;
    if ((!tr.x || !tr.x.length) && (!tr.y || !tr.y.length)) return false;
    return true;
  }

  function tracesToCSV(gd, opts){
    var sep = opts.sep || ',';
    var header = ['trace_index','trace_name','point_index','x','y'];
    if (opts.include_text) header.push('text');
    var lines = [toRow(header, sep)];

    (gd.data || []).forEach(function(tr, ti){
      if (opts.visible_only && !traceIsVisible(tr)) return;
      var name = tr.name || '';
      var xs = Array.isArray(tr.x) ? tr.x : [];
      var ys = Array.isArray(tr.y) ? tr.y : [];
      var ts = Array.isArray(tr.text) ? tr.text : [];
      
      var selected = tr.selectedpoints;
      var hasSelection = Array.isArray(selected) && selected.length > 0;

      var n = Math.max(xs.length, ys.length, ts.length);
      for (var i = 0; i < n; i++){
        if (opts.selected_only && hasSelection && !selected.includes(i)) continue;

        var row = [ti, name, i, xs[i] ?? '', ys[i] ?? ''];
        if (opts.include_text) row.push(ts[i] ?? '');
        lines.push(toRow(row, sep));
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

  var global_filename = %s;

  function chunkFilename(gd) {
    if (global_filename !== null) return global_filename;
    // Walk up the DOM to find ancestor <div> elements with an id.
    // Quarto / knitr sets that id to the chunk or figure label.
    // We prefer figure labels (which typically start with 'fig-') over chunk labels.
    var el = gd.parentElement;
    var fallbackId = null;
    
    while (el) {
      if (el.tagName === 'DIV' && el.id && el.id.length > 0) {
        if (el.id.indexOf('fig-') === 0) {
          return el.id + '.csv';
        }
        if (!fallbackId) {
          fallbackId = el.id;
        }
      }
      el = el.parentElement;
    }
    
    if (fallbackId) return fallbackId + '.csv';
    return 'plotly-data.csv';
  }

  function decoratePlot(gd) {
    if (!gd || gd._tracesToCSV || !window.Plotly) return;

    var filename = chunkFilename(gd);
    var opts = {
      visible_only: true,
      include_text: true,
      selected_only: %s,
      sep: global_sep
    };

    gd._tracesToCSV = function(gd) { return tracesToCSV(gd, opts); };
    gd._downloadText = downloadText;
    gd._csvFilename  = filename;

    var button = {
      name: 'Download CSV Auto',
      title: 'Download data as CSV (Auto-decorator)',
      icon: { width: 1000, ascent: 850, descent: -150, path: iconPath },
      click: function(gd) { downloadText(gd._csvFilename, gd._tracesToCSV(gd)); }
    };

    // Check if a similar button already exists
    var hasButton = false;
    try {
      var modebar = gd.querySelector('.modebar-container');
      if (modebar && modebar.textContent.includes('CSV')) hasButton = true;
    } catch(e) {}

    if (!hasButton) Plotly.addButtons(gd, button);
  }

  function init() {

    document.querySelectorAll('.js-plotly-plot').forEach(decoratePlot);
    var observer = new MutationObserver(function(mutations) {
      mutations.forEach(function(mutation) {
        mutation.addedNodes.forEach(function(node) {
          if (node.nodeType === 1) {
            if (node.classList && node.classList.contains('js-plotly-plot')) decoratePlot(node);
            node.querySelectorAll('.js-plotly-plot').forEach(decoratePlot);
          }
        });
      });
    });
    observer.observe(document.body, { childList: true, subtree: true });
  }

  if (document.readyState === 'complete') init();
  else window.addEventListener('load', init);
})();",
    js_str(sep),
    if (is.null(filename)) "null" else paste0("'", js_str(filename), "'"),
    if (selected_only) "true" else "false"
  )

  htmltools::tags$script(htmltools::HTML(script))
}

