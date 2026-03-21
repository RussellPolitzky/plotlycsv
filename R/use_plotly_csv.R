#' Add a global auto-decorator for all Plotly charts in a document
#'
#' This function injects a small JavaScript payload into the document that
#' automatically adds a "Download CSV" modebar button to every Plotly widget
#' rendered on the page. This is useful for large Quarto or R Markdown
#' documents where you want consistent export functionality without calling
#' `add_plotly_export_csv()` for each individual plot.
#'
#' @param filename Default filename suggested (default: "plotly-data.csv").
#'
#' @return An [htmltools::tagList] containing the JavaScript payload.
#' @export
#'
#' @examples
#' if (requireNamespace("htmltools", quietly = TRUE)) {
#'   # In a Quarto or R Markdown setup chunk:
#'   # use_plotly_csv()
#' }
use_plotly_csv <- function(filename = "plotly-data.csv") {
  js_str <- function(s) gsub("'", "\\\\'", s, fixed = TRUE)
  
  script <- sprintf(
    "(function() {
  var iconPath = 'M850 300L580 30h-430c-44 0-80 36-80 80v780c0 44 36 80 80 80h660c44 0 80-36 80-80v-550L850 300zM610 80l160 160h-160V80z M710 850h-560v-740h360v200c0 22 18 40 40 40h200v500H710z M610 450c0-11-9-20-20-20H310c-11 0-20 9-20 20s9 20 20 20h280c11 0 20-9 20-20z M610 570c0-11-9-20-20-20H310c-11 0-20 9-20 20s9 20 20 20h280c11 0 20-9 20-20z M510 690c0-11-9-20-20-20H310c-11 0-20 9-20 20s9 20 20 20h180c11 0 20-9 20-20z';

  function csvEscape(v){
    if (v === null || v === undefined) return '';
    var s = String(v);
    if (/[\\\",\\n\\r]/.test(s)) return '\"' + s.replace(/\"/g, '\"\"') + '\"';
    return s;
  }
  function toRow(arr){ return arr.map(csvEscape).join(','); }

  function traceIsVisible(tr){
    if (!tr) return false;
    if (tr.visible === false) return false;
    if (tr.visible === 'legendonly') return false;
    if ((!tr.x || !tr.x.length) && (!tr.y || !tr.y.length)) return false;
    return true;
  }

  function tracesToCSV(gd, opts){
    var header = ['trace_index','trace_name','point_index','x','y'];
    if (opts.include_text) header.push('text');
    var lines = [toRow(header)];

    (gd.data || []).forEach(function(tr, ti){
      if (opts.visible_only && !traceIsVisible(tr)) return;
      var name = tr.name || '';
      var xs = Array.isArray(tr.x) ? tr.x : [];
      var ys = Array.isArray(tr.y) ? tr.y : [];
      var ts = Array.isArray(tr.text) ? tr.text : [];
      var n = Math.max(xs.length, ys.length, ts.length);
      for (var i = 0; i < n; i++){
        var row = [ti, name, i, xs[i] ?? '', ys[i] ?? ''];
        if (opts.include_text) row.push(ts[i] ?? '');
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

  function decoratePlot(gd) {
    if (!gd || gd._tracesToCSV || !window.Plotly) return;
    
    var opts = {
      visible_only: true,
      include_text: true,
      filename: '%s'
    };

    gd._tracesToCSV = function(gd) { return tracesToCSV(gd, opts); };
    gd._downloadText = downloadText;
    gd._csvFilename = opts.filename;

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
    js_str(filename)
  )

  htmltools::tags$script(htmltools::HTML(script))
}
