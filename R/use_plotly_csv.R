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
  var iconPath = 'M150 0h700v100H150z M425 800h150v-400h175L500 150L250 400h175z';

  var global_sep = '%s';

  function csvEscape(v, s_sep){
    var delim = s_sep || ',';
    if (v === null || v === undefined) return '';
    var s = String(v);
    if (/[\\\\\\\",\\\\n\\\\r]/.test(s) || s.indexOf(delim) !== -1) return '\\\"' + s.replace(/\\\"/g, '\\\"\\\"') + '\\\"';
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

        var xi = xs[i] !== undefined && xs[i] !== null ? xs[i] : '';
        var yi = ys[i] !== undefined && ys[i] !== null ? ys[i] : '';
        var row = [ti, name, i, xi, yi];
        if (opts.include_text) {
          var t_val = ts[i] !== undefined && ts[i] !== null ? ts[i] : '';
          row.push(t_val);
        }
        lines.push(toRow(row, sep));
      }
    });
    return lines.join('\\\\n');
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

  var csvButton = {
    name: 'Download CSV Auto',
    title: 'Download data as CSV',
    icon: { width: 1000, ascent: 850, descent: -150, path: iconPath },
    click: function(gd) { if (gd._downloadText) gd._downloadText(gd._csvFilename, gd._tracesToCSV(gd)); }
  };

  function hasCsvButton(buttons) {
    return (buttons || []).some(function(b) {
      if (!b) return false;
      var name = (typeof b === 'string' ? b : (b.name || '')).toString().toUpperCase();
      var title = (typeof b === 'object' ? (b.title || '') : '').toString().toUpperCase();
      return name.indexOf('CSV') !== -1 || title.indexOf('CSV') !== -1;
    });
  }

  // Inject our button into the config/figure BEFORE Plotly renders the plot.
  // This is the only reliable way — modifying config after render (via Plotly.react)
  // does not trigger a modebar rebuild because Plotly diffs against the same object.
  function injectButton(args) {
    if (!args || args.length < 2) return;
    var dataArg = args[1];
    if (Array.isArray(dataArg)) {
      // Separate-args form: newPlot(gd, data, layout, config)
      var cfg = args[3];
      if (!cfg) { cfg = {}; args[3] = cfg; }
      if (!hasCsvButton(cfg.modeBarButtonsToAdd)) {
        cfg.modeBarButtonsToAdd = (cfg.modeBarButtonsToAdd || []).concat([csvButton]);
      }
    } else if (dataArg && typeof dataArg === 'object') {
      // Figure-object form: newPlot(gd, figure)
      if (!dataArg.config) dataArg.config = {};
      if (!hasCsvButton(dataArg.config.modeBarButtonsToAdd)) {
        dataArg.config.modeBarButtonsToAdd = (dataArg.config.modeBarButtonsToAdd || []).concat([csvButton]);
      }
    }
  }

  // After a plot is rendered, attach the helper functions to gd.
  function setupHelpers(gd) {
    if (!gd || gd._tracesToCSV) return;
    if (!gd.classList || !gd.classList.contains('js-plotly-plot')) return;
    if (!gd.data) return;

    var opts = {
      visible_only: true,
      include_text: true,
      selected_only: %s,
      sep: global_sep
    };

    gd._tracesToCSV  = function(gd) { return tracesToCSV(gd, opts); };
    gd._downloadText = downloadText;
    gd._csvFilename  = chunkFilename(gd);
  }

  function hookPlotly() {
    if (!window.Plotly || window.Plotly._csvHooked) return;
    window.Plotly._csvHooked = true;

    var wrap = function(orig) {
      if (typeof orig !== 'function') return orig;
      return function() {
        var args = Array.prototype.slice.call(arguments);
        var gd   = args[0];
        injectButton(args);
        var res = orig.apply(this, args);
        if (res && typeof res.then === 'function') {
          res.then(function(new_gd) { setupHelpers(new_gd || gd); });
        } else {
          setupHelpers(gd);
        }
        return res;
      };
    };

    window.Plotly.newPlot = wrap(window.Plotly.newPlot);
    window.Plotly.react  = wrap(window.Plotly.react);
  }

  function init() {
    hookPlotly();
    document.querySelectorAll('.js-plotly-plot').forEach(setupHelpers);
    var observer = new MutationObserver(function(mutations) {
      hookPlotly();
      mutations.forEach(function(mutation) {
        mutation.addedNodes.forEach(function(node) {
          if (node.nodeType === 1) {
            if (node.classList && node.classList.contains('js-plotly-plot')) setupHelpers(node);
            node.querySelectorAll('.js-plotly-plot').forEach(setupHelpers);
          }
        });
        if (mutation.target && mutation.target.nodeType === 1 && mutation.target.classList.contains('js-plotly-plot')) {
          setupHelpers(mutation.target);
        }
      });
    });
    observer.observe(document.body, { childList: true, subtree: true });
  }

  // Hook immediately - Plotly.js is already loaded if this script appears after it in the document
  hookPlotly();

  if (document.readyState === 'complete' || document.readyState === 'interactive') init();
  else document.addEventListener('DOMContentLoaded', init);

  // Safety: check periodically for first 10s if Plotly is loaded late
  var checkCount = 0;
  var interval = setInterval(function() {
    hookPlotly();
    if (window.Plotly) {
        document.querySelectorAll('.js-plotly-plot').forEach(setupHelpers);
    }
    if (++checkCount > 20) clearInterval(interval);
  }, 500);
})();",
    js_str(sep),
    if (is.null(filename)) "null" else paste0("'", js_str(filename), "'"),
    if (selected_only) "true" else "false"
  )

  htmltools::tags$script(htmltools::HTML(script))
}
