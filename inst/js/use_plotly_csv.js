(function() {
  var iconPath = 'M150 0h700v100H150z M425 800h150v-400h175L500 150L250 400h175z';
  var clipPath = 'M350 0h300v200h200v800H150V200h200z';

  var global_sep              = '{{GLOBAL_SEP}}';
  var global_filename         = {{GLOBAL_FILENAME}};
  var global_clipboard        = {{GLOBAL_CLIPBOARD}};
  var global_selected_only    = {{GLOBAL_SELECTED_ONLY}};
  var global_visible_only     = {{GLOBAL_VISIBLE_ONLY}};
  var global_include_text     = {{GLOBAL_INCLUDE_TEXT}};
  var global_include_z        = {{GLOBAL_INCLUDE_Z}};
  var global_include_customdata = {{GLOBAL_INCLUDE_CUSTOMDATA}};


  function csvEscape(v, s_sep) {
    var delim = s_sep || ',';
    if (v === null || v === undefined) return '';
    var s = String(v);
    if (/[",\n\r]/.test(s) || s.indexOf(delim) !== -1) return '"' + s.replace(/"/g, '""') + '"';
    return s;
  }

  function toRow(arr, s_sep) {
    return arr.map(function(x) { return csvEscape(x, s_sep); }).join(s_sep || ',');
  }

  function traceIsVisible(tr) {
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

  function tracesToCSV(gd, opts) {
    var sep    = opts.sep || ',';
    var xLabel = axisLabel(gd, 'xaxis', 'x');
    var yLabel = axisLabel(gd, 'yaxis', 'y');
    var zLabel = axisLabel(gd, 'zaxis', 'z');

    var header = ['trace_index', 'trace_name', 'point_index', xLabel, yLabel];
    if (opts.include_z)           header.push(zLabel);
    if (opts.include_text)        header.push('text');
    if (opts.include_customdata)  header.push('customdata');
    var lines = [toRow(header, sep)];

    (gd.data || []).forEach(function(tr, ti) {
      if (opts.visible_only && !traceIsVisible(tr)) return;
      var name = tr.name || '';
      var xs = Array.isArray(tr.x) ? tr.x : [];
      var ys = Array.isArray(tr.y) ? tr.y : [];
      var zs = Array.isArray(tr.z) ? tr.z : [];
      var ts = Array.isArray(tr.text) ? tr.text : [];
      var cd = Array.isArray(tr.customdata) ? tr.customdata : [];

      var selected     = tr.selectedpoints;
      var hasSelection = Array.isArray(selected) && selected.length > 0;

      var lengths = [xs.length, ys.length];
      if (opts.include_z)           lengths.push(zs.length);
      if (opts.include_text)        lengths.push(ts.length);
      if (opts.include_customdata)  lengths.push(cd.length);
      var n = Math.max.apply(null, lengths.length ? lengths : [0]);

      for (var i = 0; i < n; i++) {
        if (opts.selected_only && hasSelection && !selected.includes(i)) continue;
        var row = [
          ti, name, i,
          xs[i] !== undefined && xs[i] !== null ? xs[i] : '',
          ys[i] !== undefined && ys[i] !== null ? ys[i] : ''
        ];
        if (opts.include_z)
          row.push(zs[i] !== undefined && zs[i] !== null ? zs[i] : '');
        if (opts.include_text)
          row.push(ts[i] !== undefined && ts[i] !== null ? ts[i] : '');
        if (opts.include_customdata)
          row.push(cd[i] !== undefined && cd[i] !== null ?
            (Array.isArray(cd[i]) ? cd[i].join('|') : String(cd[i])) : '');
        lines.push(toRow(row, sep));
      }
    });
    return lines.join('\n');
  }

  function downloadText(filename, text) {
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

  function copyToClipboard(text) {
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

  function chunkFilename(gd) {
    if (global_filename !== null) return global_filename;
    var el = gd.parentElement;
    var fallbackId = null;
    while (el) {
      if (el.tagName === 'DIV' && el.id && el.id.length > 0) {
        if (el.id.indexOf('fig-') === 0) return el.id + '.csv';
        if (!fallbackId) fallbackId = el.id;
      }
      el = el.parentElement;
    }
    if (fallbackId) return fallbackId + '.csv';
    return 'plotly-data.csv';
  }

  var csvButton = {
    name:  'Download CSV Auto',
    title: 'Download data as CSV',
    icon:  { width: 1000, ascent: 850, descent: -150, path: iconPath },
    click: function(gd) { if (gd._downloadText) gd._downloadText(gd._csvFilename, gd._tracesToCSV(gd)); }
  };

  var clipButton = {
    name:  'Copy CSV Auto',
    title: 'Copy data as CSV to clipboard',
    icon:  { width: 1000, ascent: 850, descent: -150, path: clipPath },
    click: function(gd) { if (gd._copyToClipboard) gd._copyToClipboard(gd._tracesToCSV(gd)); }
  };

  function hasCsvButton(buttons) {
    return (buttons || []).some(function(b) {
      if (!b) return false;
      var name  = (typeof b === 'string' ? b : (b.name  || '')).toString().toUpperCase();
      var title = (typeof b === 'object' ? (b.title || '') : '').toString().toUpperCase();
      return name.indexOf('CSV') !== -1 || title.indexOf('CSV') !== -1;
    });
  }

  function hasClipButton(buttons) {
    return (buttons || []).some(function(b) {
      if (!b) return false;
      var name = (typeof b === 'string' ? b : (b.name || '')).toString().toUpperCase();
      return name.indexOf('COPY') !== -1 || name.indexOf('CLIP') !== -1;
    });
  }

  // Inject our button(s) into the config/figure BEFORE Plotly renders the plot.
  // This is the only reliable way — modifying config after render (via Plotly.react)
  // does not trigger a modebar rebuild because Plotly diffs against the same object.
  function injectButton(args) {
    if (!args || args.length < 2) return;
    var dataArg = args[1];
    var cfg;
    if (Array.isArray(dataArg)) {
      // Separate-args form: newPlot(gd, data, layout, config)
      cfg = args[3];
      if (!cfg) { cfg = {}; args[3] = cfg; }
    } else if (dataArg && typeof dataArg === 'object') {
      // Figure-object form: newPlot(gd, figure)
      if (!dataArg.config) dataArg.config = {};
      cfg = dataArg.config;
    } else {
      return;
    }
    var btns  = cfg.modeBarButtonsToAdd || [];
    var toAdd = [];
    if (!hasCsvButton(btns))                       toAdd.push(csvButton);
    if (global_clipboard && !hasClipButton(btns))  toAdd.push(clipButton);
    if (toAdd.length) cfg.modeBarButtonsToAdd = btns.concat(toAdd);
  }

  // After a plot is rendered, attach the helper functions to gd.
  function setupHelpers(gd) {
    if (!gd || gd._tracesToCSV) return;
    if (!gd.classList || !gd.classList.contains('js-plotly-plot')) return;
    if (!gd.data) return;

    var opts = {
      visible_only:       global_visible_only,
      include_text:       global_include_text,
      include_z:          global_include_z,
      include_customdata: global_include_customdata,
      selected_only:      global_selected_only,
      sep:                global_sep
    };

    gd._tracesToCSV     = function(gd) { return tracesToCSV(gd, opts); };
    gd._downloadText    = downloadText;
    gd._copyToClipboard = copyToClipboard;
    gd._csvFilename     = chunkFilename(gd);
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
    window.Plotly.react   = wrap(window.Plotly.react);
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

  // Hook immediately — Plotly.js is already loaded when this script appears after it.
  hookPlotly();

  if (document.readyState === 'complete' || document.readyState === 'interactive') init();
  else document.addEventListener('DOMContentLoaded', init);

  // Safety: poll for 10s in case Plotly loads late.
  var checkCount = 0;
  var interval = setInterval(function() {
    hookPlotly();
    if (window.Plotly) document.querySelectorAll('.js-plotly-plot').forEach(setupHelpers);
    if (++checkCount > 20) clearInterval(interval);
  }, 500);
})();
