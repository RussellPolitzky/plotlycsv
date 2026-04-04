function(el, x) {
  var gd = el.querySelector('.js-plotly-plot');
  if (!gd) {
    if (el.classList && el.classList.contains('js-plotly-plot')) gd = el;
  }
  if (!gd || !window.Plotly) return;

  var sep              = '{{SEP}}';
  var visible_only     = {{VISIBLE_ONLY}};
  var include_z        = {{INCLUDE_Z}};
  var include_text     = {{INCLUDE_TEXT}};
  var include_customdata = {{INCLUDE_CUSTOMDATA}};
  var selected_only    = {{SELECTED_ONLY}};

  function csvEscape(v) {
    if (v === null || v === undefined) return '';
    var s = String(v);
    if (/[",\n\r]/.test(s) || s.indexOf(sep) !== -1) {
      return '"' + s.replace(/"/g, '""') + '"';
    }
    return s;
  }

  function toRow(arr) { return arr.map(csvEscape).join(sep); }

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

  function tracesToCSV(gd) {
    var xLabel = axisLabel(gd, 'xaxis', 'x');
    var yLabel = axisLabel(gd, 'yaxis', 'y');
    var zLabel = axisLabel(gd, 'zaxis', 'z');
    var header = ['trace_index', 'trace_name', 'point_index', xLabel, yLabel];
    if (include_z)           header.push(zLabel);
    if (include_text)        header.push('text');
    if (include_customdata)  header.push('customdata');
    var lines = [toRow(header)];

    (gd.data || []).forEach(function(tr, ti) {
      if (visible_only && !traceIsVisible(tr)) return;

      var name = tr.name || '';
      var xs = Array.isArray(tr.x) ? tr.x : [];
      var ys = Array.isArray(tr.y) ? tr.y : [];
      var zs = Array.isArray(tr.z) ? tr.z : [];
      var ts = Array.isArray(tr.text) ? tr.text : [];
      var cd = Array.isArray(tr.customdata) ? tr.customdata : [];

      var selected     = tr.selectedpoints;
      var hasSelection = Array.isArray(selected) && selected.length > 0;

      var lengths = [xs.length, ys.length];
      if (include_z)           lengths.push(zs.length);
      if (include_text)        lengths.push(ts.length);
      if (include_customdata)  lengths.push(cd.length);
      var n = Math.max.apply(null, lengths.length ? lengths : [0]);

      for (var i = 0; i < n; i++) {
        if (selected_only && hasSelection && !selected.includes(i)) continue;
        var row = [
          ti, name, i,
          xs[i] !== undefined && xs[i] !== null ? xs[i] : '',
          ys[i] !== undefined && ys[i] !== null ? ys[i] : ''
        ];
        if (include_z)
          row.push(zs[i] !== undefined && zs[i] !== null ? zs[i] : '');
        if (include_text)
          row.push(ts[i] !== undefined && ts[i] !== null ? ts[i] : '');
        if (include_customdata)
          row.push(cd[i] !== undefined && cd[i] !== null ?
            (Array.isArray(cd[i]) ? cd[i].join('|') : String(cd[i])) : '');
        lines.push(toRow(row));
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

  gd._tracesToCSV     = tracesToCSV;
  gd._downloadText    = downloadText;
  gd._copyToClipboard = copyToClipboard;
  gd._csvFilename     = '{{FILENAME}}';
}
