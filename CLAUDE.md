# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`plotlycsv` is an R package that adds "Download CSV" and "Copy to Clipboard" buttons to Plotly modebar buttons in static HTML outputs (Quarto / R Markdown). It works without Shiny — the CSV export is done entirely in the browser via injected JavaScript.

## Commands

### Install / Load
```r
# Install from local source
install.packages("plotlycsv", repos = NULL, type = "source")
# or
remotes::install_local("plotlycsv")

# Load during development without reinstalling
pkgload::load_all()
```

### Run Tests
```r
# All tests
devtools::test()

# Single test file
testthat::test_file("tests/testthat/test-export_csv.R")
```

### Document & Build
```r
devtools::document()   # regenerate NAMESPACE and Rd files from roxygen
devtools::check()      # R CMD CHECK
```

## Architecture

The package has two exported functions, each covering a different usage pattern:

### `add_plotly_export_csv()` — per-widget decorator (`R/add_plotly_export_csv.R`)
Wraps a single plotly `htmlwidget`. It:
1. Registers modebar buttons in `widget$x$config$modeBarButtonsToAdd`: a "Download CSV" button and (when `clipboard = TRUE`) a "Copy CSV" button.
2. Injects a JavaScript `onRender` hook via `htmlwidgets::onRender()` that attaches `_tracesToCSV`, `_downloadText`, `_copyToClipboard`, and `_csvFilename` directly onto the Plotly DOM element (`gd`).

The JavaScript is built with `sprintf()` so that R-side parameters (`sep`, `selected_only`, `visible_only`, `include_z`, `include_text`, `include_customdata`) are baked into the generated JS string at call time. Column headers in the CSV are derived at export time from `gd.layout.xaxis.title` / `yaxis.title` / `zaxis.title` via `axisLabel()`, falling back to `"x"` / `"y"` / `"z"`.

### `use_plotly_csv()` — global document decorator (`R/use_plotly_csv.R`)
Returns an `htmltools::tags$script(...)` tag meant to be called once in a setup chunk. The script:
1. Calls `hookPlotly()` **immediately** on script load (before `DOMContentLoaded`) to wrap `Plotly.newPlot` and `Plotly.react`.
2. The hook calls `injectButton()` **before** calling the original Plotly function, adding the CSV (and optionally clipboard) buttons to `modeBarButtonsToAdd` in the figure config. This pre-render injection is required — post-render `Plotly.react` does not rebuild the modebar because Plotly diffs against the same config object reference.
3. After each render, `setupHelpers()` attaches `_tracesToCSV`, `_downloadText`, `_copyToClipboard`, and `_csvFilename` to `gd`.
4. Uses a `MutationObserver` and a polling interval (every 500 ms for 10 s) as fallbacks for late-loaded Plotly.
5. Determines the CSV filename by walking the DOM up from `gd.parentElement`, preferring `fig-*` IDs (Quarto figure labels) over other `div` IDs (chunk names), then falling back to `"plotly-data.csv"`.

The JS template exceeds R's `sprintf()` 8192-character limit, so the R-side parameters are substituted into a small `globals_js` string via `sprintf()`, then the full script is assembled with `paste0()`.

### Key design constraint
Both functions produce **static HTML** output — there is no server-side component. The entire CSV generation happens in the browser from `gd.data` (the Plotly trace data already embedded in the widget).

## Test Structure

- `tests/testthat/test-export_csv.R` — unit tests for `add_plotly_export_csv()` including an optional Chromote browser test.
- `tests/testthat/test-global_decorator.R` — unit tests for `use_plotly_csv()` including regression tests that guard the pre-render injection approach (verifies `injectButton` precedes `orig.apply`, `hookPlotly` is called immediately, and `setupHelpers` does not call `Plotly.react`).
- `tests/testthat/test-enhancements.R` — tests for all parameters: `selected_only`, `sep`, `visible_only`, `include_z`, `include_customdata`, `clipboard`; axis label usage; and clipboard button presence/absence on both decorators.
- `tests/testthat/test-quarto.R` — full integration test: renders a `.qmd` file with Quarto CLI and uses Chromote to verify both decorators work in a real browser.
- `tests/testthat/test-quarto_doc.R` — renders a comprehensive integration `.qmd` and inspects the HTML for key JS patterns and browser-side CSV export correctness.

Chromote / Quarto integration tests are skipped with `skip_on_cran()` and `skip_on_ci()` and require `chromote`, `quarto`, and a Quarto CLI installation.
