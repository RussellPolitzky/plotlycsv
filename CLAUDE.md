# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

`plotlycsv` is an R package that adds a "Download CSV" button to Plotly modebar buttons in static HTML outputs (Quarto / R Markdown). It works without Shiny — the CSV export is done entirely in the browser via injected JavaScript.

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
1. Registers a custom modebar button in `widget$x$config$modeBarButtonsToAdd`.
2. Injects a JavaScript `onRender` hook via `htmlwidgets::onRender()` that attaches `_tracesToCSV`, `_downloadText`, and `_csvFilename` directly onto the Plotly DOM element (`gd`).

The JavaScript is built with `sprintf()` so that R-side parameters (`sep`, `selected_only`, `visible_only`, `include_z`, `include_text`, `include_customdata`) are baked into the generated JS string at call time.

### `use_plotly_csv()` — global document decorator (`R/use_plotly_csv.R`)
Returns an `htmltools::tags$script(...)` tag meant to be called once in a setup chunk. The script:
1. Wraps `Plotly.newPlot` and `Plotly.react` to intercept every future render.
2. Uses a `MutationObserver` to catch dynamically added plots.
3. Falls back to a polling interval (every 500 ms for 10 s) to handle late-loaded Plotly.
4. Determines the CSV filename by walking the DOM up from `gd.parentElement`, preferring `fig-*` IDs (Quarto figure labels) over other `div` IDs (chunk names), then falling back to `"plotly-data.csv"`.

### Key design constraint
Both functions produce **static HTML** output — there is no server-side component. The entire CSV generation happens in the browser from `gd.data` (the Plotly trace data already embedded in the widget).

## Test Structure

- `tests/testthat/test-export_csv.R` — unit tests for `add_plotly_export_csv()` including an optional Chromote browser test.
- `tests/testthat/test-global_decorator.R` — unit tests for `use_plotly_csv()` (checks JS content).
- `tests/testthat/test-enhancements.R` — tests for `selected_only`, custom `sep`, and the new parameters on `use_plotly_csv()`.
- `tests/testthat/test-quarto.R` — full integration test: renders a `.qmd` file with Quarto CLI and uses Chromote to verify both decorators work in a real browser.

Chromote / Quarto integration tests are skipped with `skip_on_cran()` and `skip_on_ci()` and require `chromote`, `quarto`, and a Quarto CLI installation.
