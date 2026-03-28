# Integration tests that render the Quarto document
# tests/quarto/plotlycsv_integration_test.qmd and inspect the HTML output.
#
# Two phases are covered:
#   1. Static HTML checks — sentinel comments and filename strings are present
#      in the rendered HTML (no browser required).
#   2. Browser checks (via chromote) — the JavaScript decorators are actually
#      attached to the Plotly widgets at runtime.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Locate the Quarto CLI.
quarto_bin <- function() {
  path <- Sys.which("quarto")
  if (nzchar(path)) return(path)
  NULL
}

# Render the .qmd file.  Returns the path to the output HTML on success, or
# fails / skips with an informative message on failure.
# Strategy: copy the .qmd into output_dir and render it in place, so Quarto
# deposits the HTML next to the source (its default behaviour).
render_qmd <- function(qmd_file, pkg_path, output_dir) {
  q <- quarto_bin()
  if (is.null(q)) testthat::skip("Quarto CLI not found in PATH")

  # Copy the source document into the temp directory
  qmd_copy <- file.path(output_dir, "plotlycsv_integration_test.qmd")
  file.copy(from = qmd_file, to = qmd_copy, overwrite = TRUE)

  # Pass the package path via an env var so the Quarto session can load it.
  # system2()'s env= parameter does not work reliably on Windows; use
  # Sys.setenv/Sys.unsetenv instead.
  old_val <- Sys.getenv("PLOTLYCSV_PKG_PATH", unset = NA_character_)
  Sys.setenv(PLOTLYCSV_PKG_PATH = normalizePath(pkg_path, winslash = "/"))
  on.exit({
    if (is.na(old_val)) {
      Sys.unsetenv("PLOTLYCSV_PKG_PATH")
    } else {
      Sys.setenv(PLOTLYCSV_PKG_PATH = old_val)
    }
  }, add = TRUE)

  stdout_stderr <- system2(
    q,
    args   = c("render", shQuote(normalizePath(qmd_copy, winslash = "/"))),
    stdout = TRUE,
    stderr = TRUE
  )

  message("Quarto render log:\n", paste(stdout_stderr, collapse = "\n"))

  out_html <- file.path(output_dir, "plotlycsv_integration_test.html")
  if (!file.exists(out_html)) {
    testthat::fail(paste(
      "Quarto render did not produce:", out_html,
      "\nRender output:\n", paste(stdout_stderr, collapse = "\n")
    ))
  }
  out_html
}


# ---------------------------------------------------------------------------
# Test 1: static HTML checks (no browser needed)
# ---------------------------------------------------------------------------
test_that("Quarto document renders and contains all decorator markers", {

  skip_if_not_installed("quarto")
  skip_if_not_installed("plotly")
  skip_if_not_installed("ggplot2")
  skip_on_cran()

  if (is.null(quarto_bin())) skip("Quarto CLI not found in PATH")

  pkg_path <- normalizePath(file.path(test_path(), "..", ".."), winslash = "/")
  qmd_file <- normalizePath(
    file.path(test_path(), "..", "quarto", "plotlycsv_integration_test.qmd"),
    winslash = "/"
  )

  out_dir  <- tempfile("qtest_static_")
  dir.create(out_dir, showWarnings = FALSE)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  out_html <- render_qmd(qmd_file, pkg_path, out_dir)

  html <- paste(readLines(out_html, warn = FALSE), collapse = "\n")

  # ---- Sentinel markers written by the verification chunk ----
  expect_match(
    html, "PLOTLYCSV_GLOBAL_OK",
    fixed = TRUE,
    label = "HTML must contain the global-decorator sentinel"
  )
  expect_match(
    html, "PLOTLYCSV_PER_PLOT_OK",
    fixed = TRUE,
    label = "HTML must contain the first per-plot sentinel"
  )
  expect_match(
    html, "PLOTLYCSV_PER_PLOT_2_OK",
    fixed = TRUE,
    label = "HTML must contain the second per-plot sentinel"
  )

  # ---- Filenames injected by the two decorator types ----
  expect_match(
    html, "global_export.csv",
    fixed = TRUE,
    label = "HTML must contain the global decorator filename"
  )
  expect_match(
    html, "per_plot_hp_qsec.csv",
    fixed = TRUE,
    label = "HTML must contain the first per-plot filename"
  )
  expect_match(
    html, "per_plot_disp_drat.csv",
    fixed = TRUE,
    label = "HTML must contain the second per-plot filename"
  )

  # ---- Core JS patterns from use_plotly_csv() (global decorator) ----
  expect_match(
    html, "MutationObserver",
    fixed = TRUE,
    label = "Global decorator JS must contain MutationObserver"
  )
  expect_match(
    html, "decoratePlot",
    fixed = TRUE,
    label = "Global decorator JS must contain decoratePlot"
  )

  # ---- Core JS patterns from add_plotly_export_csv() (per-plot decorator) ----
  expect_match(
    html, "tracesToCSV",
    fixed = TRUE,
    label = "Per-plot decorator JS must contain tracesToCSV"
  )
  expect_match(
    html, "downloadText",
    fixed = TRUE,
    label = "Per-plot decorator JS must contain downloadText"
  )
})


# ---------------------------------------------------------------------------
# Test 2: browser checks — JS decorators actually attached at runtime
# ---------------------------------------------------------------------------
test_that("Quarto-rendered plots have _tracesToCSV attached in the browser", {

  skip_if_not_installed("quarto")
  skip_if_not_installed("chromote")
  skip_if_not_installed("plotly")
  skip_if_not_installed("ggplot2")
  skip_on_cran()

  if (is.null(quarto_bin())) skip("Quarto CLI not found in PATH")

  pkg_path <- normalizePath(file.path(test_path(), "..", ".."), winslash = "/")
  qmd_file <- normalizePath(
    file.path(test_path(), "..", "quarto", "plotlycsv_integration_test.qmd"),
    winslash = "/"
  )

  out_dir  <- tempfile("qtest_browser_")
  dir.create(out_dir, showWarnings = FALSE)

  out_html <- render_qmd(qmd_file, pkg_path, out_dir)

  b <- chromote::ChromoteSession$new()
  on.exit({
    try(b$close(),    silent = TRUE)
    unlink(out_dir, recursive = TRUE)
  }, add = TRUE)

  file_url <- paste0("file:///", normalizePath(out_html, winslash = "/"))
  b$Page$navigate(file_url)
  b$Page$loadEventFired()

  # Poll until the per-plot-decorated plots AND at least one global-decorated
  # plot have _tracesToCSV attached (or time out after 30 s).
  # Note: global-decorator plots use a MutationObserver / load event so they
  # may initialise slightly later than per-plot plots whose onRender hook fires
  # synchronously.  We therefore poll for the weaker condition:
  #   • both per-plot filenames present, AND
  #   • at least one plot carries the global filename.
  poll_result <- NULL
  success     <- FALSE

  for (i in seq_len(150)) {         # up to 30 seconds (150 × 0.2 s)
    Sys.sleep(0.2)

    ev <- tryCatch(
      b$Runtime$evaluate(
        "(function() {
           var plots  = Array.from(document.querySelectorAll('.js-plotly-plot'));
           var status = plots.map(function(p) {
             return {
               hasTraces : !!p._tracesToCSV,
               filename  : p._csvFilename || ''
             };
           });
           return JSON.stringify({ count: plots.length, status: status });
         })()",
        returnByValue = TRUE
      ),
      error = function(e) NULL
    )

    if (!is.null(ev) && is.null(ev$exceptionDetails)) {
      res <- jsonlite::fromJSON(ev$result$value)
      poll_result <- res

      fnames <- if (is.data.frame(res$status)) {
        res$status$filename
      } else if (is.list(res$status) && length(res$status) > 0) {
        vapply(res$status, `[[`, character(1), "filename")
      } else {
        character(0)
      }

      if ("per_plot_hp_qsec.csv"    %in% fnames &&
          "per_plot_disp_drat.csv"  %in% fnames &&
          "global_export.csv"       %in% fnames) {
        success <- TRUE
        break
      }
    }
  }

  expect_true(
    success,
    label = paste(
      "Per-plot and global-decorator plots should all have _tracesToCSV.",
      "Last poll result:", jsonlite::toJSON(poll_result, auto_unbox = TRUE)
    )
  )

  # -------------------------------------------------------------------
  # Check that global-decorator plots carry the global filename and
  # per-plot-decorator plots carry their individual filenames.
  # -------------------------------------------------------------------
  filenames <- if (!is.null(poll_result$status)) {
    if (is.data.frame(poll_result$status)) {
      poll_result$status$filename
    } else {
      vapply(poll_result$status, `[[`, character(1), "filename")
    }
  } else {
    character(0)
  }

  expect_true(
    "global_export.csv" %in% filenames,
    label = "At least one plot should carry the global filename"
  )
  expect_true(
    "per_plot_hp_qsec.csv" %in% filenames,
    label = "Per-plot decorator plot 1 should carry its custom filename"
  )
  expect_true(
    "per_plot_disp_drat.csv" %in% filenames,
    label = "Per-plot decorator plot 2 should carry its custom filename"
  )

  # -------------------------------------------------------------------
  # Verify per-plot-decorator plots: call _tracesToCSV() and parse CSV
  # -------------------------------------------------------------------
  csv_result <- tryCatch(
    b$Runtime$evaluate(
      "(function() {
         var plots = Array.from(document.querySelectorAll('.js-plotly-plot'));
         // Find a plot with the per-plot filename
         var target = plots.find(function(p) {
           return p._csvFilename === 'per_plot_hp_qsec.csv';
         });
         if (!target)            return 'NO_TARGET';
         if (!target._tracesToCSV) return 'NO_FUNC';
         return target._tracesToCSV(target);
       })()",
      returnByValue = TRUE
    ),
    error = function(e) NULL
  )

  expect_false(
    is.null(csv_result),
    label = "CSV evaluation should not throw"
  )
  csv_text <- csv_result$result$value
  expect_type(csv_text, "character")
  expect_false(
    csv_text %in% c("NO_TARGET", "NO_FUNC"),
    label = paste("Expected CSV data, got:", csv_text)
  )

  # Parse and validate the CSV data
  df <- read.csv(text = csv_text, stringsAsFactors = FALSE)
  expect_true(
    nrow(df) >= nrow(mtcars),
    label = paste(
      "Exported CSV should have at least", nrow(mtcars),
      "rows (one per mtcars observation); got", nrow(df)
    )
  )
  expect_true(
    "x" %in% names(df),
    label = "CSV must have an 'x' column"
  )
  expect_true(
    "y" %in% names(df),
    label = "CSV must have a 'y' column"
  )
})
