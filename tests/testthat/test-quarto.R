test_that("plotlycsv works in a rendered Quarto document", {
  skip_if_not_installed("quarto")
  skip_if_not_installed("chromote")
  skip_if_not_installed("plotly")
  skip_if_not_installed("ggplot2")
  skip_on_cran()
  
  # Check if quarto is actually available in the system path
  quarto_path <- Sys.which("quarto")
  if (quarto_path == "") skip("Quarto CLI not found")

  # Create a temp directory for the Quarto project
  tmp_dir <- tempfile("quarto_test")
  dir.create(tmp_dir, showWarnings = FALSE)
  qmd_file <- file.path(tmp_dir, "test.qmd")
  
  # We'll use pkgload::load_all to load the local package in the Quarto session
  # We need to find the project root relative to this test file
  pkg_path <- normalizePath("../..", winslash = "/")
  
  qmd_content <- c(
    "---",
    "title: \"Test Plotly CSV\"",
    "format:",
    "  html:",
    "    embed-resources: true",
    "---",
    "",
    "```{r setup, message=FALSE, echo=FALSE}",
    paste0("pkgload::load_all(", shQuote(pkg_path), ")"),
    "library(plotly)",
    "use_plotly_csv(filename = \"global_export.csv\")",
    "```",
    "",
    "## Global Plot",
    "```{r global_plot}",
    "plot_ly(mtcars, x = ~wt, y = ~mpg, type = \"scatter\", mode = \"markers\")",
    "```",
    "",
    "## Manual Plot",
    "```{r manual_plot}",
    "plot_ly(mtcars, x = ~hp, y = ~qsec, type = \"scatter\", mode = \"markers\") |> ",
    "  add_plotly_export_csv(filename = \"manual_export.csv\")",
    "```"
  )
  
  writeLines(qmd_content, qmd_file)
  
  # Render the Quarto document using system2 for better error visibility
  render_out <- system2(
    quarto_path, 
    args = c("render", shQuote(normalizePath(qmd_file))), 
    stdout = TRUE, 
    stderr = TRUE
  )
  
  message("Quarto render output:\n", paste(render_out, collapse = "\n"))
  
  output_html <- file.path(tmp_dir, "test.html")
  expect_true(file.exists(output_html), info = "Quarto render should produce test.html")
  
  html_lines <- readLines(output_html, warn = FALSE)
  html_content <- paste(html_lines, collapse = "\n")
  
  # Check for plotlycsv strings in HTML
  expect_match(html_content, "global_export.csv", fixed = TRUE, info = "HTML should contain global filename")
  expect_match(html_content, "manual_export.csv", fixed = TRUE, info = "HTML should contain manual filename")
  expect_match(html_content, "plotly-export-button", fixed = TRUE, info = "HTML should contain the export button class")

  # Verify with Chromote
  b <- chromote::ChromoteSession$new()
  on.exit({
    try(b$close(), silent = TRUE)
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)
  
  # Navigate to the file and wait for load
  b$Page$navigate(paste0("file://", normalizePath(output_html, winslash = "/")))
  b$Page$loadEventFired()
  
  # Poll until both plots are ready and have _tracesToCSV
  success <- FALSE
  last_res <- NULL
  for (i in seq_len(100)) {
    Sys.sleep(0.2)
    ev_res <- b$Runtime$evaluate("
      (function() {
        const plots = document.querySelectorAll('.plotly, .js-plotly-plot');
        const status = Array.from(plots).map(p => ({
          hasTraces: !!p._tracesToCSV,
          filename: p._csvFilename || ''
        }));
        return JSON.stringify({ count: plots.length, status: status });
      })()
    ", returnByValue = TRUE)
    
    if (is.null(ev_res$exceptionDetails)) {
       res <- jsonlite::fromJSON(ev_res$result$value)
       last_res <- res
       if (!is.null(res$count) && sum(res$status$hasTraces) >= 2) {
         success <- TRUE
         break
       }
    }
  }
  
  expect_true(success, info = "At least 2 plots should have _tracesToCSV attached in the rendered HTML")
  
  filenames <- if (!is.null(last_res$status)) {
    if (is.data.frame(last_res$status)) last_res$status$filename else sapply(last_res$status, function(s) s$filename)
  } else {
    character(0)
  }
  
  expect_true("global_export.csv" %in% filenames)
  expect_true("manual_export.csv" %in% filenames)
})
