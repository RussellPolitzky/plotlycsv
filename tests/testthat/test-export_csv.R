test_that("add_plotly_export_csv() decorates and configures widget", {

    skip_if_not_installed("ggplot2")
    skip_if_not_installed("plotly")

    library(ggplot2)
    library(plotly)

    p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
    p_plotly <- suppressWarnings(ggplotly(p))

    # Apply decorator
    decorated <- suppressWarnings(add_plotly_export_csv(p_plotly))

    # Check classes
    expect_s3_class(decorated, "htmlwidget")
    expect_s3_class(decorated, "plotly")

    # Check that jsHooks is populated
    expect_true(!is.null(decorated$jsHooks$render))

    # Check that the modebar button is registered in the config
    config <- decorated$x$config
    expect_true(!is.null(config$modeBarButtonsToAdd))
    
    button_names <- vapply(config$modeBarButtonsToAdd, function(b) {
      if (is.list(b) && !is.null(b$name)) b$name else ""
    }, character(1))
    expect_true("Download CSV" %in% button_names)


    # Check that injected JS contains our function signature or tracesToCSV
    hook_code <- decorated$jsHooks$render[[1]]$code
    expect_match(hook_code, "tracesToCSV")
    expect_match(hook_code, "downloadText")
})


test_that("CSV export correctly extracts data in the browser", {

    # This tests the JS payload using Chromote
    skip_on_cran()
    skip_on_ci()
    skip_if_not_installed("chromote")
    skip_if_not_installed("ggplot2")
    skip_if_not_installed("plotly")
    skip_if_not_installed("htmlwidgets")

    library(ggplot2)
    library(plotly)

    p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) + geom_point()
    p_plotly <- suppressWarnings(ggplotly(p))
    decorated <- suppressWarnings(add_plotly_export_csv(p_plotly))

    tmp_html <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(decorated, tmp_html)

    b <- chromote::ChromoteSession$new()
    on.exit(
        {
            try(b$close(), silent = TRUE)
            unlink(tmp_html)
        },
        add = TRUE
    )

    b$Page$navigate(paste0("file://", normalizePath(tmp_html, winslash = "/")))

    # Poll until plotly is ready and _tracesToCSV is present
    is_ready <- FALSE
    for (i in seq_len(50)) {
        Sys.sleep(0.1)
        res <- tryCatch(
            {
                b$Runtime$evaluate(
                    "
        (function() {
          const gd = document.querySelector('.js-plotly-plot');
          return !!(gd && gd._tracesToCSV);
        })()
      "
                )$result$value
            },
            error = function(e) FALSE
        )
        if (isTRUE(res)) {
            is_ready <- TRUE
            break
        }
    }

    expect_true(is_ready)

    # evaluate hijacking code and get the CSV text
    res <- b$Runtime$evaluate(
        "
    (function() {
      const gd = document.querySelector('.js-plotly-plot');
      if (!gd) return 'NO GD';
      if (!gd._tracesToCSV) return 'NO _tracesToCSV';
      return gd._tracesToCSV(gd);
    })();
  "
    )

    csv_string <- res$result$value

    expect_type(csv_string, "character")
    expect_true(nchar(csv_string) > 0)

    # Parse the CSV string
    df_exported <- read.csv(text = csv_string, stringsAsFactors = FALSE)

    # ggplotly usually assigns the traces based on grouped aesthetics or splits them
    # The payload must contain all the original rows from mtcars (since no filtering was applied)
    expect_equal(nrow(df_exported), nrow(mtcars))
})

