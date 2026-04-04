test_that("add_plotly_export_csv() respects selected_only", {
    skip_if_not_installed("plotly")
    library(plotly)

    p <- plot_ly(mtcars, x = ~wt, y = ~mpg) |>
        add_plotly_export_csv(selected_only = TRUE)

    js <- p$jsHooks$render[[1]]$code
    expect_match(js, "var selected_only    = true;",                              fixed = TRUE)
    expect_match(js, "if (selected_only && hasSelection && !selected.includes(i)) continue;", fixed = TRUE)
})

test_that("add_plotly_export_csv() respects custom delimiter", {
    skip_if_not_installed("plotly")
    library(plotly)

    p <- plot_ly(mtcars, x = ~wt, y = ~mpg) |>
        add_plotly_export_csv(sep = ";")

    js <- p$jsHooks$render[[1]]$code
    expect_match(js, "var sep              = ';';", fixed = TRUE)
})

test_that("use_plotly_csv() respects selected_only and sep", {
    skip_if_not_installed("htmltools")
    js_code <- as.character(use_plotly_csv(selected_only = TRUE, sep = "|"))

    expect_match(js_code, "global_selected_only")
    expect_match(js_code, "global_selected_only    = true", fixed = TRUE)
    expect_match(js_code, "global_sep              = '|'",  fixed = TRUE)
})

# ── Item 6: parameter parity ──────────────────────────────────────────────────

test_that("use_plotly_csv() exposes visible_only, include_z, include_customdata, include_text", {
    skip_if_not_installed("htmltools")

    js_default <- as.character(use_plotly_csv())
    expect_match(js_default, "global_visible_only     = true",      fixed = TRUE)
    expect_match(js_default, "global_include_text     = true",      fixed = TRUE)
    expect_match(js_default, "global_include_z        = false",     fixed = TRUE)
    expect_match(js_default, "global_include_customdata = false",   fixed = TRUE)

    js_z <- as.character(use_plotly_csv(include_z = TRUE))
    expect_match(js_z, "global_include_z        = true", fixed = TRUE)

    js_hidden <- as.character(use_plotly_csv(visible_only = FALSE))
    expect_match(js_hidden, "global_visible_only     = false", fixed = TRUE)
})

test_that("use_plotly_csv() passes all opts to tracesToCSV via setupHelpers", {
    skip_if_not_installed("htmltools")
    js_code <- as.character(use_plotly_csv(include_z = TRUE, include_customdata = TRUE))

    # opts object in setupHelpers must reference all global flags
    setup_start <- regexpr("function setupHelpers", js_code)
    hook_start  <- regexpr("function hookPlotly",   js_code)
    setup_body  <- substring(js_code, setup_start, hook_start - 1)

    expect_match(setup_body, "include_z")
    expect_match(setup_body, "include_customdata")
    expect_match(setup_body, "visible_only")
    expect_match(setup_body, "include_text")
})

# ── Item 3: axis labels ───────────────────────────────────────────────────────

test_that("use_plotly_csv() tracesToCSV uses axisLabel for column headers", {
    skip_if_not_installed("htmltools")
    js_code <- as.character(use_plotly_csv())

    expect_match(js_code, "function axisLabel")
    # header uses variables, not string literals 'x'/'y'
    csv_start  <- regexpr("function tracesToCSV", js_code)
    axis_start <- regexpr("function axisLabel",   js_code)
    # axisLabel is defined before tracesToCSV uses it
    expect_true(axis_start < csv_start ||
                    grepl("xLabel", substring(js_code, csv_start, csv_start + 500)))
    expect_match(js_code, "xLabel")
    expect_match(js_code, "yLabel")
})

test_that("add_plotly_export_csv() onRender JS uses axisLabel for column headers", {
    skip_if_not_installed("plotly")
    library(plotly)

    js <- plot_ly(mtcars, x = ~wt, y = ~mpg) |>
        add_plotly_export_csv() |>
        (\(p) p$jsHooks$render[[1]]$code)()

    expect_match(js, "function axisLabel")
    expect_match(js, "xLabel")
    expect_match(js, "yLabel")
})

# ── Item 8: clipboard button ──────────────────────────────────────────────────

test_that("use_plotly_csv() adds clipboard button by default", {
    skip_if_not_installed("htmltools")
    js_code <- as.character(use_plotly_csv())

    expect_match(js_code, "global_clipboard        = true",  fixed = TRUE)
    expect_match(js_code, "clipButton")
    expect_match(js_code, "copyToClipboard")
    expect_match(js_code, "navigator.clipboard")
})

test_that("use_plotly_csv(clipboard = FALSE) omits clipboard button from injection", {
    skip_if_not_installed("htmltools")
    js_code <- as.character(use_plotly_csv(clipboard = FALSE))

    expect_match(js_code, "global_clipboard        = false", fixed = TRUE)
    # injectButton must guard on global_clipboard before pushing clipButton
    inject_start <- regexpr("function injectButton", js_code)
    inject_end   <- regexpr("function setupHelpers", js_code)
    inject_body  <- substring(js_code, inject_start, inject_end - 1)
    expect_match(inject_body, "global_clipboard")
})

test_that("add_plotly_export_csv() adds Copy CSV button by default", {
    skip_if_not_installed("plotly")
    library(plotly)

    p <- plot_ly(mtcars, x = ~wt, y = ~mpg) |>
        add_plotly_export_csv()

    button_names <- vapply(
        p$x$config$modeBarButtonsToAdd,
        function(b) if (is.list(b) && !is.null(b$name)) b$name else "",
        character(1)
    )
    expect_true("Copy CSV" %in% button_names)
})

test_that("add_plotly_export_csv(clipboard = FALSE) omits Copy CSV button", {
    skip_if_not_installed("plotly")
    library(plotly)

    p <- plot_ly(mtcars, x = ~wt, y = ~mpg) |>
        add_plotly_export_csv(clipboard = FALSE)

    button_names <- vapply(
        p$x$config$modeBarButtonsToAdd,
        function(b) if (is.list(b) && !is.null(b$name)) b$name else "",
        character(1)
    )
    expect_false("Copy CSV" %in% button_names)
    expect_true("Download CSV" %in% button_names)
})

test_that("add_plotly_export_csv() onRender JS contains copyToClipboard", {
    skip_if_not_installed("plotly")
    library(plotly)

    js <- plot_ly(mtcars, x = ~wt, y = ~mpg) |>
        add_plotly_export_csv() |>
        (\(p) p$jsHooks$render[[1]]$code)()

    expect_match(js, "copyToClipboard")
    expect_match(js, "navigator.clipboard")
    expect_match(js, "gd._copyToClipboard")
})
