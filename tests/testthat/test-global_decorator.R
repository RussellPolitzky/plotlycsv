test_that("use_plotly_csv() returns a script tag", {
    skip_if_not_installed("htmltools")
    tags <- use_plotly_csv()
    expect_s3_class(tags, "shiny.tag")
    expect_equal(tags$name, "script")
})

test_that("use_plotly_csv() respects custom filename", {
    skip_if_not_installed("htmltools")
    tags <- use_plotly_csv(filename = "custom_export.csv")
    expect_match(as.character(tags), "custom_export.csv")
})

test_that("use_plotly_csv() script contains MutationObserver and fallback filename", {
    skip_if_not_installed("htmltools")
    js_code <- as.character(use_plotly_csv())
    expect_match(js_code, "MutationObserver")
    expect_match(js_code, "plotly-data.csv")
})

# ── Regression tests for the pre-render injection fix ──────────────────────────
#
# Root cause (fixed): the old implementation called Plotly.react() *after* the
# plot was rendered, passing the same gd._context object back as the new config.
# Plotly.react diffs old vs new by reference; finding no change it skipped the
# modebar rebuild and the button never appeared.
#
# The correct approach:
#   1. hookPlotly() is called IMMEDIATELY when the script runs (not deferred to
#      DOMContentLoaded), so the hook is installed before HTMLWidgets calls
#      Plotly.newPlot.
#   2. injectButton() mutates the config/figure BEFORE calling the original
#      Plotly.newPlot, so the button is present in the initial render.
#   3. setupHelpers() (called after render) only attaches helper functions to gd
#      — it does NOT call Plotly.react.

test_that("use_plotly_csv() injects button BEFORE render via injectButton()", {
    skip_if_not_installed("htmltools")
    js_code <- as.character(use_plotly_csv())

    # injectButton must exist — it is responsible for pre-render injection
    expect_match(js_code, "function injectButton")

    # injectButton must be called inside the wrap() closure (i.e. before orig.apply)
    # Verify the call order: injectButton(args) appears before orig.apply in the source
    pos_inject  <- regexpr("injectButton\\(args\\)", js_code)
    pos_origapp <- regexpr("orig\\.apply", js_code)
    expect_true(
        pos_inject < pos_origapp,
        label = "injectButton(args) must appear before orig.apply() in the generated JS"
    )
})

test_that("use_plotly_csv() calls hookPlotly() immediately, not only on DOMContentLoaded", {
    skip_if_not_installed("htmltools")
    js_code <- as.character(use_plotly_csv())

    # hookPlotly() must be called at the top level of the IIFE (immediate call),
    # outside and BEFORE the DOMContentLoaded / readyState block.
    pos_immediate_hook <- regexpr("hookPlotly\\(\\);\\s*\n\\s*if.*readyState", js_code, perl = TRUE)
    expect_true(
        pos_immediate_hook > 0,
        label = "hookPlotly() must be called immediately before the readyState check"
    )
})

test_that("use_plotly_csv() setupHelpers does NOT call Plotly.react", {
    skip_if_not_installed("htmltools")
    js_code <- as.character(use_plotly_csv())

    # Extract the body of setupHelpers by finding the text between its opening
    # brace and the next top-level function definition.
    setup_start <- regexpr("function setupHelpers", js_code)
    hook_start  <- regexpr("function hookPlotly",   js_code)
    expect_true(setup_start > 0 && hook_start > setup_start)

    setup_body <- substring(js_code, setup_start, hook_start - 1)

    # The setupHelpers body must not contain a Plotly.react call — that was the bug
    expect_false(
        grepl("Plotly\\.react", setup_body),
        label = "setupHelpers() must not call Plotly.react (that was the regression)"
    )
})

test_that("use_plotly_csv() hasCsvButton guards against duplicate buttons", {
    skip_if_not_installed("htmltools")
    js_code <- as.character(use_plotly_csv())
    expect_match(js_code, "function hasCsvButton")
    # injectButton must check hasCsvButton before concatenating
    inject_start <- regexpr("function injectButton", js_code)
    inject_end   <- regexpr("function setupHelpers", js_code)
    inject_body  <- substring(js_code, inject_start, inject_end - 1)
    expect_match(inject_body, "hasCsvButton")
})
