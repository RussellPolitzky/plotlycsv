test_that("add_plotly_export_csv() respects selected_only", {
    skip_if_not_installed("plotly")
    library(plotly)

    p <- plot_ly(mtcars, x = ~wt, y = ~mpg) |>
        add_plotly_export_csv(selected_only = TRUE)

    js <- p$jsHooks$render[[1]]$code
    expect_match(js, "selected_only: true", fixed = TRUE)
})

test_that("add_plotly_export_csv() respects custom delimiter", {
    skip_if_not_installed("plotly")
    library(plotly)

    p <- plot_ly(mtcars, x = ~wt, y = ~mpg) |>
        add_plotly_export_csv(sep = ";")

    js <- p$jsHooks$render[[1]]$code
    expect_match(js, "var sep = ';'", fixed = TRUE)
})

test_that("use_plotly_csv() respects new parameters", {
    skip_if_not_installed("htmltools")
    tags <- use_plotly_csv(selected_only = TRUE, sep = "|")
    js_code <- as.character(tags)

    expect_match(js_code, "selected_only: true", fixed = TRUE)
    expect_match(js_code, "var global_sep = '|'", fixed = TRUE)
})
