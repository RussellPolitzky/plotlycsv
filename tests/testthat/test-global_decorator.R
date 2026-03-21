test_that("use_plotly_csv() returns the expected script tag", {
    skip_if_not_installed("htmltools")
    library(htmltools)

    # Call function
    tags <- use_plotly_csv()

    # Check class
    expect_s3_class(tags, "shiny.tag")
    expect_equal(tags$name, "script")

    # Check contents
    js_code <- as.character(tags)
    expect_match(js_code, "MutationObserver")
    expect_match(js_code, "decoratePlot")
    expect_match(js_code, "plotly-data.csv")
})

test_that("use_plotly_csv() respects custom filename", {
    skip_if_not_installed("htmltools")
    tags <- use_plotly_csv(filename = "custom_export.csv")
    expect_match(as.character(tags), "custom_export.csv")
})
