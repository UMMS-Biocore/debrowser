require(shiny)
library(debrowser)
library(testthat)

test_that("demo data can be loaded", {
    load(system.file("extdata", "demo", "demodata.Rda",
        package = "debrowser"))
    expect_true(is.data.frame(demodata))
    expect_equal(demodata[29311, 2], 2)
    expect_equal(demodata[29311, 5], 7.1)
    expect_equal(demodata[29311, 6], 6)
    expect_null(demodata[1, 7])
})
