context("Server requests")

test_that("request for variables in area(s) works as expected", {
    skip_on_cran()

    df <- get_variables_areas("FR")
    expect_equal(nrow(df), 15355)
    expect_equal(ncol(df), 5)

    df <- get_variables_areas("XX")
    expect_equal(nrow(df), 0)
})

test_that("request for data works as expected", {
    skip_on_cran()

    df <- get_data_variables("FR", "sptinc_p99p100_992_t")
    expect_equal(nrow(df), 102)
    expect_equal(ncol(df), 4)

    df <- get_data_variables("FR", "xxxxxx_p99p100_992_t")
    expect_equal(nrow(df), 0)
})

test_that("request for metadata works as expected", {
    skip_on_cran()

    df <- get_metadata_variables("FR", "sptinc_p99p100_992_t")
    expect_equal(nrow(df), 1)
    expect_equal(ncol(df), 16)

    df <- get_metadata_variables("FR", "xxxxxx_p99p100_992_t")
    expect_equal(nrow(df), 0)
})

