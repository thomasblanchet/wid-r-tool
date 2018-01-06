context("Server requests")

test_that("request for variables in area(s) works as expected", {
    skip_on_cran()

    df <- get_variables_areas("FR")
    expect_equal(nrow(df), 13454)
    expect_equal(ncol(df), 5)

    df <- get_variables_areas("XX")
    expect_equal(nrow(df), 0)
    expect_equal(ncol(df), 5)
})

test_that("request for data works as expected", {
    skip_on_cran()

    df <- get_data_variables("FR", "sptinc_p99p100_992_t", 1980:2000)
    expect_equal(nrow(df), 21)
    expect_equal(ncol(df), 5)

    df <- get_data_variables("FR", "xxxxxx_p99p100_992_t", 1980:2000)
    expect_equal(nrow(df), 0)
    expect_equal(ncol(df), 5)
})

test_that("request for metadata works as expected", {
    skip_on_cran()

    df <- get_metadata_variables("FR", "sptinc_992_t")
    expect_equal(nrow(df), 1)
    expect_equal(ncol(df), 8)

    df <- get_metadata_variables("FR", "xxxxxx_992_t")
    expect_equal(nrow(df), 0)
    expect_equal(ncol(df), 8)
})

