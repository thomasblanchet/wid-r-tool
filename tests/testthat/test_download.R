context("Test overall downloads")

test_that("we can download mutliple indicators for a single country", {
    skip_on_cran()

    data <- download_wid(
        areas = "FR",
        indicators = c("sfiinc", "aptinc"),
        perc = c("p90p100", "p20p30"),
        years = 1990:2000
    )

    expect_true(all(data$year >= 1990 & data$year <= 2000))
    expect_true(all(data$country == "FR"))
    expect_true(identical(unique(data$percentile), c("p20p30", "p90p100")))
    expect_true(identical(
        unique(substr(data$variable, 1, 6)),
        sort(c("aptinc", "sfiinc"))
    ))
})

test_that("we can download a single indicator for multiple countries", {
    skip_on_cran()

    data <- download_wid(
        areas = c("FR", "US"),
        indicators = "sptinc",
        perc = "p90p100",
        ages = "992",
        pop = "j",
        years = 1990:2000
    )

    expect_true(all(data$percentile == "p90p100"))
    expect_true(identical(
        unique(data$country),
        sort(c("FR", "US"))
    ))
    expect_true(all(data$year >= 1990 & data$year <= 2000))
    expect_true(all(substr(data$variable, 10, 11) == "j"))
    expect_true(all(substr(data$variable, 07, 09) == "992"))
    expect_true(all(substr(data$variable, 01, 06) == "sptinc"))
})

test_that("we can download population data", {
    skip_on_cran()

    data <- download_wid(
        areas = "DE",
        indicators = "npopul"
    )

    expect_true(all(data$country == "DE"))
    expect_true(all(data$percentile == "p0p100"))
    expect_true(all(substr(data$variable, 1, 6) == "npopul"))
    expect_true(identical(
        unique(substr(data$variable, 10, 11)),
        sort(c("i", "f", "m"))
    ))
})

test_that("we can download metadata", {
    skip_on_cran()

    data <- download_wid(
        areas = "FR",
        indicators = "sptinc",
        perc = "p99p100",
        age = "992",
        pop = "j",
        metadata = TRUE
    )

    expect_true(all(data$country == "FR"))
    expect_true(all(data$countryname == "France"))
    expect_true(all(data$variable == "sptinc992j"))
    expect_true(all(data$percentile == "p99p100"))
    expect_true(all(data$quality == "4"))
    expect_true(all(data$imputation == "surveys and tax data"))
})

test_that("we can exclude extrapolations/interpolations", {
    skip_on_cran()

    data <- download_wid(
        areas = "MZ",
        indicators = "sptinc",
        perc = "p99p100",
        age = "992",
        pop = "j",
        include_extrapolations = TRUE
    )

    data_noextra <- download_wid(
        areas = "MZ",
        indicators = "sptinc",
        perc = "p99p100",
        age = "992",
        pop = "j",
        include_extrapolations = FALSE
    )

    expect_true(all(data$country == "MZ"))
    expect_true(all(data$variable == "sptinc992j"))
    expect_true(all(data$percentile == "p99p100"))

    expect_true(all(data$country == "MZ"))
    expect_true(all(data_noextra$variable == "sptinc992j"))
    expect_true(all(data_noextra$percentile == "p99p100"))

    data_both <- merge(data, data_noextra,
        by = c("country", "variable", "percentile", "year"), all = TRUE)

    expect_true(all(data_both$value.x == data_both$value.y | is.na(data_both$value.y)))
})
