context("Argument checking")

test_that("correctly checks indicator names", {
    expect_error(check_indicators("999"),
        regexp="indicators must be 6-letter codes, the following are invalid: 999", fixed=TRUE)
    expect_error(check_indicators(c("998", "999")),
        regexp="indicators must be 6-letter codes, the following are invalid: 998, 999", fixed=TRUE)
    expect_error(check_indicators(c("998", "sfiinc", "999")),
        regexp="indicators must be 6-letter codes, the following are invalid: 998, 999", fixed=TRUE)
    expect_error(check_indicators(c("sfiinc", "all")),
        regexp="indicators must be 6-letter codes, the following are invalid: all", fixed=TRUE)

    expect_silent(check_indicators("all"))
    expect_silent(check_indicators("sfiinc"))
    expect_silent(check_indicators(c("sfiinc", "aptinc")))
})

test_that("correctly checks area codes", {
    expect_error(check_areas("00"),
        regexp="areas codes must take the form XX or XX-YY, the following are invalid: 00", fixed=TRUE)
    expect_error(check_areas(c("00", "01")),
        regexp="areas codes must take the form XX or XX-YY, the following are invalid: 00, 01", fixed=TRUE)
    expect_error(check_areas(c("00", "AA", "01")),
        regexp="areas codes must take the form XX or XX-YY, the following are invalid: 00, 01", fixed=TRUE)
    expect_error(check_areas(c("AA", "all")),
        regexp="areas codes must take the form XX or XX-YY, the following are invalid: all", fixed=TRUE)

    expect_silent(check_areas("all"))
    expect_silent(check_areas("AA"))
    expect_silent(check_areas("AA-BB"))
    expect_silent(check_areas(c("AA-BB", "AA")))
})

test_that("correctly checks years", {
    expect_error(check_years(10), regexp="the following years are invalid: 10", fixed=TRUE)
    expect_error(check_years(c(10, 11)), regexp="the following years are invalid: 10, 11", fixed=TRUE)
    expect_error(check_years(c(10, "2000", 11)), regexp="the following years are invalid: 10, 11", fixed=TRUE)
    expect_error(check_years(c(2000, "all")), regexp="the following years are invalid: all", fixed=TRUE)

    expect_silent(check_years("all"))
    expect_silent(check_years(2000))
    expect_silent(check_years("2000"))
    expect_silent(check_years(c(2000, "2001")))
})

test_that("correctly checks percentiles", {
    expect_error(check_perc("pAAp100"),
        regexp="percentiles must take the form pXX or pXXpYY, the following are invalid: pAAp100", fixed=TRUE)
    expect_error(check_perc(c("pAA", "pBBp100")),
        regexp="percentiles must take the form pXX or pXXpYY, the following are invalid: pAA, pBBp100", fixed=TRUE)
    expect_error(check_perc(c("pAA", "p99.9p100", "p1pBB")),
        regexp="percentiles must take the form pXX or pXXpYY, the following are invalid: pAA, p1pBB", fixed=TRUE)
    expect_error(check_perc(c("p99.5p100", "all")),
        regexp="percentiles must take the form pXX or pXXpYY, the following are invalid: all", fixed=TRUE)

    expect_silent(check_perc("all"))
    expect_silent(check_perc("p50"))
    expect_silent(check_perc("p99.5p99.6"))
    expect_silent(check_perc(c("p10p100", "p56p99.5")))
})

test_that("correctly checks age codes", {
    expect_error(check_ages(10),
        regexp="ages must be numerical codes between 100 and 999, the following are invalid: 10", fixed=TRUE)
    expect_error(check_ages(c(10, 11)),
        regexp="ages must be numerical codes between 100 and 999, the following are invalid: 10, 11", fixed=TRUE)
    expect_error(check_ages(c(10, "992", 11)),
        regexp="ages must be numerical codes between 100 and 999, the following are invalid: 10, 11", fixed=TRUE)
    expect_error(check_ages(c(992, "all")),
        regexp="ages must be numerical codes between 100 and 999, the following are invalid: all", fixed=TRUE)

    expect_silent(check_ages("all"))
    expect_silent(check_ages(992))
    expect_silent(check_ages("992"))
    expect_silent(check_ages(c(999, "992")))
})

test_that("correctly checks population codes", {
    expect_error(check_pop(10),
        regexp="population codes must be 'i', 'j', 'm', 'f', 't' or 'e', the following are invalid: 10", fixed=TRUE)
    expect_error(check_pop(c(10, "ij")),
        regexp="population codes must be 'i', 'j', 'm', 'f', 't' or 'e', the following are invalid: 10, ij", fixed=TRUE)
    expect_error(check_pop(c(10, "i", "ij")),
        regexp="population codes must be 'i', 'j', 'm', 'f', 't' or 'e', the following are invalid: 10, ij", fixed=TRUE)
    expect_error(check_pop(c("j", "all")),
        regexp="population codes must be 'i', 'j', 'm', 'f', 't' or 'e', the following are invalid: all", fixed=TRUE)

    expect_silent(check_pop("all"))
    expect_silent(check_pop("i"))
    expect_silent(check_pop(c("i", "j")))
})

