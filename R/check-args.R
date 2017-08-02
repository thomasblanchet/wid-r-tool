#' @title Check list of indicator codes
#'
#' @author Thomas Blanchet
#'
#' @description Check that the list of indicator codes submitted by the
#' user is valid.
#'
#' @param indicators List of indicators.

check_indicators <- function(indicators) {
    if (length(indicators) > 1 || indicators != "all") {
        invalid <- !grepl("^[a-z]{6}$", indicators)
        if (any(invalid)) {
            stop(paste("indicators must be 6-letter codes, the following are",
                "invalid:", paste(indicators[invalid], collapse=", ")))
        }
    }
}

#' @title Check list of area codes
#'
#' @author Thomas Blanchet
#'
#' @description Check that the list of area codes submitted by the
#' user is valid.
#'
#' @param areas List of area codes

check_areas <- function(areas) {
    if (length(areas) > 1 || areas != "all") {
        invalid <- !grepl("^[A-Z]{2}(-[A-Z]{2})?$", areas)
        if (any(invalid)) {
            stop(paste("areas codes must take the form XX or XX-YY, the following are",
                "invalid:", paste(areas[invalid], collapse=", ")))
        }
    }
}

#' @title Check list of years
#'
#' @author Thomas Blanchet
#'
#' @description Check that the list of years submitted by the user is valid
#'
#' @param years List of years

check_years <- function(years) {
    if (length(years) > 1 || years != "all") {
        years_num <- suppressWarnings(as.numeric(years))
        invalid <- is.na(years_num) | years_num > 9999 | years_num < 1000
        if (any(invalid)) {
            stop(paste("the following years are invalid:", paste(years[invalid], collapse=", ")))
        }
    }
}

#' @title Check list of percentiles
#'
#' @author Thomas Blanchet
#'
#' @description Check that the list of percentiles submitted by the user is valid
#'
#' @param perc List of percentiles

check_perc <- function(perc) {
    if (length(perc) > 1 || perc != "all") {
        invalid <- !grepl("^p[0-9]+(\\.[0-9]+)?(p[0-9]+(\\.[0-9]+)?)?$", perc)
        if (any(invalid)) {
            stop(paste("percentiles must take the form pXX or pXXpYY, the following are",
                "invalid:", paste(perc[invalid], collapse=", ")))
        }
    }
}

#' @title Check list of age codes
#'
#' @author Thomas Blanchet
#'
#' @description Check that the list of age codes submitted by the
#' user is valid.
#'
#' @param ages List of age codes

check_ages <- function(ages) {
    if (length(ages) > 1 || ages != "all") {
        ages_num <- suppressWarnings(as.numeric(ages))
        invalid <- is.na(ages_num) | ages_num > 999 | ages_num < 100
        if (any(invalid)) {
            stop(paste("ages must be numerical codes between 100 and 999, the",
                "following are invalid:", paste(ages[invalid], collapse=", ")))
        }
    }
}

#' @title Check list of population codes
#'
#' @author Thomas Blanchet
#'
#' @description Check that the list of population codes submitted by the
#' user is valid.
#'
#' @param pop List of population codes

check_pop <- function(pop) {
    if (length(pop) > 1 || pop != "all") {
        invalid <- !grepl("^[ijmfte]$", pop)
        if (any(invalid)) {
            stop(paste("population codes must be 'i', 'j', 'm', 'f', 't' or 'e',",
                "the following are invalid:", paste(pop[invalid], collapse=", ")))
        }
    }
}

