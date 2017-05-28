#' @title Download data from WID.world
#'
#' @author Thomas Blanchet
#'
#' @description Downloads data from the World Wealth and Income Database
#' (\url{WID.world}) into a \code{data.frame}.
#'
#' @param indicators List of five-letter strings, or \code{"all"}:
#' code names of the indicators in the database. Default is "all" for all
#' indicators. See 'Details' for more.
#' @param areas List of strings, or \code{"all"}: area code names of the
#' database. \code{"XX"} for countries/regions, \code{"XX-YY"} for subregions.
#' Default is \code{"all"} for all areas. See 'Details' for more.
#' @param years Numerical vector, or \code{"all"}: years to retrieve.
#' Default is \code{"all"} for all years.
#' @param ages Numerical vector, or \code{"all"}: age category codes in the
#' database. 999 for all ages, 992 for adults. Default is \code{"all"} for all
#' age categories. See 'Details' for more.
#' @param populations List of characters, or \code{"all"}: type of population.
#' \code{"t"} for tax units, \code{"i"} for individuals. Default is
#' \code{"all"} for all population types. See 'Details' for more.
#' @param metadata Should the function fetch metadata too (ie. variable
#' descriptions, sources, methodological notes, etc.)? Default is \code{FALSE}.

download_wid <- function(indicators="all", areas="all", years="all", ages="all",
                         populations="all", metadata=FALSE) {
    return(NULL)
}
