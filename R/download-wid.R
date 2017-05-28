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
#' @param verbose Should the function indicate the progress of the request?
#' Default is \code{FALSE}.
#'
#' @importFrom httr GET add_headers content
#' @importFrom utils read.csv
#'
#' @export

download_wid <- function(indicators="all", areas="all", years="all", ages="all",
                         populations="all", metadata=FALSE, verbose=FALSE) {

    # Make sure that at least some indicators and some areas were selected
    if (indicators == "all" && areas == "all") {
        stop("You must select at least some specific indicators, areas, or both.")
    }

    # Concatenate area codes, if necessary
    if (areas != "all") {
        query_areas <- paste(areas, collapse=",")
    }

    if (verbose) {
        cat("* Get variables associated to your selection...")
    }

    # Get the variables associated to the area(s)
    url <- paste0(
        "https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/prod/",
        "wid-countries-available-variables?countries=", areas, "&variables=all"
    )
    response_request <- GET(url, add_headers("x-api-key"=api_key))
    response_content <- eval(parse(text=content(response_request, as="text", encoding="UTF-8")), envir=NULL)
    response_table <- read.csv(text=response_content, stringsAsFactors=FALSE,
        colClasses=c("character", "character", "character", "integer", "character"))

    return(response_table)
}
