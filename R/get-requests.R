#' @title Get variables associated to a list of area codes
#'
#' @author Thomas Blanchet
#'
#' @description Perform the GET request to the server to retrieve all
#' variables for a list of area codes.
#'
#' @param areas List of area codes.
#'
#' @importFrom httr GET add_headers content
#' @importFrom utils read.csv
#' @importFrom base64enc base64encode
#' @importFrom stringi stri_unescape_unicode

get_variables_areas <- function(areas) {
    # Concatenate area codes
    query_areas <- paste(areas, collapse=",")

    # Perform request
    url <- paste0(
        "https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/prod/",
        "wid-countries-available-variables?countries=", query_areas, "&variables=all"
    )
    response_request <- GET(url, add_headers("x-api-key"=base64encode(api_key)))
    response_content <- stri_unescape_unicode(content(response_request, as="text", encoding="UTF-8"))
    response_content <- trimws(response_content, whitespace = '"')

    response_table <- read.csv(text=response_content, stringsAsFactors=FALSE,
        colClasses=c("character", "character", "character", "integer", "character"))

    return(response_table)
}

#' @title Get data associated to a list of variables
#'
#' @author Thomas Blanchet
#'
#' @description Perform GET request to the server to retrieve data associated
#' to a list of variables.
#'
#' @param areas List of area codes.
#' @param variables List of variables, of the form: \code{"xxxxxx_pXXpYY_999_i"}
#' @param years List of years.
#'
#' @importFrom httr GET add_headers content
#' @importFrom utils read.csv
#' @importFrom base64enc base64encode
#' @importFrom stringi stri_unescape_unicode

get_data_variables <- function(areas, variables, years) {
    # Concatenate area codes, variables & years
    query_areas <- paste(areas, collapse=",")
    query_variables <- paste(variables, collapse=",")
    query_years <- paste(years, collapse=",")

    # Perform request
    url <- paste0(
        "https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/prod/",
        "wid-countries-variables-dl?countries=", query_areas,
        "&variables=", query_variables, "&years=", query_years
    )
    response_request <- GET(url, add_headers("x-api-key"=base64encode(api_key)))
    response_content <- stri_unescape_unicode(content(response_request, as="text", encoding="UTF-8"))
    response_content <- trimws(response_content, whitespace = '"')

    response_table <- read.csv(text=response_content, stringsAsFactors=FALSE,
        colClasses=c("character", "character", "character", "integer", "numeric"))

    return(response_table)
}

#' @title Get metadata associated to a list of variables
#'
#' @author Thomas Blanchet
#'
#' @description Perform GET request to the server to retrieve metadata
#' associated to a list of variables.
#'
#' @param areas List of area codes.
#' @param variables List of variables, of the form: \code{"xxxxxx_999_i"}
#'
#' @importFrom httr GET add_headers content
#' @importFrom utils read.csv
#' @importFrom base64enc base64encode
#' @importFrom stringi stri_unescape_unicode

get_metadata_variables <- function(areas, variables) {
    # Concatenate area codes, variables & years
    query_areas <- paste(areas, collapse=",")
    query_variables <- paste(variables, collapse=",")

    # Perform request
    url <- paste0(
        "https://rfap9nitz6.execute-api.eu-west-1.amazonaws.com/prod/",
        "wid-countries-variables-metadata?countries=", query_areas,
        "&variables=", query_variables)
    response_request <- GET(url, add_headers("x-api-key"=base64encode(api_key)))
    response_content <- stri_unescape_unicode(content(response_request, as="text", encoding="UTF-8"))
    response_content <- trimws(response_content, whitespace = '"')

    response_table <- read.csv(text=response_content, stringsAsFactors=FALSE,
        header=FALSE, skip=1, col.names=c("variable", "shortname", "shortdes", "pop", "age",
            "country", "source", "method", "empty"),
        colClasses=c("character", "character", "character", "character",
            "character", "character", "character", "character"))
    response_table$empty <- NULL

    return(response_table)
}

