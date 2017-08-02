#' @title Download data from WID.world
#'
#' @author Thomas Blanchet
#'
#' @description Downloads data from the World Wealth and Income Database
#' (\url{http://WID.world}) into a \code{data.frame}.
#'
#' @param indicators List of six-letter strings, or \code{"all"}:
#' code names of the indicators in the database. Default is \code{"all"} for all
#' indicators. See 'Details' for more.
#' @param areas List of strings, or \code{"all"}: area code names of the
#' database. \code{"XX"} for countries/regions, \code{"XX-YY"} for subregions.
#' Default is \code{"all"} for all areas. See 'Details' for more.
#' @param perc List of strings, or \code{"all"}: percentiles take the form
#' \code{"pXX"} or \code{"pXXpYY"}. Default is \code{"all"} for all percentiles.
#' See 'Details' for more.
#' @param years Numerical vector, or \code{"all"}: years to retrieve.
#' Default is \code{"all"} for all years.
#' @param ages Numerical vector, or \code{"all"}: age category codes in the
#' database. 999 for all ages, 992 for adults. Default is \code{"all"} for all
#' age categories. See 'Details' for more.
#' @param pop List of characters, or \code{"all"}: type of population.
#' \code{"t"} for tax units, \code{"i"} for individuals. Default is
#' \code{"all"} for all population types. See 'Details' for more.
#' @param metadata Should the function fetch metadata too (ie. variable
#' descriptions, sources, methodological notes, etc.)? Default is \code{FALSE}.
#' @param verbose Should the function indicate the progress of the request?
#' Default is \code{FALSE}.
#'
#' @importFrom plyr ddply
#'
#' @export

download_wid <- function(indicators="all", areas="all", years="all", perc="all",
                         ages="all", pop="all", metadata=FALSE, verbose=FALSE) {

    # Make sure that at least some indicators and some areas were selected
    if (indicators == "all" && areas == "all") {
        stop("you must select at least some specific indicators, areas, or both.")
    }

    # Check the format of arguments
    check_indicators(indicators)
    check_areas(areas)
    check_years(years)
    check_perc(perc)
    check_ages(ages)
    check_pop(pop)

    if (verbose) {
        cat("* Get variables associated to your selection...")
    }

    # Get the variables associated to the area(s)
    variables <- get_variables_areas(areas)

    # If empty response, return NULL
    if (nrow(variables) == 0) {
        if (verbose) {
            cat("DONE\n")
            cat("(no data matching your selection)\n")
        }
        return(NULL)
    }

    # data.frame of specified indicators
    if (indicators == "all") {
        df_indicators <- NULL
    } else {
        df_indicators <- data.frame(variable=indicators)
    }
    # data.frame of specified percentiles
    if (perc == "all") {
        df_perc <- NULL
    } else {
        df_perc <- data.frame(percentile=perc)
    }
    # data.frame of ages
    if (ages == "all") {
        df_ages <- NULL
    } else {
        df_ages <- data.frame(age=ages)
    }
    # data.frame of population codes
    if (pop == "all") {
        df_pop <- NULL
    } else {
        df_pop <- data.frame(pop=pop)
    }

    # Only keep the variables that match the user selection
    if (!is.null(df_indicators)) {
        variables <- merge(variables, df_indicators, by="variable", all.x=FALSE, all.y=TRUE)
    }
    if (!is.null(df_perc)) {
        variables <- merge(variables, df_perc, by="percentile", all.x=FALSE, all.y=TRUE)
    }
    if (!is.null(df_ages)) {
        variables <- merge(variables, df_ages, by="age", all.x=FALSE, all.y=TRUE)
    }
    if (!is.null(df_pop)) {
        variables <- merge(variables, df_pop, by="pop", all.x=FALSE, all.y=TRUE)
    }

    # Check that there are some data left
    if (nrow(variables) == 0) {
        if (verbose) {
            cat("DONE\n")
            cat("(no data matching your selection)\n")
        }
        return(NULL)
    }

    # Display how many variables remain, if requested
    if (verbose) {
        cat("DONE\n")

        nb_indicator <- length(unique(variables$variable))
        cat("(found", nb_indicator, "variable")
        if (nb_indicator > 1) {
            cat("s")
        }

        nb_country <- length(unique(variables$country))
        cat(paste(" for", nb_country, "area"))
        if (nb_country > 1) {
            cat("s")
        }

        nb_perc <- length(unique(variables$percentile))
        cat(paste(",", nb_perc, "percentile"))
        if (nb_perc > 1) {
            cat("s")
        }

        nb_age <- length(unique(variables$age))
        cat(paste(",", nb_age, "age categor"))
        if (nb_age > 1) {
            cat("ies")
        } else {
            cat("y")
        }

        nb_pop <- length(unique(variables$pop))
        cat(paste(",", nb_pop, "population categor"))
        if (nb_pop > 1) {
            cat("ies)\n")
        } else {
            cat("y)\n")
        }
        cat("* Download the data...")
    }

    # Generate variable names used in the API
    variables$data_codes <- paste(variables$variable, variables$percentile,
        variables$age, variables$pop, sep="_")

    # Divide the data in smaller chunks to avoid request that are too large
    variables$chunk <- round(1:nrow(variables)/50)
    data <- ddply(variables, "chunk", function(variables) {
        query_codes <- unique(variables$data_codes)
        query_areas <- unique(variables$country)

        return(get_data_variables(query_areas, query_codes, years))
    })

    # Remove potential duplicates
    data$chunk <- NULL
    data <- data[!duplicated(data[, c("country", "indicator", "percentile", "year")]), ]
    data$variable <- paste0(
        substr(data$indicator, 1, 6),
        substr(data$indicator, 8, 10),
        substr(data$indicator, 12, 12)
    )

    if (verbose) {
        cat("DONE\n")
    }

    # Retrieve metadata, if requested
    if (metadata) {
        if (verbose) {
            cat("* Download the metadata...")
        }

        # Only keep the information necessary for the metadata, and then
        # proceed similarly as for the data
        variables$metadata_codes <- paste(variables$variable, variables$age,
            variables$pop, sep="_")
        variables <- unique(variables[, c("country", "metadata_codes")])

        variables$chunk <- round(1:nrow(variables)/50)
        metadata <- ddply(variables, "chunk", function(variables) {
            query_codes <- unique(variables$metadata_codes)
            query_areas <- unique(variables$country)

            return(get_metadata_variables(query_areas, query_codes))
        })
        metadata$chunk <- NULL

        # Merge with the data
        data <- merge(data, metadata, by=c("variable", "country"), all.x=TRUE, all.y=FALSE)

        if (verbose) {
            cat("DONE\n")
        }
    }

    # Clean the up the final dataset
    data$indicator <- NULL
    data <- data[order(data$country, data$variable, data$percentile, data$year), ]

    return(data)
}
