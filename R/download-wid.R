#' @title Download data from WID.world
#'
#' @author Thomas Blanchet
#'
#' @description Downloads data from the World Wealth and Income Database
#' (\url{http://WID.world}) into a \code{data.frame}.
#' Type \code{vignette("wid-demo")} for a detailed presentation.
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
#' @param include_extrapolations Should the function return estimates that are
#' the results of extrapolations and interpolations based on limited data?
#' Default is \code{TRUE}.
#' @param verbose Should the function indicate the progress of the request?
#' Default is \code{FALSE}.
#'
#' @details
#'
#' Although all arguments default to \code{"all"}, you cannot download the
#' entire database by typing \code{download_wid()}. The command requires you
#' to specify either some indicators or some areas. \strong{To download the entire
#' database, please visit \url{https://wid.world/data/} and choose "download
#' full dataset".}
#'
#' If there is no data matching you selection on WID.world (maybe because
#' you specified an indicator or an area that doesn't exist), the command
#' will return \code{NULL} with a warning.
#'
#' All monetary amounts for countries and country subregions are in constant
#' local currency of the reference year (i.e. the previous year, the database
#' being updated every year around July). Monetary amounts for world regions
#' are in EUR PPP of the reference year. You can access the price index using
#' the indicator \code{inyixx}, the PPP exchange rates using \code{xlcusp}
#' (USD), \code{xlceup} (EUR), \code{xlcyup} (CNY), and the market exchange
#' rates using \code{xlcusx} (USD), \code{xlceux} (EUR), \code{xlcyux}
#' (CNY). To check the current reference year, you can look at when the price
#' index is equal to 1.
#'
#' Shares and wealth/income ratios are given as a fraction of 1. That is,
#' a top 1\% share of 20\% is given as 0.2. A wealth/income ratio of
#' 300\% is given as 3.
#'
#' The arguments of the command follow a nomenclature specific to WID.world.
#' We provide more details with a few examples below. \strong{For the complete
#' up-to-date documentation of the structure of the database, please visit
#' \url{https://wid.world/codes-dictionary}.}
#'
#' \subsection{Indicators}{
#' The argument \code{indicators} is a vector of 6-letter codes that corresponds to a
#' given series type for a given income or wealth concept. The first letter
#' correspond to the type of series. Some of the most common possibilities include:
#' \tabular{rcl}{
#' \bold{one-letter code} \tab      \tab \bold{description}     \cr
#' \code{a} \tab      \tab average             \cr
#' \code{s} \tab      \tab share               \cr
#' \code{t} \tab      \tab threshold           \cr
#' \code{m} \tab      \tab macroeconomic total \cr
#' \code{w} \tab      \tab wealth/income ratio \cr
#' }
#'
#' The next five letters correspond a concept (usually of income and wealth).
#' Some of the most common possibilities include:
#' \tabular{rcl}{
#' \bold{five-letter code} \tab      \tab \bold{description} \cr
#' \code{ptinc} \tab      \tab pre-tax national income \cr
#' \code{pllin} \tab      \tab pre-tax labor income    \cr
#' \code{pkkin} \tab      \tab pre-tax capital income  \cr
#' \code{fiinc} \tab      \tab fiscal income           \cr
#' \code{hweal} \tab      \tab net personal wealth     \cr
#' }
#'
#' For example, \code{sfiinc} corresponds to the share of fiscal income,
#' \code{ahweal} corresponds to average personal wealth. If you don't specify
#' any indicator, it defaults to \code{"all"} and downloads all available indicators.
#' }
#'
#' \subsection{Area codes}{
#' All data in WID.world is associated to a given area, which can be a country,
#' a region within a country, an aggregation of countries (eg. a continent), or
#' even the whole world. The argument \code{areas} is a vector of codes that specify
#' the areas for which to retrieve data. Countries and world regions are coded
#' using 2-letter ISO codes. Country subregions are coded as \code{XX-YY}
#' where \code{XX} is the country 2-letter code. If you don't specify any area,
#' it defaults to \code{"all"} and downloads data for all available areas.
#' }
#'
#' \subsection{Years}{
#' All data in WID.world correspond to a year. Some series go as far back as
#' the 1800s. The argument \code{years} is a vector of integer that specify
#' those years. If you don't specify any year, it defaults to \code{"all"}
#' and downloads data for all available years.
#' }
#'
#' \subsection{Percentiles}{
#' The key feature of WID.world is that it provides data on the whole
#' distribution, not just totals and averages. The argument \code{perc}
#' is a vector of strings that indicate for which part of the distribution
#' the data should be retrieved. For share and average variables,
#' percentiles correspond to percentile ranges and take the form \code{pXXpYY}.
#' For example the top 1\% share correspond to \code{p99p100}. The top 10\% share
#' excluding the top 1\% is \code{p90p99}. Thresholds associated to the
#' percentile group \code{pXXpYY} correspond to the minimal income or wealth
#' level that gets you into the group. For example, the threshold of the
#' percentile group \code{p90p100} or \code{p90p91} correspond to the 90\%
#' quantile. Variables with no distributional meaning use the percentile p0p100.
#' If you don't specify any percentile, it defaults to \code{"all"} and
#' downloads data for all available parts of the distribution.
#' }
#'
#' \subsection{Age groups}{
#' Data may only concern the population in a certain age group.
#' The argument \code{ages} is a vector of age codes that specify which
#' age categories to retrieve. Ages are coded using 3-digit codes.
#' Some of the most common possibilities include:
#' \tabular{rcl}{
#' \bold{three-digit code} \tab      \tab \bold{description} \cr
#' \code{999} \tab      \tab all ages                          \cr
#' \code{992} \tab      \tab adults, including elderly (20+)   \cr
#' \code{996} \tab      \tab adults, excluding elderly (20-65) \cr
#' }
#' If you don't specify any age, it defaults to \code{"all"} and downloads
#' data for all available age groups.
#' }
#'
#' \subsection{Population types}{
#' The data in WID.world can refer to different types of population
#' (i.e. different statistical units). The argument \code{pop} is a vector of
#' population codes. They are coded using one-letter codes. Some of the
#' most common possibilities include:
#' \tabular{rcl}{
#' \bold{one-letter code} \tab      \tab \bold{description} \cr
#' \code{i} \tab      \tab individuals                                                             \cr
#' \code{t} \tab      \tab tax units                                                               \cr
#' \code{j} \tab      \tab equal-split adults (ie. income or wealth divided equally among spouses) \cr
#' }
#' If you don't specify any code, it defaults to \code{"all"}
#' and downloads data for all types of population.
#' }
#'
#' \subsection{Extrapolations/interpolations}{
#' Some of the data on WID.world is the result of interpolations (when data
#' is only available for a few years) or extrapolations (when data is not
#' available for the most recent years) that are based on much more limited
#' information that other data points. We include these
#' interpolations/extrapolation by default as a convenience, and also because
#' these values are used to perform regional aggregations. Yet we stress that
#' these estimates, especially at the level of individual countries, can be
#' fragile.
#'
#' For many purposes, it can be preferable to exclude these data points.
#' For that, use the option \code{include_extrapolations = FALSE}.
#' }
#'
#' @return A \code{data.frame} with the following columns:
#' \describe{
#'     \item{\code{country}}{The country or area code.}
#'     \item{\code{variable}}{The variable name, which combine the indicator,
#'     the age code and the population code.}
#'     \item{\code{percentile}}{The part of the distribution the value relates to.}
#'     \item{\code{year}}{The year the value relates to.}
#'     \item{\code{value}}{The value of the indicator.}
#' }
#' If you specify \code{metadata = TRUE}, the \code{data.frame} also has the
#' following columns:
#' \describe{
#'     \item{\code{countryname}}{The full name of the country/region.}
#'     \item{\code{shortname}}{A short version of the variable full name in plain english.}
#'     \item{\code{shortdes}}{A description of the type of series.}
#'     \item{\code{pop}}{The population type, in plain english.}
#'     \item{\code{age}}{The age group, in plain english.}
#'     \item{\code{source}}{The source for the data.}
#'     \item{\code{method}}{Methodological notes, if any.}
#'     \item{\code{imputation}}{Type of estimate (when applicable). The \code{imputation}
#'         field is a short qualitative description of the type of estimate provided,
#'         which is strongly related to data quality. For technical details, see
#'         the \code{method} field and papers cited in \code{source}.}
#'     \item{\code{quality}}{Data quality (when applicable). The \code{quality}
#'         field is a score from 0 to 5 indicating the quality of the data.}
#' }
#'
#' @importFrom plyr ddply ldply
#'
#' @export

download_wid <- function(indicators = "all", areas = "all", years = "all", perc = "all",
                         ages = "all", pop = "all", metadata = FALSE,
                         include_extrapolations = TRUE, verbose = FALSE) {

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
    variables <- ldply(indicators, function(sixlet) get_variables_areas(areas, sixlet))

    # If empty response, return NULL
    if (nrow(variables) == 0) {
        if (verbose) {
            cat("DONE\n")
            cat("(no data matching your selection)\n")
        }
        warning("no data matching selection")
        return(NULL)
    }

    # data.frame of specified indicators
    if (length(indicators) == 1 && indicators == "all") {
        df_indicators <- NULL
    } else {
        df_indicators <- data.frame(variable = indicators)
    }
    # data.frame of specified percentiles
    if (length(perc) == 1 && perc == "all") {
        df_perc <- NULL
    } else {
        df_perc <- data.frame(percentile = perc)
    }
    # data.frame of ages
    if (length(ages) == 1 && ages == "all") {
        df_ages <- NULL
    } else {
        df_ages <- data.frame(age = ages)
    }
    # data.frame of population codes
    if (length(pop) == 1 && pop == "all") {
        df_pop <- NULL
    } else {
        df_pop <- data.frame(pop = pop)
    }

    # Only keep the variables that match the user selection
    if (!is.null(df_indicators)) {
        variables <- merge(variables, df_indicators, by = "variable", all.x = FALSE, all.y = FALSE)
    }
    if (!is.null(df_perc)) {
        variables <- merge(variables, df_perc, by = "percentile", all.x = FALSE, all.y = FALSE)
    }
    if (!is.null(df_ages)) {
        variables <- merge(variables, df_ages, by = "age", all.x = FALSE, all.y = FALSE)
    }
    if (!is.null(df_pop)) {
        variables <- merge(variables, df_pop, by = "pop", all.x = FALSE, all.y = FALSE)
    }

    # Check that there are some data left
    if (nrow(variables) == 1 && all(is.na(variables[1, -1]))) {
        if (verbose) {
            cat("DONE\n")
            cat("(no data matching your selection)\n")
        }
        warning("no data matching selection")
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
        cat("* Download the data...\n")
    }

    # Generate variable names used in the API
    variables$data_codes <- paste(
        variables$variable,
        variables$percentile,
        variables$age,
        variables$pop,
        sep = "_"
    )

    # Divide the data in smaller chunks to avoid request that are too large:
    # group by variable and percentile
    variables$group <- as.numeric(factor(variables$data_code))
    variables$chunk <- floor(variables$group/10)

    data <- ddply(variables, "chunk", function(variables) {
        query_codes <- unique(variables$data_codes)
        query_areas <- unique(variables$country)

        return(get_data_variables(query_areas, query_codes,
            no_extrapolation = !include_extrapolations))
    }, .progress = ifelse(verbose, "text", "none"))

    # Remove potential duplicates
    data <- data[!duplicated(data[, c("country", "indicator", "year")]), ]

    # Remove years not requested
    if (!identical(years, "all")) {
        data <- data[(data$year %in% years), ]
    }

    # Extract codes without percentiles
    indicator <- t(simplify2array(strsplit(data$indicator, "_", fixed = TRUE)))
    data$variable <- paste0(indicator[, 1], indicator[, 3], indicator[, 4])
    data$percentile <- indicator[, 2]

    # Retrieve metadata, if requested
    if (metadata) {
        if (verbose) {
            cat("* Download the metadata...\n")
        }

        # Only keep the information necessary for the metadata, and then
        # proceed similarly as for the data
        variables$metadata_codes <- paste(
            variables$variable,
            variables$percentile,
            variables$age,
            variables$pop,
            sep = "_"
        )
        variables <- variables[!duplicated(variables[, c("country", "variable")]), ]

        variables$chunk <- floor(1:nrow(variables)/50)
        data_metadata <- ddply(variables, "chunk", function(variables) {
            query_codes <- unique(variables$metadata_codes)
            query_areas <- unique(variables$country)

            return(get_metadata_variables(query_areas, query_codes))
        }, .progress = ifelse(verbose, "text", "none"))
        data_metadata$chunk <- NULL

        # Remove percentile from variable
        indicator <- t(simplify2array(strsplit(data_metadata$variable, "_", fixed = TRUE)))
        data_metadata$variable <- paste0(indicator[, 1], indicator[, 3], indicator[, 4])

        # Merge with the data
        data <- merge(data, data_metadata,
            by = c("variable", "country"),
            all.x = TRUE,
            all.y = FALSE
        )
    }

    # Clean the up the final dataset
    data$indicator <- NULL
    data <- data[order(data$country, data$variable, data$percentile, data$year), ]
    rownames(data) <- NULL
    if (metadata) {
        data <- data[, c(
            "country", "countryname", "variable", "percentile", "year", "value",
            "shortname", "shortdes", "pop", "age", "source", "imputation", "quality"
        )]
    } else {
        data <- data[, c("country", "variable", "percentile", "year", "value")]
    }

    return(data)
}
