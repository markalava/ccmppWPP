###-----------------------------------------------------------------------------
### * Helpers

## Quick and dirty way of validating 'x' as a list returned by
## \code{\link{DDextract_ccmppWPPinputs_tier1}}

valid_DDextract_ccmppWPPinputs_tier1 <- function(x) {
    fail <- FALSE
    stop_msg <- character()

    if (!is.list(x)) fail <- TRUE
    if (!identical(sort(names(x)),
                   sort(c("ccmppWPP_inputs",
                           "pop_count_age_sex_reference")))) fail <- TRUE

    ## Reference Pops
    req_names <- c("time_start", "time_span",
                   "age_start", "age_span", "sex", "value")
    missing_nms <- req_names[!req_names %in% names(x$pop_count_age_sex_reference)]
    if (length(missing_nms)) {
        stop_msg <- paste0(stop_msg, "\n",
                           paste0("'x$pop_count_age_sex_reference' must have all of the following elements: ",
                                  toString(req_names),
                                  ". ",
                                  toString(missing_nms),
                                  " is/are missing."))
        fail <- TRUE
    }

    ## CCMPP Inputs
    req_names <- get_all_required_ccmpp_input_list_element_names()
    missing_nms <- req_names[!req_names %in% names(x$ccmppWPP_inputs)]
    if (length(missing_nms)) {
        stop_msg <- paste0(stop_msg, "\n",
                           paste0("'x$ccmppWPP_inputs' must have all of the following elements: ",
                                  toString(req_names),
                                  ". ",
                                  toString(missing_nms),
                                  " is/are missing."))
        fail <- TRUE
        }

    ## Attributes
    req_attr_names <- c("revision", "locid", "variant")
    if (!all(req_attr_names %in% names(attributes(x$ccmppWPP_inputs)))) {
        stop_msg <- paste0(stop_msg, "\n",
                           paste0("'x$ccmppWPP_inputs' must have attributes ",
                                  toString(req_attr_names)))
        fail <- TRUE
    }

    if (fail)
        stop("'x' must be a list of the form returned by 'DDextract_ccmppWPPinputs_tier1'.",
             stop_msg)

    ## PASS!
    else
        return(TRUE)
}


###-----------------------------------------------------------------------------
### * Locations

#' Get the location ID for a DDextract
#'
#' This will identify the location to which the data in \code{x}
#' pertain. Note that this will simply extract the stored value which
#' (currently) could be a numeric or character identifier.
#'
#' @param x A list of the form returned by
#'     \code{\link{DDextract_ccmppWPPinputs_tier1}}
#' @return A numeric or character location ID.
#' @author Mark Wheldon
#' @export
DDextract_get_locid <- function(x) {
    stopifnot(valid_DDextract_ccmppWPPinputs_tier1(x))
    attributes(x$ccmppWPP_inputs)$locid
}


###-----------------------------------------------------------------------------
### * Times

###-----------------------------------------------------------------------------
### ** Census Years

#' Get years in which censuses were conducted
#'
#' This function takes a three digit numeric code (\cite{United
#' Nations, 2021}) for a location (country or area) and returns the
#' years in which censuses were conducted. This is a convenience
#' function for extracting the years from
#' \code{data("census_years"). This function is called by
#' \code{\link{DDextract_to_pop_count_age_sex_reference}}. See that
#' function's help file for additional notes.
#'
#' Years in which censuses were conducted are provided in
#' \code{data("census_years")} for many countries. Ranges of the form
#' \code{"xxxx-yyyy"} (e.g., \code{"2000-2005"}) are possible if a
#' rolling population count was conducted over several years. France,
#' for example, has conducted rolling censuses.
#'
#' @param x A numeric location code or location name as a character
#'     string, or a list of the form returned by
#'     \code{\link{DDextract_ccmppWPPinputs_tier1}}.
#' @return A \emph{character} vector of years.
#'
#' @family DDSQL_census_years
#'
#' @references
#' United Nations (2021). Standard country or area codes for
#'     statistical use (M49). Department of Economic and Social
#'     Affairs, Statistics
#'     Division. https://unstats.un.org/unsd/methodology/m49/
#'     (retrieved 1 April 2021).
#'
#' @author Mark Wheldon
#' @export
get_census_years <- function(x) {
    UseMethod("get_census_years")
}

#' @rdname get_census_years
#' @export
get_census_years.list <- function(x) {
    stopifnot(valid_DDextract_ccmppWPPinputs_tier1(x))
    get_census_years(DDextract_get_locid(x))
}

#' @rdname get_census_years
#' @export
get_census_years.numeric <- function(x) {
    stopifnot(identical(length(x), 1L))
    ccmppWPP::census_years[ccmppWPP::census_years$LocID %in% x,]$ReferencePeriod
}

#' @rdname get_census_years
#' @export
get_census_years.character <- function(x) {
    stopifnot(identical(length(x), 1L))
    ccmppWPP::census_years[ccmppWPP::census_years$LocName %in% x,]$ReferencePeriod
}


#' Parse vector of years containing ranges
#'
#' Parses a character vector of times (usually years). Single times
#' are left as-is and converted to numeric (via
#' \code{\link{as.numeric}}). Ranges in the form \code{"xxxx-yyyy"}
#' (e.g., as returned by \code{link{get_census_years}}) are replaced
#' by their midpoints, rounded down to the nearest integer, and then
#' converted. This function is called by
#' \code{\link{DDextract_to_pop_count_age_sex_reference}}. See that
#' function's help file for additional notes.
#'
#' @family DDSQL_census_years
#'
#' @param times A vector of times (probably years), numeric or
#'     character, possibly with ranges in the format
#'     \code{"xxxx-yyyy"} (see \dQuote{Description}).
#' @return A numeric vector of times.
#'
#' @author Mark Wheldon
#' @export
parse_census_years_ranges <- function(times) {
    if (is.character(times)) {
        ## If there is a range, take the mid-point
        times <- sapply(times, FUN = function(z) {
            spl <- as.numeric(unlist(strsplit(z, "-")))
            if (any(!is.finite(spl)))
                stop("'times' was recognized as having ranges of the form 'xxxx-yyyy' but some or all of these elements did not yield valid numeric values after splitting.")
            if (identical(length(spl), 1L)) times <- spl
            else if (identical(length(spl), 2L)) {
                times <- floor(mean(spl))
            }
            else stop("Cannot understand some elements of 'times'. If 'times' is character, all elements must be coercible to numeric values, or be two coercible values separated by '-', e.g., '1990', '1990-1995'.")
        })
    }
    times <- unique(as.numeric(unlist(times)))
    if (any(is.na(times))) stop("'times' has 'NA' elements (possibly after parsing at '-').")
    if (!all(is.finite(times)))
        stop("Some, or all, elements of 'times' are not finite after parsing.")
    return(times)
}


###-----------------------------------------------------------------------------
### ** Reference Pop Times

## Exclude the baseline year
exclude_baseline_pop_count_times <- function(x, times) {
    stopifnot(valid_DDextract_ccmppWPPinputs_tier1(x))
    base_times <- as.numeric(unique(x$ccmppWPP_inputs$pop_count_age_sex_base$time_start))
    if (!all(is.finite(base_times)))
        stop("Cannot determine the 'times' of the baseline population counts in 'x$ccmppWPP'.")
    return(times[!times %in% base_times])
}


#' Get reference population times from DDSQL extract
#'
#' Get the times of the \code{pop_count_age_sex_reference} element of
#' \code{x}. This function is primarily intended to be called via
#' \code{\link{DDextract_to_pop_count_age_sex_reference}}. See that
#' function's help file for more details.
#'
#' @family DDSQL_extract
#' @seealso \code{\link{ccmpp_input_list}}, \code{\link{pop_count_age_sex_base}}
#'
#' @param x A list of the form returned by
#'     \code{\link{DDextract_ccmppWPPinputs_tier1}}.
#' @param excl_baseline Should the time of the baseline population
#'     count be included in the return value?
#' @return A numeric vector of times.
#'
#' @author Mark Wheldon
#' @noRd
get_pop_count_reference_times <- function(x, excl_baseline = FALSE) {
    stopifnot(valid_DDextract_ccmppWPPinputs_tier1(x))
    times <- unique(as.numeric(x$pop_count_age_sex_reference$time_start))
    if (!all(is.finite(times)))
        stop("'times' were taken from 'x$pop_count_age_sex_reference' but some are missing or non-finite.")
    if (excl_baseline) return(exclude_baseline_pop_count_times(x, times))
    else return(times)
}



#' Get reference population counts from a DDSQL query
#'
#' Extracts and (optionally) subsets reference population data from a
#' list of the kind returned by
#' \code{\link{DDextract_ccmppWPPinputs_tier1}}. The result is
#' returned as a \code{\link{demog_change_component_df}} object.
#'
#' \code{x} must be a
#' \code{\link{list}} with a \code{\link{data.frame}} element named
#' \code{pop_count_age_sex_reference}. This \code{data.frame} must
#' have colums \code{time_start}, \code{time_span}, \code{age_start},
#' \code{age_span}, \code{sex}, \code{value}.
#'
#' Argument \code{times} can be used to return counts for specific
#' times only. These can be specified explicitly as a numeric vector
#' or one of the key words \describe{
#' \item{\dQuote{excl_baseline}}{All years available except the
#' baseline year.}
#' \item{\dQuote{all}}{All years available.}
#' \item{\dQuote{census}}{Only the years censuses were
#' conducted. Census years are taken from the dataset
#' \code{"census_years"} provided with the package.
#' \item{\dQuote{census_excl_baseline}}{Census years excluding the
#' baseline year (if the baseline year was not a census year this will
#' give the same result as \dQuote{census}).}}
#'
#' @section Note:
#' Census times cannot be determined by default because the object
#' returned by \code{\link{DDextract_ccmppWPPinputs_tier1}} does not
#' record the location to which the data pertain. Use
#' \code{\link{get_census_years}} to separately look up census years.
#'
#' @param x A list of the form returned by
#'     \code{\link{DDextract_ccmppWPPinputs_tier1}}.
#' @param times Either a numeric vector containing years for which
#'     reference population counts should be extracted from \code{x},
#'     or a character string specifying a pre-defined set of times;
#'     see \dQuote{Details}.
#' @param ... Passed to other methods.
#' @return An object of class \code{\link{demog_change_component_df}}
#'     containing the reference population counts.
#'
#' @family DDSQL_extract
#' @author Mark Wheldon
#'
#' @examples
#'
#' data("france_wpp_1950_2020_population_data")
#' data("census_years")
#'
#' all_years <-
#'   DDextract_get_pop_count_age_sex_reference(france_wpp_1950_2020_population_data)
#'
#' census_years_only <-
#'   DDextract_get_pop_count_age_sex_reference(france_wpp_1950_2020_population_data,
#'                                    times = "census")
#'
#' selected_years <-
#'   DDextract_get_pop_count_age_sex_reference(france_wpp_1950_2020_population_data,
#'                                    times = 1950:1955)
#'
#' @export
DDextract_get_pop_count_age_sex_reference <- function(x,
                                                      times = c("all", "excl_baseline", "census",
                                                                "census_excl_baseline")) {
    op <- getOption("ccmppWPP.suppress_S3_class_messages")
    options(ccmppWPP.suppress_S3_class_messages = TRUE)
    on.exit(options(ccmppWPP.suppress_S3_class_messages = op))

    stopifnot(valid_DDextract_ccmppWPPinputs_tier1(x))

    ## 'times'
    if (!is.numeric(times)) {
        times <- match.arg(times)
        if (identical(times, "all"))
            return(demog_change_component_df(x$pop_count_age_sex_reference))
        else if (identical(times, "excl_baseline"))
            times <- get_pop_count_reference_times(x, excl_baseline = TRUE)
        else if (identical(times, "census"))
            times <- parse_census_years_ranges(get_census_years(x))
        else if (identical(times, "census_excl_baseline"))
            times <-
                exclude_baseline_pop_count_times(x,
                    parse_census_years_ranges(get_census_years(x)))
    } else {
        stop("'times' is not valid.")
    }

    ## Subset and return
    return(subset_time(as_demog_change_component_df(x$pop_count_age_sex_reference,
                                                    value_type =  "count"),
                       times))
}


#' Convert Demo Data extract to a CCMPP input list
#'
#' Takes the output of \code{\link{DDextract_ccmppWPPinputs_tier1}}
#' and returns an object inheriting from
#' \code{\link{ccmpp_input_list}}. Note that this will discard some
#' components of the input (e.g., the reference population counts).
#'
#' This function extracts the CCMPP input list component from \code{x}
#' and checks that the components are consistent with each
#' other. Currenlty, this involves selecting rows for times that all
#' components have in common.
#'
#' @param x A list of the form returned by \code{\link{DDextract_ccmppWPPinputs_tier1}}.
#' @return An object inheriting from \code{\link{ccmpp_input_list}}.
#' @author Mark Wheldon
#' @export
DDextract_get_ccmpp_input_list <- function(x) {
    op <- getOption("ccmppWPP.suppress_S3_class_messages")
    options(ccmppWPP.suppress_S3_class_messages = TRUE)
    on.exit(options(ccmppWPP.suppress_S3_class_messages = op))

    stopifnot(valid_DDextract_ccmppWPPinputs_tier1(x))

    ## Make all ccmpp inputs have common times. Do this 'manually',
    ## i.e,. not using ccmppWPP functions. To use 'ccmppWPP::times'
    ## all elements have to be coerced to ccmpp_input_df objects first
    ## which is time conusming and wasteful because this will be done
    ## by 'as_ccmpp_input_list' anyway. A downside is that the name of
    ## the time columne ('time_start') is hardcoded.
    req_names <- get_all_required_ccmpp_input_list_element_names()
    all_but_baseline_names <- req_names[!req_names %in% "pop_count_age_sex_base"]
    common_times <-
        Reduce("intersect",
               lapply(x$ccmppWPP_inputs[all_but_baseline_names], function(z) {
                   unique(z$time_start)
               }))
    baseline_time <- unique(x$ccmppWPP_inputs$pop_count_age_sex_base$time_start)
    if (!identical(length(baseline_time), 1L))
        stop("Baseline population data frame has more than one unique 'time_start'.")
    if (!baseline_time %in% common_times)
        stop("Baseline population counts are for year ", sQuote(baseline_time),
             " but this is not in the set of common times for other indicators (",
             sQuote(paste0(toString(common_times, width = 26))), ").")
    x <- lapply(x$ccmppWPP_inputs[req_names], function(z, common_times) {
        z[z$time_start %in% common_times, ]
    }, common_times = common_times)

    return(as_ccmpp_input_list(x))
}



