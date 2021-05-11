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
#' @param loc_id
#' @return A \emph{character} vector of years.
#'
#' @family DDSQL_extract
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
get_census_years <- function(loc_id) {
    census_years[census_years$LocID == loc_id,]$ReferencePeriod
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
#' @param times A vector of times (probably years), numeric or
#'     character, possibly with ranges in the format
#'     \code{"xxxx-yyyy"} (see \dQuote{Description}).
#' @return A numeric vector of times.
#' @family DDSQL_extract
#' @author Mark Wheldon
#' @export
parse_pop_count_reference_times_ranges <- function(times) {
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


#' Get reference population times from DDSQL extract
#'
#' \code{DDextract_exclude_baseline_pop_count_times} subsets its argument
#' \code{times} by excluding the time of the baseline population
#' counts in the \code{ccmppWPP_inputs} element of
#' \code{x}. \code{DDextract_get_pop_count_reference_times} is a wrapper that
#' sets \code{times} to the times of the
#' \code{pop_count_age_sex_reference} element of \code{x}. This
#' function is primarily intended to be called via
#' \code{\link{DDextract_to_pop_count_age_sex_reference}}. See that function's
#' help file for more details.
#'
#' @param x A list of the form returned by \code{\link{DDextract_ccmppWPPinputs_tier1}}.
#' @return A numeric vector of times.
#' @family DDSQL_extract
#' @seealso \code{\link{ccmpp_input_list}}, \code{\link{pop_count_age_sex_base}}
#' @author Mark Wheldon
#' @export
DDextract_get_pop_count_reference_times <- function(x) {
    times <- unique(as.numeric(x$pop_count_age_sex_reference$time_start))
    if (!all(is.finite(times)))
        stop("'times' were taken from 'x$pop_count_age_sex_reference' but some are not finite.")
    return(DDextract_exclude_baseline_pop_count_times(x, times))
}

#' @rdname DDextract_get_pop_count_reference_times
#' @aliases DDextract_exclude_baseline_pop_count_times
#' @export
DDextract_exclude_baseline_pop_count_times <- function(x, times) {
    base_times <- as.numeric(unique(x$ccmppWPP_inputs$pop_count_age_sex_base$time_start))
    if (!all(is.finite(base_times)))
        stop("Cannot determine the 'times' of the baseline population counts in 'x$ccmppWPP'.")
    return(times[!times %in% base_times])
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
#' \code{times} can be used to return counts for specific times only,
#' e.g., specific years. If \code{times} is \code{NULL} (default) it
#' will be initialized to the times of
#' \code{x$pop_count_age_sex_reference}. You might, instead, want to
#' set it to something determined by
#' \code{\link{get_census_years}}. Regardless,
#' \code{\link{extract_pop_count_reference_times}} will be
#' called. This will discard any elements of \code{times} not in
#' \code{times(x$pop_count_age_sex_reference)} and then parse the
#' result using
#' \code{\link{parse_pop_count_reference_times_ranges}}. Once
#' \code{times} is set and parsed, the time of the baseline population
#' counts in \code{pop_count_base_component(x$ccmppWPP_inputs)} will
#' be removed.
#'
#' @param x A list of the form returned by
#'     \code{\link{DDextract_ccmppWPPinputs_tier1}}.
#' @param times Vector containing years for which reference population
#'     counts should be extracted from \code{x}. Can be numeric or
#'     character; see \dQuote{Details}. If \code{NULL} (default), all
#'     times will be retained.
#' @param ... Passed to other methods.
#' @return An object of class
#'     \code{\link{demog_change_component_df}} containing
#'     the reference population counts.
#' @family DDSQL_extract
#' @author Mark Wheldon
#'
#' @examples
#'
#' data("france_wpp_1950_2020_population_data")
#' data("census_years")
#'
#' all_years <-
#'   DDextract_to_pop_count_age_sex_reference(france_wpp_1950_2020_population_data)
#'
#' census_years_only <-
#'   DDextract_to_pop_count_age_sex_reference(france_wpp_1950_2020_population_data,
#'                                    times = get_census_years(250))
#'
#' @export
DDextract_get_pop_count_age_sex_reference <- function(x, times = NULL) {
    ## Checks
    stopifnot("pop_count_age_sex_reference" %in% names(x))

    req_names <- c("time_start", "time_span",
                   "age_start", "age_span", "sex", "value")
    missing_nms <- req_names[!req_names %in% names(x$pop_count_age_sex_reference)]
    if (length(missing_nms))
        stop("'x$pop_count_age_sex_reference' must have all of the following elements: ",
             toString(req_names),
             ". ",
             toString(missing_nms),
             " is/are missing.")

    ## 'times'
    if (is.null(times)) {
        times <- DDextract_get_pop_count_reference_times(x)
    } else {
        times <- DDextract_exclude_baseline_pop_count_times(x,
                   times = parse_pop_count_reference_times_ranges(times))
    }

    ## Subset
    pop_count_age_sex_reference <- demog_change_component_df(x$pop_count_age_sex_reference)
    return(subset_time(pop_count_age_sex_reference, times))
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

    ## Check
    stopifnot("ccmppWPP_inputs" %in% names(x))
    req_names <- c("pop_count_age_sex_base",
                   "life_table_age_sex", "fert_rate_age_f",
                   "srb", "mig_net_count_age_sex",
                   "mig_net_rate_age_sex", "mig_net_count_tot_b",
                   "mig_parameter")
    missing_nms <- req_names[!req_names %in% names(x$ccmppWPP_inputs)]
    if (length(missing_nms))
        stop("'x$ccmppWPP_inputs' must have all of the following elements: ",
             toString(req_names),
             ". ",
             toString(missing_nms),
             " is/are missing.")

    ## Make all ccmpp inputs have common times. Do this 'manually',
    ## i.e,. not using ccmppWPP functions. To use 'ccmppWPP::times'
    ## all elements have to be coerced to ccmpp_input_df objects first
    ## which is time conusming and wasteful because this will be done
    ## by 'as_ccmpp_input_list' anyway. A downside is that the name of
    ## the time columne ('time_start') is hardcoded.
    all_but_baseline_names <- req_names[!req_names %in% "pop_count_age_sex_base"]
    common_times <-
        Reduce("intersect",
               lapply(x$ccmppWPP_inputs[all_but_baseline_names], function(z) {
                   unique(z$time_start)
               }))
    x <- lapply(x$ccmppWPP_inputs[req_names], function(z, common_times) {
        z[z$time_start %in% common_times, ]
    }, common_times = common_times)

    return(as_ccmpp_input_list(x))
}


