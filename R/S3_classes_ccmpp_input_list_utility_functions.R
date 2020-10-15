###-----------------------------------------------------------------------------
### * Subset

#' Subset by time, age, or sex
#'
#' These functions subset the component data frames of objects
#' inheriting from \code{\link{ccmpp_input_list}} by \dQuote{time} or
#' \dQuote{age} and retain the class if valid (see
#' \dQuote{Details}). Subsetting such that only one level of the
#' subset dimension is retained will drop the dimension if \code{drop
#' = TRUE}, otherwise it is retained (default). Subsetting a
#' \code{ccmpp_input_list} by \dQuote{indicator} or
#' \dQuote{sex} cannot produce a valid member of the class.
#'
#' If the object returned after subsetting is a valid member of the
#' original class it will be returned (valid according to
#' \code{\link{validate_ccmpp_object}}). If it is not valid an error
#' will be signalled and nothing is returned.
#'
#' @section Note:
#' These functions are not particularly efficient. For repeated
#' subsetting within, e.g., a for loop, it is better to use standard
#' subsetting operations on the data frame component and re-cast the
#' result as a classed object at the end if desired.
#'
#' @seealso \code{link{subset_time}}, etc., for the S3 generic
#'     function; \code{link{subset_time.demog_change_component_df}}
#'     for the method for objects inheriting from
#'     \code{demog_change_component_df}.
#'
#' @param x An object to subset.
#' @param time,age Vectors indicating the levels of
#'     time, age, or sex to retain.
#' @param drop Logical; should demographic change component dimensions
#'     with only one level be dropped?
#' @return The object after subsetting if a valid member of the class;
#'     otherwise an error.
#' @author Mark Wheldon
#' @name subset_ccmpp_input_list
#' @family subset_by_demographic_dimension
NULL

#' @rdname subset_ccmpp_input_list
#' @export
subset_time.ccmpp_input_list <- function(x, times, drop = FALSE) {

    for (df_nm in names(x)) {
        if (is_by_time(x[[df_nm]]))
            x[[df_nm]] <- subset_time(x[[df_nm]], times = times, drop = drop)
    }
    return(as_ccmpp_input_list(as.list(x)))
}

#' @rdname subset_ccmpp_input_list
#' @export
subset_age.ccmpp_input_list <- function(x, ages, drop = FALSE) {

    if (isFALSE(0 %in% ages))
        stop("'0' must be in 'ages' for the result to be a valid 'ccmpp_input_list'.")

    for (df_nm in names(x)) {
        if (is_by_age(x[[df_nm]]))
            x[[df_nm]] <- subset_age(x[[df_nm]], ages = ages, drop = drop)
    }
    return(as_ccmpp_input_list(as.list(x)))
}

###-----------------------------------------------------------------------------
### * Transformations

#' @rdname fert_rate_tot_f
#' @export
fert_rate_tot_f.ccmpp_input_list <- function(x) {
    fert_rate_tot_f(fert_rate_component(x))
}
