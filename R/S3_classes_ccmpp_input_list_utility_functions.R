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
### * Age-time matrices

#' Coerce a \code{ccmpp_input_list} to a list of age-time matrices
#'
#' A list of age * time matrices will be created from \code{x} by
#' calling \code{\link{as_age_time_matrix}} on each element.
#'
#' @seealso \code{\link{as_age_time_matrix}}
#'
#' @inheritParams as_age_time_matrix
#' @return A \code{list}.
#' @author Mark Wheldon
#' @name as_age_time_matrix_list
NULL

#' @rdname as_age_time_matrix_list
#' @export
as_age_time_matrix_list <- function(x, drop_zero_fert_ages = FALSE, ...) {
    UseMethod("as_age_time_matrix_list")
}

#' @rdname as_age_time_matrix_list
#' @export
as_age_time_matrix_list.ccmpp_input_list <- function(x, drop_zero_fert_ages = FALSE, ...) {
    lapply(x, function(y) {
        if (is_by_indicator(y)) {
            y <- as_demog_change_component_df(y)
                                #because cannot subset most of the ccmpp input
                                #elements. Note that this will mean dispatch will go
                                #straight to the 'as_demog_change_component_df' method;
                                #any methods added for ccmpp_input_df or subclasses will
                                #not be called
            names_of_indicators_y <- setNames(indicators(y), indicators(y))
            lapply(names_of_indicators_y, function(this_indicator_name) {
                y_subset_by_indicator <-
                    subset_indicator(y, indicators = this_indicator_name, drop = TRUE)
                if (is_by_sex(y_subset_by_indicator)) {
                    names_of_sexes_y_subset_by_indicator <-
                        setNames(sexes(y_subset_by_indicator),
                                 sexes(y_subset_by_indicator))
                    lapply(names_of_sexes_y_subset_by_indicator, function(this_sex_name) {
                        y_subset_by_indicator_and_sex <-
                            subset_sex(y_subset_by_indicator, this_sex_name, drop = TRUE)
                        as_age_time_matrix(y_subset_by_indicator_and_sex,
                                           drop_zero_fert_ages = drop_zero_fert_ages, ...)
                    })
                } else {
                    as_age_time_matrix(y_subset_by_indicator,
                                       drop_zero_fert_ages = drop_zero_fert_ages, ...)
                }
            })
        } else if (is_by_sex(y)) {
            y <- as_demog_change_component_df(y)
                                #because cannot subset most of the ccmpp input
                                #elements. Note that this will mean dispatch will go
                                #straight to the 'as_demog_change_component_df' method;
                                #any methods added for ccmpp_input_df or subclasses will
                                #not be called
            names_of_sexes_y <- setNames(sexes(y), sexes(y))
            lapply(names_of_sexes_y, function(this_sex_name) {
                this_sex_obj_subset <- subset_sex(y, this_sex_name, drop = TRUE)
                as_age_time_matrix(this_sex_obj_subset,
                                   drop_zero_fert_ages = drop_zero_fert_ages, ...)
            })
        } else {
            as_age_time_matrix(y, drop_zero_fert_ages = drop_zero_fert_ages, ...)
        }
    })
}
