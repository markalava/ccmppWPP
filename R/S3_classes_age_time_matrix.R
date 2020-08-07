###-----------------------------------------------------------------------------
### * Age-time matrices

#' Coerce a \code{demog_change_component_df} to \code{matrix}
#'
#' An age * time matrix will be created from the input object,
#' \code{x}, which must not have a \dQuote{sex} dimension. To create a
#' matrix from such an object, first remove the \dQuote{sex} dimension
#' with \code{\link{subset_sex}}.
#'
#' @param x An object inheriting from class
#'     \code{demog_change_component_df}.
#' @param drop_zero_fert_ages Logical; should rows corresponding to
#'     \dQuote{non_zero_fert_ages} be dropped when converting
#'     \code{fert_rate_age_f} objects.
#' @param ... Further arguments passed to and from methods.
#' @return A \code{matrix}.
#' @author Mark Wheldon
#' @name as_age_time_matrix
NULL

#' @rdname as_age_time_matrix
#' @export
as_age_time_matrix <- function(x, ...) {
    UseMethod("as_age_time_matrix")
}

#' @rdname as_age_time_matrix
#' @export
as_age_time_matrix.demog_change_component_df <- function(x, ...) {

    if (is_by_sex(x))
        stop("'x' has dimension \"sex\"; select a single sex using 'subset_sex(..., drop = TRUE)' to create an age-time matrix.")

    if (is_by_indicator(x))
        stop("'x' has dimension \"indicator\"; select a single indicator using 'subset_indicator(..., drop = TRUE)' to create an age-time matrix.")

    dims <- demog_change_component_dimensions(x)

    if (identical(length(dims), 1L)) {
        if(is_by_time(x)) {
            tx <- times(x)
            return(matrix(x$value,
                          nrow = 1, ncol = length(tx), byrow = FALSE,
                          dimnames = list(NULL, time = tx)))
        } else if (is_by_age(x)) {
            ax <- ages(x)
            return(matrix(x$value,
                          nrow = length(ax), ncol = 1, byrow = FALSE,
                          dimnames = list(age = ax, NULL)))
        }
    } else {
            ax <- ages(x)
            tx <- times(x)
            return(matrix(x$value,
                          nrow = length(ax), ncol = length(tx),
                          byrow = FALSE,
                          dimnames = list(age = ax, time = tx)))
    }
}


###-----------------------------------------------------------------------------
### * Age-time matrix Lists

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
