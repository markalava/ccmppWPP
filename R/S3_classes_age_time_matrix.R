################################################################################
###
### 'age_time_matrix's are not (yet) a formal S3 class but could
### become so if worthwhile later on.
###
### Part of the 'matrix' method for 'as_demog_change_component_df' is
### motived by the 'age_time_matrix' concept.
###
################################################################################


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
        stop("'x' has dimension \"sex\"; select a single sex using 'subset_sex(as_demog_change_component_df(x), ..., drop = TRUE)' to create an age-time matrix. See '?as_age_time_matrix_list' for an alternative approach.")

    if (is_by_indicator(x))
        stop("'x' has dimension \"indicator\"; select a single indicator using 'subset_indicator(as_demog_change_component_df(x), ..., drop = TRUE)' to create an age-time matrix. See '?as_age_time_matrix_list' for an alternative approach.")

    dims <- demog_change_component_dimensions(x)

    if (identical(length(dims), 1L)) {
        if(is_by_time(x)) {
            tx <- times(x)
            return(matrix(x$value,
                          nrow = 1, ncol = length(tx), byrow = FALSE,
                          dimnames = list(NULL, time_start = tx)))
        } else if (is_by_age(x)) {
            ax <- ages(x)
            return(matrix(x$value,
                          nrow = length(ax), ncol = 1, byrow = FALSE,
                          dimnames = list(age_start = ax, NULL)))
        }
    } else {
            ax <- ages(x)
            tx <- times(x)
            return(matrix(x$value,
                          nrow = length(ax), ncol = length(tx),
                          byrow = FALSE,
                          dimnames = list(age_start = ax, time_start = tx)))
    }
}

#' @rdname as_age_time_matrix
#' @export
as_age_time_matrix.fert_rate_age_f <- function(x, drop_zero_fert_ages = FALSE, ...) {
    nzfa <- non_zero_fert_ages(x)
    x <- NextMethod()
    if (drop_zero_fert_ages)
        x <- x[dimnames(x)$age %in% nzfa, ]
    return(x)
    }


###-----------------------------------------------------------------------------
### * Age-time matrix Lists

#' Coerce a \code{ccmpp_input_list} to a list of age-time matrices
#'
#' A list of age * time matrices will be created from \code{x} by
#' calling \code{\link{as_age_time_matrix}} according to the
#' method. The \code{ccmpp_input_list} method
#'
#' @seealso \code{\link{as_age_time_matrix}}
#'
#' @param x Depending on the method
#' \describe{
#'   \item{\code{demog_change_component_df}}{An object inheriting from \code{\link{demog_change_component_df}}}
#'   \item{\code{ccmpp_input_list}}{A \code{\link{ccmpp_input_list}} object}}
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
as_age_time_matrix_list.demog_change_component_df <- function(x, drop_zero_fert_ages = FALSE, ...) {
    if (is_by_indicator(x)) {
        lapply(setNames(indicators(x), indicators(x)), function(z) {
                                # Need to 'as_demog_change_component_df' because many
                                # 'ccmpp_input_df' objects cannot be
                                # subset.
            as_age_time_matrix_list(subset_indicator(as_demog_change_component_df(x),
                                                     z,
                                                     drop = TRUE),
                                    drop_zero_fert_ages = drop_zero_fert_ages, ...)
        })
    } else if (is_by_sex(x)) {
        lapply(setNames(sexes(x), sexes(x)), function(z) {
            as_age_time_matrix_list(subset_sex(as_demog_change_component_df(x), z,
                                               drop = TRUE),
                                    drop_zero_fert_ages = drop_zero_fert_ages, ...)
        })
    } else {
        return(as_age_time_matrix(x, drop_zero_fert_ages = drop_zero_fert_ages, ...))
    }
}

#' @rdname as_age_time_matrix_list
#' @export
as_age_time_matrix_list.ccmpp_input_list <- function(x, drop_zero_fert_ages = FALSE, ...) {
    lapply(x, function(y) {
        as_age_time_matrix_list(y,
                                drop_zero_fert_ages = drop_zero_fert_ages, ...)
        })
}
