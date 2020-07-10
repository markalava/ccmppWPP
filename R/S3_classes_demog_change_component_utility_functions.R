###-----------------------------------------------------------------------------
### * Subset by time, age, or sex

#' Subset by time, age, or sex
#'
#' These functions subset \code{demog_change_component_df}s by one of
#' the three dimensions, age, time, or sex. Subsetting such that only
#' one level of the subset dimension is retained will drop the
#' dimension if \code{drop = TRUE}, otherwise it is retained (default).
#'
#' @param x An object to subset.
#' @param time,age,sex Vectors indicating the levels of time, age, or
#'     sex to retain.
#' @param drop Logical; should demographic change component dimensions
#'     with only one level be dropped?
#' @return The object after subsetting.
#' @author Mark Wheldon
#' @name subset_demog_change_component_df
NULL

#' @rdname subset_demog_change_component_df
#' @export
subset_time <- function(x, ...) {
    UseMethod("subset_time")
}

#' @rdname subset_demog_change_component_df
#' @export
subset_time.demog_change_component_df <- function(x, time, drop = FALSE) {
    stopifnot(is_by_time(x))
    stopifnot(is.finite(as.numeric(time)))

    ## save attributes now
    if (is_by_time(x))
        time_span_x <- time_span(x)
    else time_span_x <- NULL
    if (is_by_age(x))
        age_span_x <- age_span(x)
    else age_span_x <- NULL
    value_type_x <- value_type(x)

    time_col_name <- get_df_col_names_excl_spans_for_dimensions("time")
    time_x <- x[[time_col_name]] %in% time
    if (identical(sum(time_x), 0L))
        stop("'x' does not have any entries with 'time' %in% '", time, "'.")

    x <- x[time_x, ]
    if (identical(length(time), 1L) && drop) {
        x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions("time"))]
        attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "time"]
        }

    return(suppressMessages(demog_change_component_df(x,
                                                      dimensions = NULL,
                                                      value_type = value_type_x)))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age <- function(x, ...) {
    UseMethod("subset_age")
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.demog_change_component_df <- function(x, age, drop = FALSE) {
    stopifnot(is_by_age(x))
    stopifnot(is.finite(as.numeric(age)))

    ## save attributes now
    if (is_by_time(x))
        time_span_x <- time_span(x)
    else time_span_x <- NULL
    if (is_by_age(x))
        age_span_x <- age_span(x)
    else age_span_x <- NULL
    value_type_x <- value_type(x)

    age_col_name <- get_df_col_names_excl_spans_for_dimensions("age")
    age_x <- x[[age_col_name]] %in% age
    if (identical(sum(age_x), 0L))
        stop("'x' does not have any entries with 'age' %in% '", age, "'.")

    x <- x[age_x, ]
    if (identical(length(age), 1L) && drop) {
        x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions("age"))]
        attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "age"]
        }

    return(suppressMessages(demog_change_component_df(x,
                                                      dimensions = NULL,
                                                      value_type = value_type_x)))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex <- function(x, ...) {
    UseMethod("subset_sex")
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.demog_change_component_df <-
    function(x, sex = get_all_allowed_sexes(), drop = FALSE) {
        stopifnot(is_by_sex(x))
        sex <- match.arg(sex, several.ok = TRUE)

        ## save attributes now
        if (is_by_time(x))
            time_span_x <- time_span(x)
        else time_span_x <- NULL
        if (is_by_age(x))
            age_span_x <- age_span(x)
        else age_span_x <- NULL
        value_type_x <- value_type(x)

        sex_col_name <- get_df_col_names_excl_spans_for_dimensions("sex")
        sex_x <- x[[sex_col_name]] %in% sex
        if (identical(sum(sex_x), 0L))
            stop("'x' does not have any entries with 'sex' %in% '", sex, "'.")

        x <- x[sex_x, ]
        if (identical(length(sex), 1L) && drop) {
            x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions("sex"))]
            attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "sex"]
            }

        return(suppressMessages(demog_change_component_df(x,
                                                          dimensions = NULL,
                                                          value_type = value_type_x)))
    }

#' @rdname subset_demog_change_component_df
#' @export
subset_indicator <- function(x, ...) {
    UseMethod("subset_indicator")
}

#' @rdname subset_demog_change_component_df
#' @export
subset_indicator.demog_change_component_df <-
    function(x, indicator = get_all_allowed_indicatores(), drop = FALSE) {
        stopifnot(is_by_indicator(x))
        indicator <- match.arg(indicator, several.ok = TRUE)

        ## save attributes now
        if (is_by_time(x))
            time_span_x <- time_span(x)
        else time_span_x <- NULL
        if (is_by_age(x))
            age_span_x <- age_span(x)
        else age_span_x <- NULL
        value_type_x <- value_type(x)

        indicator_col_name <- get_df_col_names_excl_spans_for_dimensions("indicator")
        indicator_x <- x[[indicator_col_name]] %in% indicator
        if (identical(sum(indicator_x), 0L))
            stop("'x' does not have any entries with 'indicator' %in% '", indicator, "'.")

        x <- x[indicator_x, ]
        if (identical(length(indicator), 1L) && drop) {
            x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions("indicator"))]
            attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "indicator"]
            }

        return(suppressMessages(demog_change_component_df(x,
                                                          dimensions = NULL,
                                                          value_type = value_type_x)))
    }


###-----------------------------------------------------------------------------
### * Age-time matrices

#' Coerce a \code{demog_change_component_df} to \code{matrix}
#'
#' An age * time matrix will be created from the input object,
#' \code{x}, which must not have a \dQuote{sex} dimension. To create a
#' matrix from such an object, first remove the \dQuote{sex} dimension
#' with \code{\link{subset_sex}}.
#'
#' @param x An object of class \code{demog_change_component_df}.
#' @return A \code{list}.
#' @author Mark Wheldon
#' @name as_age_time_matrix
NULL

#' @rdname as_age_time_matrix
#' @export
as_age_time_matrix <- function(x) {
    UseMethod("as_age_time_matrix")
}

#' @rdname as_age_time_matrix
#' @export
as_age_time_matrix.demog_change_component_df <- function(x) {

    if (is_by_sex(x))
        stop("'x' has dimension \"sex\"; select a single sex using 'subset_sex(..., drop = TRUE)' to create an age-time matrix.")

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
