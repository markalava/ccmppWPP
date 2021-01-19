###-----------------------------------------------------------------------------
### * Subset by time, age, or sex

#' Subset by time, age, sex, or indicator
#'
#' These functions subset objects inheriting from
#' \code{demog_change_component_df} by one of the four dimensions,
#' indicator, age, time, or sex, and retain the class if valid (see
#' \dQuote{Details}). Subsetting such that only one level of the
#' subset dimension is retained will drop the dimension if \code{drop
#' = TRUE}, otherwise it is retained (default). If the object returned
#' after subsetting is a valid member of the original class it will be
#' returned (valid according to
#' \code{\link{validate_ccmpp_object}}). If it is not valid an error
#' will be signalled and nothing is returned.
#'
#' For \code{subset_times} and \code{subset_ages} rows can be excluded
#' by supplying negative \code{times} or \code{ages}. In
#' that case, all values must be negative and \code{include} must be
#' \code{TRUE} (the default). Violations will result in an error.
#'
#' Argument \code{drop} is not available in the methods for objects
#' inheriting from \code{ccmpp_input_df} because these objects must
#' have (and keep) the set of dimensions specified in their class
#' definition. E.g., \code{fert_rate_input} objects may be subset by
#' time, but the time dimension cannot be dropped because objects of
#' this class must always have a time dimension.
#'
#' @section Note on efficiency:
#' These functions are not particularly efficient. For repeated
#' subsetting within, e.g., a for loop, it is better to use standard
#' subsetting operations on the data frame component and re-cast the
#' result as a classed object at the end if desired.
#'
#' @param x An object to subset.
#' @param indicators,times,ages,sexes Vectors indicating the levels of
#'     time, age, or sex to retain (\code{include = TRUE}) or exclude
#'     (\code{include = FALSE}). \code{ages} and \code{times} are
#'     coerced to numeric via \code{\link{as.numeric}}.
#' @param include Logical; should the rows corresponding to the values
#'     supplied in the previous argument be those that are kept or
#'     discarded?
#' @param drop Logical; should demographic change component dimensions
#'     with only one level be dropped? Not available in all methods;
#'     see \dQuote{Details}.
#' @return The object after subsetting if a valid member of the class;
#'     otherwise an error.
#' @author Mark Wheldon
#' @name subset_demog_change_component_df
#' @family subset_by_demographic_dimension
NULL

#' @rdname subset_demog_change_component_df
#' @export
subset_time <- function(x, times, include = TRUE, ...) {
    UseMethod("subset_time")
}

#' @rdname subset_demog_change_component_df
#' @export
subset_time.demog_change_component_df <- function(x, times, include = TRUE, drop = FALSE) {
    stopifnot(is_by_time(x))
    times <- as.numeric(times)
    stopifnot(is.finite(times))

    if (!identical(abs(sum(sign(times))) + sum(times == 0), as.double(length(times))))
        stop("Either supply all positive or all negative values for 'times'.")
    if (any(times < 0)) {
        if (!include)
            stop("Negative 'times' can only be used with 'include = TRUE'.")
        else {
            include <- FALSE
            times  <- -times
        }
    }

    value_type_x <- value_type(x)
    time_col_name <- get_df_col_names_for_dimensions(dimensions = "time", spans = FALSE)

    if (include) {
        time_x <- x[[time_col_name]] %in% times
        if (identical(sum(time_x), 0L))
            stop("'x' does not have any entries with 'time' %in% '", times, "'. ")
    } else {
        time_x <- !x[[time_col_name]] %in% times
        if (identical(sum(time_x), 0L))
            stop("'x' does not have any entries; you have excluded all rows.")
    }

    x <- x[time_x, ]
    if (identical(length(times), 1L) && drop) {
        x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions(dimensions = "time", spans = FALSE))]
        attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "time"]
        }

    return(suppressMessages(demog_change_component_df(x,
                                                      dimensions = NULL,
                                                      value_type = value_type_x)))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age <- function(x, ages, include = TRUE, ...) {
    UseMethod("subset_age")
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.demog_change_component_df <- function(x, ages, include = TRUE, drop = FALSE) {
    stopifnot(is_by_age(x))
    ages <- as.numeric(ages)
    stopifnot(is.finite(ages))

    if (!identical(abs(sum(sign(ages))) + sum(ages == 0), as.double(length(ages))))
        stop("Either supply all positive or all negative values for 'ages'.")
    if (any(ages < 0)) {
        if (!include)
            stop("Negative 'ages' can only be used with 'include = TRUE'.")
        else {
            include <- FALSE
            ages  <- -ages
        }
    }

    value_type_x <- value_type(x)
    age_col_name <- get_df_col_names_for_dimensions(dimensions = "age", spans = FALSE)

    if (include) {
    age_x <- x[[age_col_name]] %in% ages
    if (identical(sum(age_x), 0L))
        stop("'x' does not have any entries with 'age' %in% '", ages, "'.")
    } else {
    age_x <- !x[[age_col_name]] %in% ages
    if (identical(sum(age_x), 0L))
        stop("'x' does not have any entries; you have excluded all rows.")
    }

    x <- x[age_x, ]
    if (identical(length(ages), 1L) && drop) {
        x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions(dimensions = "age", spans = FALSE))]
        attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "age"]
        }

    return(suppressMessages(demog_change_component_df(x,
                                                      dimensions = NULL,
                                                      value_type = value_type_x)))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex <- function(x, sexes, include = TRUE, ...) {
    UseMethod("subset_sex")
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.demog_change_component_df <-
    function(x, sexes = get_all_allowed_sexes(), include = TRUE, drop = FALSE) {
        stopifnot(is_by_sex(x))
        sexes <- match.arg(sexes, several.ok = TRUE)

        value_type_x <- value_type(x)
        sex_col_name <- get_df_col_names_for_dimensions(dimensions = "sex", spans = FALSE)

        if (include) {
            sex_x <- x[[sex_col_name]] %in% sexes
            if (identical(sum(sex_x), 0L))
                stop("'x' does not have any entries with 'sex' %in% '", sexes, "'.")
        } else {
            sex_x <- !x[[sex_col_name]] %in% sexes
            if (identical(sum(sex_x), 0L))
                stop("'x' does not have any entries; you have excluded all rows.")
        }

        x <- x[sex_x, ]
        if (identical(length(sexes), 1L) && drop) {
            x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions(dimensions = "sex", spans = FALSE))]
            attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "sex"]
            }

        return(suppressMessages(demog_change_component_df(x,
                                                          dimensions = NULL,
                                                          value_type = value_type_x)))
    }

#' @rdname subset_demog_change_component_df
#' @export
subset_indicator <- function(x, indicators, include = TRUE, ...) {
    UseMethod("subset_indicator")
}

#' @rdname subset_demog_change_component_df
#' @export
subset_indicator.demog_change_component_df <-
    function(x, indicators, include = TRUE, drop = FALSE) {
        stopifnot(is_by_indicator(x))

        value_type_x <- value_type(x)
        indicator_col_name <- get_df_col_names_for_dimensions(dimensions = "indicator", spans = FALSE)

        if (include) {
            indicator_x <- x[[indicator_col_name]] %in% indicators
            if (identical(sum(indicator_x), 0L))
                stop("'x' does not have any entries with 'indicator' %in% '", indicators, "'.")
        } else {
            indicator_x <- !x[[indicator_col_name]] %in% indicators
            if (identical(sum(indicator_x), 0L))
                stop("'x' does not have any entries; you have excluded all rows.")
        }

        x <- x[indicator_x, ]
        if (identical(length(indicators), 1L) && drop) {
            x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions(dimensions = "indicator", spans = FALSE))]
            attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "indicator"]
        }

        return(suppressMessages(demog_change_component_df(x,
                                                          dimensions = NULL,
                                                          value_type = value_type_x)))
    }

###-----------------------------------------------------------------------------
### * Calculation conveniences

#' Calculate the ratio of values in two data frames
#'
#' The ratio of the \dQuote{value} columns in the numerator
#' (\code{num}) and denominator (\code{denom}) data frames is
#' calculated and returned. \code{num} and \code{denom} are first
#' merged on columns named \code{by_vars_names} and these columns are
#' in the output, which is also a data frame.
#'
#' @param num A data frame with columns \dQuote{\code{by_vars_names}}
#'     and \dQuote{value}, holding the numerator values.
#' @param denom Similar to \code{num} but holding the denominator
#'     values.
#' @param by_vars_names Character vector of names of columns to merge
#'     by. By default, all columns except \dQuote{value}.
#' @inheritParams base::merge
#' @return A data frame with columns \dQuote{\code{by_vars_names}} and
#'     \dQuote{value}, where the latter holds the ratio of
#'     \dQuote{value}s in \code{num} and \code{denom}.
#' @author Mark Wheldon
#' @export
make_value_ratio <- function(num, denom,
                             by_vars_names = NULL,
                             all.x = TRUE, all.y = FALSE) {
    num <- as.data.frame(num)
    denom <- as.data.frame(denom)
    if (is.null(by_vars_names)) {
        by_vars_names <- intersect(names(num), names(denom))
        by_vars_names <- by_vars_names[!by_vars_names == "value"]
    }
    if (!length(by_vars_names))
        stop("'No 'by_vars' variables in common.")

    if (!is.null(attr(num, "value_type")) &&
        !is.null(attr(denom, "value_type"))) {
        if (!identical(as.numeric(value_type(num)),
                       as.numeric(value_type(denom))))
            stop("'value_type's are different for 'num' and 'denom'.")
    }

    if (!is.null(attr(num, "value_scale")) &&
        !is.null(attr(denom, "value_scale"))) {
        if (!identical(as.numeric(value_scale(num)),
                       as.numeric(value_scale(denom))))
            stop("'value_scale's are different for 'num' and 'denom'.")
    }

    x <- base::merge(num, denom, by = by_vars_names,
                     all.x = all.x, all.y = all.y,
                     sort = FALSE, suffixes = c(".num", ".denom"))
    x$value <- x$value.num / x$value.denom
    x <- x[, -which(names(x) %in% c("value.num", "value.denom"))]
    return(x)
}


#' Calculate the product of values in two data frames
#'
#' The product of the \dQuote{value} columns in data frames \code{x}
#' and \code{y} is calculated and returned. \code{x} and
#' \code{y} are first merged on columns named \code{by_vars_names}
#' and these columns are in the output, which is also a data frame.
#'
#' @param x,y Data frames with columns \dQuote{\code{by_vars_names}}
#'     and \dQuote{value}.
#' @param by_vars_names Character vector of names of columns to merge
#'     by. By default, all columns except \dQuote{value}.
#' @inheritParams base::merge
#' @return A data frame with columns \dQuote{\code{by_vars_names}} and
#'     \dQuote{value}, where the latter holds the product of
#'     \dQuote{value}s in \code{x} and \code{y}.
#' @author Mark Wheldon
#' @export
make_value_product <- function(x, y,
                             by_vars_names = NULL,
                             all.x = TRUE, all.y = FALSE) {
    num <- as.data.frame(x)
    y <- as.data.frame(y)
    if (is.null(by_vars_names)) {
        by_vars_names <- intersect(names(x), names(y))
        by_vars_names <- by_vars_names[!by_vars_names == "value"]
    }
    if (!length(by_vars_names))
        stop("'No 'by_vars' variables in common.")

    if (!is.null(attr(x, "value_scale")) && !is.na(attr(x, "value_scale")) &&
        !is.null(attr(y, "value_scale")) && !is.na(attr(y, "value_scale"))) {
        if (!identical(as.numeric(value_scale(x)),
                       as.numeric(value_scale(y))))
            stop("'value_scale's are different for 'x' and 'y'.")
    }

    x <- base::merge(x, y, by = by_vars_names,
                     all.x = all.x, all.y = all.y,
                     sort = FALSE, suffixes = c(".x", ".y"))
    x$value <- x$value.x * x$value.y
    x <- x[, -which(names(x) %in% c("value.x", "value.y"))]
    return(x)
}
