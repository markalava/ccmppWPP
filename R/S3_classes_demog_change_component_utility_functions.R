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
    value_scale_x <- value_scale(x)
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
                                                      value_type = value_type_x,
                                                      value_scale = value_scale_x
                                                      )))
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
    value_scale_x <- value_scale(x)
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
                                                      value_type = value_type_x,
                                                      value_scale = value_scale_x)))
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
        value_scale_x <- value_scale(x)
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
                                                          value_type = value_type_x,
                                                          value_scale = value_scale_x)))
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
        value_scale_x <- value_scale(x)
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
                                                          value_type = value_type_x,
                                                          value_scale = value_scale_x)))
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



###-----------------------------------------------------------------------------
### * Transformations

###-----------------------------------------------------------------------------
### ** Collapse by demographic dimension

#' Collapse a \code{demog_change_component_df} object across demographic dimension(s)
#'
#' This is function conveniently collapses objects across the
#' demographic dimensions specified in \code{by_dimension}. The
#' \dQuote{value} column is aggregated using function \code{FUN} and
#' columsn corresponding to \code{by_dimension} are dropped. If you
#' want to aggregate other columns, or complete collapsing over a
#' dimension is not desired, use \code{\link[stats]{aggregate}}, or
#' similar.
#'
#' Argument \code{by_dimension} is a character vector and can be any
#' of the allowed demographic \dQuote{dimensions} such as
#' \dQuote{age}, \dQuote{time}, \dQuote{sex} (you can see the full
#' list by calling \code{ccmppWPP:::get_all_allowed_dimensions}). The
#' result will have values completely collapsed over the given
#' dimension. E.g., if \code{by_dimension = "age"} then column
#' \code{value} in the result will contain the original \code{value}s
#' aggregated over all remaining dimensions, where aggregation is done
#' by function \code{FUN}.
#'
#' The function will try to return an object of the class given in
#' \code{out_class}, which is just \code{\class{x}} by default (see
#' the argument description for how to specify). If a valid object of
#' this class cannot be created from the result an error is signalled.
#'
#' @param x An object inheriting from
#'     \code{demog_change_component_df}.
#' @param by_dimension A \emph{character vector} (not a list) of
#'     demographic \dQuote{dimensions} to aggregate over; see
#'     \dQuote{Details}). Exactly one of \code{by} and
#'     \code{by_dimension} must be specified.
#' @param out_class The first element of the class of \code{x}
#'     \emph{after} aggregation. It will be expanded by adding
#'     elements 2, 3, ... of \code{class(x)}} if \code{out_class} is
#'     an element of \class{x} and not the last element. Otherwise,
#'     \code{out_class} is left as-is.
#' @param FUN A function to use to aggregate the \dQuote{value} column
#'     of \code{x}; found by \code{\link{match.fun}}.
#' @param ... Passed to
#'     \code{\link[stats]{aggregate.data.frame}}. Must not include any
#'     named arguments that would clash with that function.
#' @return A data frame.
#' @author Mark Wheldon
#' @seealso \code{\link[stats]{aggregate}}, \code{link{abridge}}
#'     \code{\link{demog_change_component_df}}
#' @examples
#' x1 <- ccmpp_input_df(expand.grid(age_start = 0:5, time_start = 1950:1954, value = 1),
#'                    value_type = "count")
#' x1_agg <- collapse_demog_dimension(x1, by_dimension = "time")
#' stopifnot("ccmpp_input_df" %in% class(x1_agg))
#'
#'
#' ## When the result is not a valid member of the class:
#'
#' x2 <- subset(x1, age_start != 0)
#' stopifnot(identical(class(x2), "data.frame"))
#'
#' x2 <- demog_change_component_df(x2, value_type = "count", value_scale = 1)
#' # Force an invalid object (do not try this at home!):
#' class(x2) <- c("ccmpp_input_df", "demog_change_component_df", "data.frame")
#' \dontrun{
#' x2_agg <- collapse_demog_dimension(x2, by_dimension = "age") # exits with an error message
#' }
#'
#' # Coerce object
#' x2_dcc <- demog_change_component_df(x2, value_type = "count", value_scale = 1)
#' x2_dcc_agg <- collapse_demog_dimension(x2_dcc, by_dimension = "age") # OK
#' stopifnot(identical(class(x2_dcc_agg), c("demog_change_component_df", "data.frame")))
#'
#' # Specify 'out_class'
#' x2_dcc_agg <- collapse_demog_dimension(x2, by_dimension = "age", out_class = "demog_change_component_df") # OK
#' stopifnot(identical(class(x2_dcc_agg), c("demog_change_component_df", "data.frame")))
#'
#'
#' ## Coercing 'x' to a data.frame.
#'
#' x3 <- as.data.frame(x1) # will issue a warning that class is dropped
#' x3_agg <- collapse_demog_dimension(x3, by_dimension = "time")
#' stopifnot(identical(class(x3_agg, "data.frame"))
#'
#' # Can re-cast as a 'demog_change_component_df'
#' x_agg_dcc <- as_demog_change_component_df(x3_agg)
#'
#' @export
collapse_demog_dimension <- function(x, FUN = "sum", ..., by_dimension = get_all_allowed_dimensions(),
                                     out_class = class(x)[1]) {
    ## Check arguments
    FUN <- match.fun(FUN)
    stopifnot(identical(length(out_class), 1L))
    if (!is.character(by_dimension)) stop("'by_dimension' must be a 'character' vector.")
    by_dimension <- match.arg(by_dimension, several.ok = TRUE)
    by_dimension <- get_all_req_col_names_excl_spans_for_dimensions(by_dimension)
    by_dimension <- by_dimension[by_dimension != "value"]
    class_x <- class(x)
    if (length(class_x) > 1) {
        i <- match(out_class, class_x)
        if (!is.na(i) && i < length(class_x)) out_class <- c(out_class, tail(class_x, -i))
    }
    if (!all(out_class %in% c(get_all_demog_change_component_df_class_names(), "data.frame")))
        stop("'out_class' must only use classes in this list '",
             toString(c(get_all_demog_change_component_df_class_names(), "data.frame")),
             "'.")

    ## Make sure 'x' can be collapsed: only certain 'value_type's can be collapsed:
    value_type_x <- value_type(x)
    value_scale_x <- value_scale(x)
    if (!value_type_x %in% get_all_aggregatable_value_types())
        stop("'value_type(x)' is '", toString(value_type_x),
             "' but the only aggregatable 'value_type's are '",
             toString(get_all_aggregatable_value_types()), "'.")

    ## Do the collapsing
    x <- as.data.frame(x)
    out <- stats::aggregate.data.frame(x = x[, "value", drop = FALSE],
                                       by = x[, by_dimension, drop = FALSE],
                                       FUN = FUN, ...)
    tryout <- try(do.call(get_as_function_for_class(out_class[1]),
                          list(x = out, value_type = value_type_x, value_scale = value_scale_x)),
                  silent = TRUE)
    if (!identical(class(tryout), "try-error") && identical(class(tryout), out_class)) return(tryout)
    else {
        class_orig <- class_x
        while(identical(class(tryout), "try-error") && length(class_x[-1])) {
            class_x <- class_x[-1]
            tryout <- try(do.call(get_as_function_for_class(class_x[1]),
                                  list(x = out, value_type = value_type_x, value_scale = value_scale_x)),
                          silent = TRUE)
            if (!identical(class(tryout), "try-error")) {
                msg <- paste0("The result of collapsing 'x' cannot be coerced to the class in argument 'out_class' (i.e., '", toString(out_class), "').\n\tIf you want to collapse 'x', set 'out_class' to '", class_x[1], "' or coerce it to a '", class_x[1], "' object and use 'collapse_demog_dimension' again (see examples in '?collapse_demog_dimension').")
                stop(msg)
            }
        }
        stop("Could not collapse 'x'. Try coercing it to a 'data.frame' via 'as.data.frame(x)' and re-applying the function on that, or use 'base::aggregate'. Note that the result will not have the same class as 'x' until you re-cast it.")
    }
}


## #' Abridge a \code{demog_change_component_df} object
## #'
## #' This is a generic function with methods for the various sub-classes
## #' of \code{ccmpp_input_df}. It abridges the first argument, \code{x},
## #' such that the result has spans or ages and times as supplied in the
## #' \code{span_abridged}, \code{age_start_abridged} and
## #' \code{time_start_abridged} arguments. See \dQuote{Details} for
## #' restrictions on specifying the arguments.
## #'
## #' If you specify \code{age_span_abridged} you cannot specify
## #' \code{age_start_abridged}, and similarly for
## #' \code{time_span_abridged} and \code{time_start_abridged}. If you
## #' specify only one of \dQuote{age} and \dQuote{time}, all levels of
## #' the other are kept if \code{x} has the corresponding demographic
## #' dimension. Moreover, abridging is done within levels of \dQuote{sex} and
## #' \dQuote{indicators} if \code{x} has those dimensions.
## #'
## #' This function calls
## #' \code{\link{aggregate.demog_change_component_df}} to do the actual
## #' abridging.
## #'
## #' If you get an error along the lines of \dQuote{result is invalid as
## #' a member of the class} or similar, it means that, after abridging,
## #' the result is no longer valid as a member of the class \code{x}
## #' has. This would occur, for example, if \code{x} inherits from
## #' \code{ccmpp_input_df} and you only supplied one of the
## #' \code{..._start_abridged} arguments. Try supplying both or,
## #' instead, specify \code{span_abridged}.
## #'
## #' @param x An object inheriting from
## #'     \code{\link{demog_change_component_df}}.
## #' @param age_span_abridged A numeric scalar giving the \emph{new}
## #'     \code{age_span} values; \code{x} will be abridged such that
## #'     it's \code{age_span} is \code{age_span_abridged}.
## #' @param time_span_abridged As \code{age_span_abridged} but for time.
## #' @param age_start_abridged Vector of \emph{new} \code{age_start}
## #'     values; \code{x} will be abridged such that
## #'     \code{age_start_abridged} defines the new age groups.
## #' @param time_start_abridged As \code{age_start_abridged} but for
## #'     time.
## #' @param out_class See same argument for
## #'     \code{\link{aggregate.demog_change_component_df}}.
## #' @param ... Other arguments passed to other methods.
## #' @return An object of the same class as \code{x}, abridged.
## #' @author Mark Wheldon
## #' @name abridge
## #' @seealso \code{\link[stats]{aggregate}} which this function relies
## #'     on, \code{link{collapse_demog_dimension}} for a related
## #'     function.
## #' @export
## abridge <- function(x, ...) {
##     UseMethod("abridge")
## }

## #' @rdname abridge
## #' @export
## abridge.demog_change_component_df <- function(x,
##                                               age_span_abridged = NULL, time_span_abridged = NULL,
##                                    age_start_abridged = NULL, time_start_abridged = NULL,
##                                    out_class = class(x)[1], ...) {
##     ## Lots of checks:

##     ## 'x' must be abridgeable
##     if (!value_type(x) %in% get_all_aggregatable_value_types())
##         stop("'value_type(x)' is '", toString(value_type(x)),
##              "' but the only abridge-able 'value_type's are '", toString(get_all_aggregatable_value_types()), "'.")
##     if (!is_by_age(x) && !is_by_time(x))
##         stop("'x' is has neither the \"age\" nor \"time\" demographic dimension; it must have at least one of these to be abridged.")

##     ## Age arguments
##     if (!is.null(age_span_abridged) || !is.null(age_start_abridged)) {
##         if (!is_by_age(x))
##             stop("'age_start_abridged' or 'age_span_abridged' is non-NULL but 'x' does not have the age demographic dimension.")
##         if (!is.null(age_span_abridged) && !is.null(age_start_abridged))
##             stop("You can specify 'age_span_abridged' OR 'age_start_abridged' but not both.")
##         if (!is.null(age_span_abridged) && (!is.numeric(age_span_abridged) || !identical(length(age_span_abridged), 1L))) {
##             stop("'age_span_abridged' must be 'numeric', length 1.")
##         } else if (!is.null(age_start_abridged) && !is.numeric(age_start_abridged)) {
##             stop("'age_start_abridged' must be 'numeric'.")
##         }
##     }

##     ## Time arguments
##     if (!is.null(time_span_abridged) || !is.null(time_start_abridged)) {
##         if (!is_by_time(x))
##             stop("'time_start_abridged' or 'time_span_abridged' is non-NULL but 'x' does not have the time demographic dimension.")
##         if (!is.null(time_span_abridged) && !is.null(time_start_abridged))
##             stop("You can specify 'time_span_abridged' OR 'time_start_abridged' but not both.")
##         if (!is.null(time_span_abridged) && (!is.numeric(time_span_abridged) || !identical(length(time_span_abridged), 1L))) {
##             stop("'time_span_abridged' must be 'numeric', length 1.")
##         } else if (!is.null(time_start_abridged) && !is.numeric(time_start_abridged)) {
##             stop("'time_start_abridged' must be 'numeric'.")
##         }
##     }

##     ## out_class
##     class_x <- class(x)
##     if (length(class_x) > 1) {
##         i <- match(out_class, class_x)
##         if (!is.na(i) && i < length(class_x)) out_class <- c(out_class, tail(class_x, -i))
##     }
##     if (!all(out_class %in% c(get_all_demog_change_component_df_class_names(), "data.frame")))
##         stop("'out_class' must only use classes in this list '",
##              toString(c(get_all_demog_change_component_df_class_names(), "data.frame")),
##              "'.")

##     ## Start abridging ---:

##     by_list <- list()

##     if (!is.null(age_span_abridged)) {
##             ages_x <- ages(x)
##             age_start_abridged <-
##                 seq(from = min(ages_x), to = max(ages_x), by = age_span_abridged)
##     }
##     if (!is.null(time_span_abridged)) {
##             times_x <- times(x)
##             time_start_abridged <-
##                 seq(from = min(times_x), to = max(times_x), by = time_span_abridged)
##     }
##     if (!is.null(age_start_abridged)) { # either user-supplied or computed above from 'age_span_abridged'
##         start_int <- findInterval(x$age_start, age_start_abridged)
##         age_start <- age_start_abridged[start_int]
##         by_list <- c(by_list, list(age_start = age_start))
##     } else if (is_by_age(x)) { # 'age_start_abridged' is null
##         ## Keep any age columns already in 'x'
##         by_list <- c(by_list, list(age_start = x$age_start))
##     }
##     if (!is.null(time_start_abridged)) { # either user-supplied or computed from 'time_span_abridged'
##         start_int <- findInterval(x$time_start, time_start_abridged)
##         time_start <- time_start_abridged[start_int]
##         by_list <- c(by_list, list(time_start = time_start))
##     } else if (is_by_time(x)) { # 'time_start_abridged' is null
##         ## Keep any time columns already in 'x'
##         by_list <- c(by_list, list(time_start = x$time_start))
##     }

##     ## Abridge by sex and indicator if present
##     if (is_by_sex(x))
##         by_list <- c(by_list, list(sex = x$sex))
##     if (is_by_indicator(x))
##         by_list <- c(by_list, list(indicator = x$indicator))

##     ## Aggregate
##     value_type_x <- value_type(x)
##     value_scale_x <- value_scale(x)
##     out <- aggregate(data.frame(value = x$value), by = by_list, FUN = "sum")

##     ## Check the class
##     tryout <- try(do.call(get_as_function_for_class(out_class[1]),
##                           list(x = out, value_type = value_type_x, value_scale = value_scale_x)),
##                   silent = TRUE)
##     if (identical(class(tryout), "try-error") || !identical(class(tryout), out_class)) {
##         class_orig <- class_x
##         while(identical(class(tryout), "try-error") && length(class_x[-1])) {
##             class_x <- class_x[-1]
##             tryout <- try(do.call(get_as_function_for_class(class_x[1]),
##                                   list(x = out, value_type = value_type_x, value_scale = value_scale_x)),
##                           silent = TRUE)
##             if (!identical(class(tryout), "try-error")) {
##                 msg <- paste0("The result of abridging 'x' cannot be coerced to the class in argument 'out_class' (i.e., '", toString(out_class), "').\n\tIf you want to abridge 'x', set 'out_class' to '", class_x[1], "' or coerce it to a '", class_x[1], "' object and use 'abridge' again (see examples in '?abridge').")
##                 stop(msg)
##             }
##         }
##         stop("Could not abridge 'x'.")
##     } else {
##         validate_tryout <- FALSE
##         if (!identical(value_type(tryout), value_type_x)) {
##             validate_tryout <- TRUE  #have to validate again after setting value type and scale
##             value_type(tryout) <- value_type_x
##         }
##         if (!identical(as.numeric(value_scale(tryout)), as.numeric(value_scale_x))) {
##             validate_tryout <- TRUE
##             value_scale(tryout) <- value_scale_x
##         }
##         if (validate_tryout) tryout <- validate_ccmpp_object(tryout)

##         return(tryout)
##     }
## }
