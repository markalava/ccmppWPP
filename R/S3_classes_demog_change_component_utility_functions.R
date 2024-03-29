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
#' \code{\link{validate_ccmppWPP_object}}). If it is not valid an error
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
                                # TODO: should hard-code 'time_span'

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
    if (identical(length(unique(x[[time_col_name]])), 1L) && drop) {
        x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions(dimensions = "time", spans = FALSE))]
        attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "time"]
        }

    return(suppressMessages(demog_change_component_df(x,
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
                                # TODO: should hard-code 'age_span'

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
    if (identical(length(unique(x[[age_col_name]])), 1L) && drop) {
        x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions(dimensions = "age", spans = FALSE))]
        attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "age"]
        }

    return(suppressMessages(demog_change_component_df(x,
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
                                # TODO: should hard-code 'sex'

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
        if (identical(length(unique(x[[sex_col_name]])), 1L) && drop) {
            x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions(dimensions = "sex", spans = FALSE))]
            attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "sex"]
            }

        return(suppressMessages(demog_change_component_df(x,
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
                                # TODO: should hard-code 'indicator'

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
        if (identical(length(unique(x[[indicator_col_name]])), 1L) && drop) {
            x <- x[, -which(colnames(x) == get_df_col_names_for_dimensions(dimensions = "indicator", spans = FALSE))]
            attr(x, "dimensions") <- attr(x, "dimensions")[attr(x, "dimensions") != "indicator"]
        }

        return(suppressMessages(demog_change_component_df(x,
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
#' When \code{by_vars_names} is \code{NULL}, if \code{denom} has
#' column \code{time_span} and all entries are 0, \dQuote{time_span}
#' will be omitted from \code{by_vars_names}. \emph{Note:} This only
#' happens if \code{by_vars_names} is \code{NULL}.
#'
#' @param num A data frame with columns \dQuote{\code{by_vars_names}}
#'     and \dQuote{value}, holding the numerator values.
#' @param denom Similar to \code{num} but holding the denominator
#'     values.
#' @param by_vars_names Character vector of names of columns to merge
#'     by. By default, all columns except \dQuote{value}. See also
#'     \dQuote{Details}.
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
        ## Handle 'time_span == 0', eg when 'denom' is a
        ## 'pop_count_age_sex[_base]' object.
        if ("time_span" %in% colnames(denom)) {
            if (all(denom$time_span == 0))
                by_vars_names <- by_vars_names[!by_vars_names == "time_span"]
            }
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
#' Special processing occurs when \code{by_vars_names} is \code{NULL}
#' and either \code{x} or \code{y} has column \code{time_span} and all
#' entries are 0. In this case, before merging, \dQuote{time_span} is
#' omitted from \code{by_vars_names} and the \code{time_span} column
#' is dropped from \code{y}. This ensures the result has only one
#' \code{time_span} column, namely the one from \code{x}.
#'
#' @param x,y Data frames with columns \dQuote{\code{by_vars_names}}
#'     and \dQuote{value}.
#' @inheritParams base::merge
#' @inheritParams make_value_ratio
#' @return A data frame with columns \dQuote{\code{by_vars_names}} and
#'     \dQuote{value}, where the latter holds the product of
#'     \dQuote{value}s in \code{x} and \code{y}.
#' @author Mark Wheldon
#' @export
make_value_product <- function(x, y,
                             by_vars_names = NULL,
                             all.x = TRUE, all.y = FALSE) {
    x <- as.data.frame(x)
    y <- as.data.frame(y)
    if (is.null(by_vars_names)) {
        by_vars_names <- intersect(names(x), names(y))
        by_vars_names <- by_vars_names[!by_vars_names == "value"]
        ## Handle 'time_span == 0', eg when 'x' or 'y' are a
        ## 'pop_count_age_sex[_base]' objects.
        if ("time_span" %in% colnames(x)) {
            if (all(x$time_span == 0))
                by_vars_names <- by_vars_names[!by_vars_names == "time_span"]
        }
        if ("time_span" %in% colnames(y)) {
            if (all(y$time_span == 0)) {
                by_vars_names <- by_vars_names[!by_vars_names == "time_span"]
                ## Also need to remove the 'time_span' column
                ## otherwise it will appear in the result.
                y <- y[, -which(colnames(y) == "time_span")]
            }
        }
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
#' This function conveniently collapses objects across the demographic
#' dimension(s) specified in \code{by_dimensions}. The \dQuote{value}
#' column is aggregated using function \code{FUN}. Columns
#' corresponding to \code{by_dimensions} are dropped. If you want to
#' aggregate other columns, or complete collapsing over a dimension is
#' not desired, use \code{\link[stats]{aggregate}}, or similar.
#'
#' Argument \code{by_dimensions} is a character vector and can be any
#' of the allowed demographic \dQuote{dimensions} such as
#' \dQuote{age}, \dQuote{time}, \dQuote{sex} (you can see the full
#' list by calling \code{ccmppWPP:::get_all_allowed_dimensions}). The
#' result will have values completely collapsed over the given
#' dimension. E.g., if \code{by_dimensions = "age"} then column
#' \code{value} in the result will contain the original \code{value}s
#' aggregated over all remaining dimensions, where aggregation is done
#' by function \code{FUN}.
#'
#' @section Setting the class of the output:
#' Collapsing a \code{demog_change_component_df} object will likely
#' make it an invalid member of its class. By default, the function
#' will attempt to return a
#' \code{\link{demog_change_component_df}}. If this fails a
#' \code{\link{data.frame}} will be returned with a warning.
#'
#' @param x An object inheriting from
#'     \code{demog_change_component_df}.
#' @param by_dimensions A \emph{character vector} (not a list) of
#'     demographic \dQuote{dimensions} to aggregate over; see
#'     \dQuote{Details}).
#' @param out_class The class of the object to be returned
#'     (specifically the first element of the vector returned by
#'     \code{\link{class}}). See \dQuote{Setting the class of the output}.
#' @param FUN A function to use to aggregate the \dQuote{value} column
#'     of \code{x}; found by \code{\link{match.fun}}.
#' @param ... Passed to
#'     \code{\link[stats]{aggregate.data.frame}}. Must not include
#'     arguments named \code{X}, \code{by}, or \code{FUN}.
#' @return A data frame.
#' @author Mark Wheldon
#' @seealso \code{\link[stats]{aggregate}}, \code{link{abridge}}
#'     \code{\link{demog_change_component_df}}
#'
#' @export
collapse_demog_dimension <- function(x, FUN = "sum", ...,
                                     by_dimensions = demog_change_component_dims(x),
                                     collapse_dimensions = NULL,
                                     out_class = c("demog_change_component_df", "data.frame")) {
    ## Check arguments
    FUN <- match.fun(FUN)
    out_class <- match.arg(out_class)
    if (!is.null(collapse_dimensions)) {
        if (!is.character(collapse_dimensions))
            stop("'collapse_dimensions' must be a 'character' vector.")
        by_dimensions <-
            demog_change_component_dims(x)[!demog_change_component_dims(x) %in% collapse_dimensions]
    } else {
        if (!is.character(by_dimensions)) stop("'by_dimensions' must be a 'character' vector.")
        by_dimensions <- match.arg(by_dimensions, several.ok = TRUE)
    }
    by_dimensions <- get_all_req_col_names_excl_spans_for_dimensions(by_dimensions)
    by_dimensions <- by_dimensions[by_dimensions != "value"]

    ## Make sure 'x' can be collapsed: only certain 'value_type's can be collapsed:
    value_type_x <- value_type(x)
    value_scale_x <- value_scale(x)
    if (!value_type_x %in% get_all_aggregatable_value_types())
        stop("'value_type(x)' is '", toString(value_type_x),
             "' but the only aggregatable 'value_type's are '",
             toString(get_all_aggregatable_value_types()), "'.")

    ## Do the collapsing
    out <- stats::aggregate.data.frame(x = x[, "value", drop = FALSE],
                                       by = x[, by_dimensions, drop = FALSE],
                                       FUN = FUN, ...)
    if (identical(out_class, "demog_change_component_df")) {
        tryout <- try(demog_change_component_df(out, value_type = value_type_x, value_scale = value_scale_x),
                      silent = TRUE)
        if (!identical(class(tryout), "try-error")) return(tryout)
        else warning("Returning a 'data.frame'.")
    } else return(out)
}


#' Abridge a \code{demog_change_component_df} object
#'
#' This is a generic function with methods for the various sub-classes
#' of \code{ccmpp_input_df}. It abridges the first argument, \code{x},
#' such that the result has spans or ages and times as supplied in the
#' \code{span_abridged}, \code{age_start_abridged} and
#' \code{time_start_abridged} arguments. See \dQuote{Details} for
#' restrictions on specifying the arguments.
#'
#' If you specify \code{age_span_abridged} you cannot specify
#' \code{age_start_abridged}, and similarly for
#' \code{time_span_abridged} and \code{time_start_abridged}. If you
#' specify only one of \dQuote{age} and \dQuote{time}, all levels of
#' the other are kept if \code{x} has the corresponding demographic
#' dimension. Moreover, abridging is done within levels of \dQuote{sex} and
#' \dQuote{indicators} if \code{x} has those dimensions.
#'
#' This function calls
#' \code{\link{aggregate.data.frame}} to do the actual
#' abridging.
#'
#' If you get an error along the lines of \dQuote{result is invalid as
#' a member of the class} or similar, it means that, after abridging,
#' the result is no longer valid as a member of the class \code{x}
#' has. This would occur, for example, if \code{x} inherits from
#' \code{ccmpp_input_df} and you only supplied one of the
#' \code{..._start_abridged} arguments. Try supplying both or,
#' instead, specify \code{span_abridged}.
#'
#' @param x An object inheriting from
#'     \code{\link{demog_change_component_df}}.
#' @param age_span_abridged A numeric scalar giving the \emph{new}
#'     \code{age_span} values; \code{x} will be abridged such that
#'     it's \code{age_span} is \code{age_span_abridged}.
#' @param time_span_abridged As \code{age_span_abridged} but for time.
#' @param age_start_abridged Vector of \emph{new} \code{age_start}
#'     values; \code{x} will be abridged such that
#'     \code{age_start_abridged} defines the new age groups.
#' @param time_start_abridged As \code{age_start_abridged} but for
#'     time.
#' @param out_class See same argument for
#'     \code{\link{aggregate.demog_change_component_df}}.
#' @param ... Other arguments passed to other methods.
#' @return An object of the same class as \code{x}, abridged.
#' @author Mark Wheldon
#' @name abridge
#' @seealso \code{\link[stats]{aggregate}} which this function relies
#'     on, \code{link{collapse_demog_dimension}} for a related
#'     function.
#' @export
abridge <- function(x, ...) {
    UseMethod("abridge")
}

#' @rdname abridge
#' @export
abridge.demog_change_component_df <- function(x,
                                              age_span_abridged = NULL, time_span_abridged = NULL,
                                   age_start_abridged = NULL, time_start_abridged = NULL,
                                   out_class = class(x)[1], ...) {
    ## Lots of checks:

    ## 'x' must be abridgeable
    if (!value_type(x) %in% get_all_aggregatable_value_types())
        stop("'value_type(x)' is '", toString(value_type(x)),
             "' but the only abridge-able 'value_type's are '", toString(get_all_aggregatable_value_types()), "'.")
    if (!is_by_age(x) && !is_by_time(x))
        stop("'x' is has neither the \"age\" nor \"time\" demographic dimension; it must have at least one of these to be abridged.")

    ## Age arguments
    if (!is.null(age_span_abridged) || !is.null(age_start_abridged)) {
        if (!is_by_age(x))
            stop("'age_start_abridged' or 'age_span_abridged' is non-NULL but 'x' does not have the age demographic dimension.")
        if (!is.null(age_span_abridged) && !is.null(age_start_abridged))
            stop("You can specify 'age_span_abridged' OR 'age_start_abridged' but not both.")
        if (!is.null(age_span_abridged) && (!is.numeric(age_span_abridged) || !identical(length(age_span_abridged), 1L))) {
            stop("'age_span_abridged' must be 'numeric', length 1.")
        } else if (!is.null(age_start_abridged) && !is.numeric(age_start_abridged)) {
            stop("'age_start_abridged' must be 'numeric'.")
        }
    }

    ## Time arguments
    if (!is.null(time_span_abridged) || !is.null(time_start_abridged)) {
        if (!is_by_time(x))
            stop("'time_start_abridged' or 'time_span_abridged' is non-NULL but 'x' does not have the time demographic dimension.")
        if (!is.null(time_span_abridged) && !is.null(time_start_abridged))
            stop("You can specify 'time_span_abridged' OR 'time_start_abridged' but not both.")
        if (!is.null(time_span_abridged) && (!is.numeric(time_span_abridged) || !identical(length(time_span_abridged), 1L))) {
            stop("'time_span_abridged' must be 'numeric', length 1.")
        } else if (!is.null(time_start_abridged) && !is.numeric(time_start_abridged)) {
            stop("'time_start_abridged' must be 'numeric'.")
        }
    }

    ## out_class
    class_x <- class(x)
    if (length(class_x) > 1) {
        i <- match(out_class, class_x)
        if (!is.na(i) && i < length(class_x)) out_class <- c(out_class, tail(class_x, -i))
    }
    if (!all(out_class %in% c(get_all_demog_change_component_df_class_names(), "data.frame")))
        stop("'out_class' must only use classes in this list '",
             toString(c(get_all_demog_change_component_df_class_names(), "data.frame")),
             "'.")

    ## Start abridging ---:

    by_list <- list()

    if (!is.null(age_span_abridged)) {
            ages_x <- ages(x)
            age_start_abridged <-
                seq(from = min(ages_x), to = max(ages_x), by = age_span_abridged)
    }
    if (!is.null(time_span_abridged)) {
            times_x <- times(x)
            time_start_abridged <-
                seq(from = min(times_x), to = max(times_x), by = time_span_abridged)
    }
    if (!is.null(age_start_abridged)) { # either user-supplied or computed above from 'age_span_abridged'
        start_int <- findInterval(x$age_start, age_start_abridged)
        age_start <- age_start_abridged[start_int]
        by_list <- c(by_list, list(age_start = age_start))
    } else if (is_by_age(x)) { # 'age_start_abridged' is null
        ## Keep any age columns already in 'x'
        by_list <- c(by_list, list(age_start = x$age_start))
    }
    if (!is.null(time_start_abridged)) { # either user-supplied or computed from 'time_span_abridged'
        start_int <- findInterval(x$time_start, time_start_abridged)
        time_start <- time_start_abridged[start_int]
        by_list <- c(by_list, list(time_start = time_start))
    } else if (is_by_time(x)) { # 'time_start_abridged' is null
        ## Keep any time columns already in 'x'
        by_list <- c(by_list, list(time_start = x$time_start))
    }

    ## Abridge by sex and indicator if present
    if (is_by_sex(x))
        by_list <- c(by_list, list(sex = x$sex))
    if (is_by_indicator(x))
        by_list <- c(by_list, list(indicator = x$indicator))

    ## Aggregate
    value_type_x <- value_type(x)
    value_scale_x <- value_scale(x)
    out <- stats::aggregate(data.frame(value = x$value), by = by_list, FUN = "sum")

    ## Deal with pop counts and zero time spans

    if (is_by_time(x) && has_time_span_zero(x) && is.null(time_span_abridged) && is.null(time_start_abridged))
        out$time_span <- 0

    ## Check the class
    tryout <- try(do.call(get_as_function_for_class(out_class[1]),
                          list(x = out, value_type = value_type_x, value_scale = value_scale_x)),
                  silent = TRUE)
    if (identical(class(tryout), "try-error") || !identical(class(tryout), out_class)) {
        class_orig <- class_x
        while(identical(class(tryout), "try-error") && length(class_x[-1])) {
            class_x <- class_x[-1]
            tryout <- try(do.call(get_as_function_for_class(class_x[1]),
                                  list(x = out, value_type = value_type_x, value_scale = value_scale_x)),
                          silent = TRUE)
            if (!identical(class(tryout), "try-error")) {
                msg <- paste0("The result of abridging 'x' cannot be coerced to the class in argument 'out_class' (i.e., '", toString(out_class), "').\n\tIf you want to abridge 'x', set 'out_class' to '", class_x[1], "' or coerce it to a '", class_x[1], "' object and use 'abridge' again (see examples in '?abridge').")
                if (is_by_time(x) && has_time_span_zero(x) &&
                    (!is.null(time_span_abridged) || is.null(time_start_abridged)))
                    msg <- c(msg, "\nNote: 'x' inherits from a class with time_span == 0 but time has been abridged over.")
                stop(msg)
            }
        }
        stop("Could not abridge 'x'.")
    } else {
        validate_tryout <- FALSE
        if (!identical(value_type(tryout), value_type_x)) {
            validate_tryout <- TRUE  #have to validate again after setting value type and scale
            value_type(tryout) <- value_type_x
        }
        if (!identical(as.numeric(value_scale(tryout)), as.numeric(value_scale_x))) {
            validate_tryout <- TRUE
            value_scale(tryout) <- value_scale_x
        }
        if (validate_tryout) tryout <- validate_ccmppWPP_object(tryout)

        return(tryout)
    }
}
