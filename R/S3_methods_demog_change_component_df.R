###
### Methods for *existing* generics. See '..._utility_functions.R' for new generics and methods.
###

###-----------------------------------------------------------------------------
### * Fundamentals

#' Extract or Replace Parts of a \code{demog_change_component_df}
#'
#' These are methods for the subset operators and their companion
#' replacement functions for objects of class
#' \code{demog_change_component_df}. The return values differ and,
#' importantly, subsetting and subset-replacement do not return objects of class
#' \code{demog_change_component_df} (see \dQuote{Details}).
#'
#' Arbitrary subsets or modifications of
#' \code{demog_change_component_df}s (and objects of subclasses) may
#' not be valid members of the class. Therefore, the object resulting
#' from a subset operation via \code{`[`}, or a replacement via
#' \code{`[<-`}, and similar, will no longer inherit from class
#' \code{demog_change_component_df} (or subclasses) and the attributes
#' specific to those classes will be lost.
#'
#' To create a \code{demog_change_component_df} object from a subset
#' call \code{\link{demog_change_component_df}} explicitly (e.g.,
#' \code{demog_change_component_df(x[x$time_start == 1960,])}). A
#' similar approach can be taken for subset-replacement. For an
#' alternative approach to simple subsetting on the demographic change
#' component dimensions see \code{\link{subset_time}} and friends.
#'
#' @seealso \code{\link{demog_change_component_df}} for class
#'     definitions, \code{link{subset_time}} and friends.
#'
#' @inheritParams base::`[.data.frame`
#' @inheritParams base::`[<-.data.frame`
#' @return A \code{data.frame},
#'     one-dimensional vector, or scalar depending on the dimensions
#'     of the extracted values.
#' @author Mark Wheldon
#'
#' @name subset_replace
NULL


#' @rdname subset_replace
#' @export
`[.demog_change_component_df` <- function(x, i, j, drop) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Subsetting a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. See '?subset_time' and friends for an alternative approach.")
    }
    x <- NextMethod()
    if(is_demog_change_component_df(x)) return(as.data.frame(x))
                                # 'NextMethod()' will preseve the
                                # 'demog_change_component_df' class so the
                                # 'demog_change_component_df' method for
                                # 'as.data.frame' will be called. This
                                # will then produce a simple data
                                # frame with no extra attributes.
    else return(x)
}


#' @rdname subset_replace
#' @export
`[<-.demog_change_component_df` <- function(x, i, j, value) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Replacing elements in a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if(is_demog_change_component_df(x)) return(as.data.frame(x))
                                # 'NextMethod()' will preseve the
                                # 'demog_change_component_df' class so the
                                # 'demog_change_component_df' method for
                                # 'as.data.frame' will be called. This
                                # will then produce a simple data
                                # frame with no extra attributes.
    else return(x)
    }


#' @rdname subset_replace
#' @export
`$<-.demog_change_component_df` <- function(x, name, value) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Replacing elements in a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if(is_demog_change_component_df(x)) return(as.data.frame(x))
                                # 'NextMethod()' will preseve the
                                # 'demog_change_component_df' class so the
                                # 'demog_change_component_df' method for
                                # 'as.data.frame' will be called. This
                                # will then produce a simple data
                                # frame with no extra attributes.
    else return(x)
    }


#' @rdname subset_replace
#' @export
`[[<-.demog_change_component_df` <- function(x, i, j, value) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Replacing elements in a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if(is_demog_change_component_df(x)) return(as.data.frame(x))
                                # 'NextMethod()' will preseve the
                                # 'demog_change_component_df' class so the
                                # 'demog_change_component_df' method for
                                # 'as.data.frame' will be called. This
                                # will then produce a simple data
                                # frame with no extra attributes.
    else return(x)
    }

###-----------------------------------------------------------------------------
### * Coercion

## Coercion removes the class and all attributes. Note that the
## default 'as.matrix' will remove them; a method is not required.

#' Coerce a \code{demog_change_component_df}.
#'
#' The class-specific attributes will be lost.
#'
#' @param x An object of class \code{demog_change_component_df}.
#' @return A coerced object.
#' @author Mark Wheldon
#' @name generic_coerce_demog_change_component_df
NULL

#' @rdname generic_coerce_demog_change_component_df
#' @export
as.data.frame.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("The result of the coercion will not inherit from class '",
                oldClass(x)[1],
                "' and will not have any attributes specific to that class.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x))
        oldClass(x) <- strip_demog_change_component_df_classes_attribute(oldClass(x))
    for (a in get_all_allowed_attributes()) {
        attr(x, a) <- NULL
    }
        return(x)
}

###-----------------------------------------------------------------------------
### * Combine via c, rbind, etc.

#' Combine \code{demog_change_component_df} objects
#'
#' This is an S3 method for \code{\link{base::rbind}} for objects that
#' inherit from \code{demog_change_component_df}. The special classes
#' are dropped. If called interactively a warning to this effect is
#' issued.
#'
#' @inheritParams base::rbind.data.frame
#' @return A data frame.
#' @author Mark Wheldon
#' @export
rbind.demog_change_component_df <-
    function(..., deparse.level = 1, make.row.names = TRUE,
             stringsAsFactors = default.stringsAsFactors(),
             factor.exclude = TRUE) {
        ldots <- list(...)
        if (!length(ldots)) NextMethod(generic = "rbind")

        l1 <- ldots[[1]]
        oclass <- oldClass(l1)
        oclass_l <- length(oclass)
        x <- NextMethod(generic = "rbind")
        if (is_demog_change_component_df(x)) {
            if (identical(parent.frame(), .GlobalEnv)) {
                S3_class_warning("The result of the rbind will not inherit from class '",
                        oclass[1],
                        "' and will not have any attributes specific to that class.")
            }
            oldClass(x) <- strip_demog_change_component_df_classes_attribute(oldClass(x))
    for (a in get_all_allowed_attributes()) {
        attr(x, a) <- NULL
    }
            return(x)
        } else return(x)
    }

###-----------------------------------------------------------------------------
### * Print, Summary, and Friends

#' Print Values of a \code{demog_change_component_df}
#'
#' This is a method for the generic \code{\link{base::print}}
#' function. Only the first \code{n} rows are printed for convenience
#' (by default). If all rows are desired use \code{as.data.frame(x)}
#' or see the definition of argument \code{n}.
#'
#' Printed output consists
#' of some text, \code{"info"}, and the first \code{n} lines of the
#' data, \code{"table"}. What gets printed is controlled by argument
#' \code{print_what}.
#'
#' @inheritParams base::print.data.frame
#' @inheritParams base::print.table
#' @inheritParams base::print.default
#'
#' @param x An object of class \code{demog_change_component_df}.
#' @param n Integer controlling how many rows of \code{x} are
#'     printed. Passed to \code{link{head}}; see the documentation of
#'     that function for valid values and associated behaviours.
#' @param print_what What should be printed?
#' @author Mark Wheldon
#' @export
print.demog_change_component_df <-
    function(x, ..., n = min(6L, nrow(x)), digits = NULL,
             quote = FALSE, right = TRUE, row.names = FALSE,
             na.print = ".",
             print_what = c("info", "table")) {

        print_what <- match.arg(print_what, several.ok = TRUE)

        if ("info" %in% print_what) {

            attr_names <- names(demog_change_component_attributes(x))
            attr_names <- attr_names[!(attr_names == "dimensions")]
            if (inherits(x, "fert_rate_age_f"))
                attr_names <- attr_names[!(attr_names == "non_zero_fert_ages")]
            attr_msgs <- sapply(attr_names, function(z) {
                att_z <- attr(x, z)
                att_z_l <- length(att_z)
                if (att_z_l) {
                    if (att_z_l > 3)
                        pr_att_z <- paste0(paste(attr(x, z)[1:3], collapse = ", "),
                                           "...")
                    else pr_att_z <- paste(attr(x, z), collapse = ", ")
                    paste0(z, " = '", pr_att_z, "'")
                }
            })
            attr_msgs <- paste(attr_msgs[!sapply(attr_msgs, "is.null")], collapse = ", ")

            cat_msg <- paste0("# A '", class(x)[1], "' object with ", format(nrow(x), big.mark = ","),
                              " rows.",
                              "\n# dimensions = '", paste(demog_change_component_dims(x), collapse = ", "), "'.",
                              "\n# ", attr_msgs,
                              ".\n")
            if (inherits(x, "fert_rate_age_f"))
                cat_msg <- paste0(cat_msg, paste0("# non_zero_fert_ages = '",
                                                  print_non_zero_fert_ages(attr(x, "non_zero_fert_ages"), width = 20),
                                                  "'.\n"))
            if (is_by_sex(x))
                cat_msg <- paste(cat_msg, "# 'sex' has levels: ", toString(sexes(x)),
                                 ".\n",
                                 sep = "")

            if (is_by_indicator(x))
                cat_msg <- paste(cat_msg, "# 'indicator' has levels: ", toString(indicators(x), width = 40),
                                 ".\n",
                                 sep = "")

            if (identical(value_type(x), "categorical"))
                cat_msg <- paste(cat_msg, "# 'value' has levels: ", toString(values(x), width = 40),
                                 ".\n",
                                 sep = "")

            cat(cat_msg)

        }

        if ("table" %in% print_what) {
            y <- x[seq_len(n),]
            y[is.na(y)] <- na.print
            print(y,
                  digits = digits, quote = quote, na.print = ".",
                        right = right, row.names = row.names,
                        ...)
            cat("# ... etc.\n")
        }
        return(invisible(x))
    }


#' Summarize a \code{demog_change_component_df}
#'
#' A method for the generic \code{\link{summary}} function for objects
#' of class \code{demog_change_component_df}}. Summary statistics are returned invisibly in a named list.
#'
#' @inheritParams base::summary
#'
#' @param object An object of class \code{demog_change_component_df}.
#' @return A list with elements \code{time}, \code{age}, \code{sex}, and \code{table}.
#' @author Mark Wheldon
#' @name summary_demog_change_component_df
#' @export
summary.demog_change_component_df <-
    function(object, maxsum = 7,
             digits = max(3, getOption("digits") - 3), vsep, ...) {

        ## Time
        if (is_by_time(object)) {
            time <- list(range = c("time_start" = min(object$time_start),
                                   "time_end" = max(object$time_start)),
                         span = if ("time_span" %in% names(attributes(object))) { time_span(object) } else NULL)
        } else {
            time <- NULL
        }

        ## Age
        if (is_by_age(object)) {
            age <- list(range = c("age_start" = min(object$age_start),
                                  "age_end" = max(object$age_start)),
                        span = if ("age_span" %in% names(attributes(object))) { age_span(object) } else NULL)
        } else {
            age <- NULL
        }

        ## Sex
        if (is_by_sex(object)) {
            sex <- list(levels = levels(as.factor(object$sex)))
        } else {
            sex <- NULL
        }

        ## Indicator
        if (is_by_indicator(object)) {
            indicator <- list(levels = levels(as.factor(object$indicator)))
        } else {
            indicator <- NULL
        }

        ## Values
        if (identical("categorical", value_type(object))) {
            values <- list(levels = levels(as.factor(object$value)))
        } else {
            values <- NULL
        }

        ## Stats
        table <- NextMethod()

        return(structure(list(dimensions = demog_change_component_dims(object),
                              time = time, age = age, sex = sex, indicator = indicator,
                              value_type = value_type(object), values = values,
                              value_scale = value_scale(object), table = table),
                         class = c("summary_demog_change_component_df", "list")))
    }


#' Print a summary of a \code{demog_change_component_df}
#'
#' A method for the S3 generic \code{\link{base::summary}} for
#' objects of class \code{summary_demog_change_component_df}, as
#' returned by \code{\link{summary.demog_change_component_df}.
#'
#' @param x An object of class \code{demog_change_component_df}.
#' @param ... Currently not used.
#' @return Called for its side-effect.
#' @author Mark Wheldon
#' @name print_demog_change_component_df
#'
#' @seealso \code{\link{summary.demog_change_component_df}}.
#'
#' @export
print.summary_demog_change_component_df <-
    function(x, vsep, ..., print_what = c("info", "table")) {

        print_what <- match.arg(print_what, several.ok = TRUE)

        if (missing(vsep))
            vsep <- strrep("-", 0.75 * getOption("width"))

        msg <- character(0)

        if ("info" %in% print_what) {

            ## Dimensions
            msg <- paste0(msg, "dimensions:  ",
                          paste(x$dimensions,
                                collapse = ", "),
                          "\n")

            ## Time
            if (!is.null(x$time)) {
                msg <- paste0(msg, "       time:  range = [",
                              paste(x$time$range, collapse = ", "),
                              "]")
                if (!is.null(x$time$span))
                    msg <- paste0(msg,
                                  "\tspan = ",
                                  toString(x$time$span, 7))
                msg <- paste0(msg, "\n")
            }

            ## Age
            if (!is.null(x$age)) {
                msg <- paste0(msg, "        age:  range = [",
                              paste(x$age$range, collapse = ", "),
                              "]")
                if (!is.null(x$age$span))
                    msg <- paste0(msg,
                                  "\tspan = ",
                                  toString(x$age$span, 7))
                msg <- paste0(msg, "\n")
            }

            ## Sex
            if (!is.null(x$sex))
                msg <- paste0(msg, "        sex:  levels = ",
                              paste(x$sex$levels, collapse = ", "),
                              "\n")

            ## Indicator
            if (!is.null(x$indicator))
                msg <- paste0(msg, "  indicator:  levels = ",
                              toString(x$indicator$levels, 50),
                              "\n")

            ## Values
            if (!is.null(x$values)) {
                msg <- paste0(msg, " value_type:  ",
                              x$value_type,
                              "\tlevels = ", toString(x$values$levels, 20),
                              "\n")
            } else {
                msg <- paste0(msg, " value_type:  ",
                              x$value_type,
                              "\n")
            }

            ## Value_Scale
            if (!is.na(x$value_scale))
                msg <- paste0(msg, "value_scale:  ",
                              print(x$value_scale),
                              "\n")

            ## Print
            if (length(msg > 0))
                msg <- paste0(msg, vsep, "\n")

        }

        if ("table" %in% print_what) {

            cat(msg, "table:\n", sep = "")
            print(x$table)

        } else {
            cat(msg)
        }

        return(invisible(x))
    }


###-----------------------------------------------------------------------------
### * Plot

#' Plot \code{demog_change_component_df} objects
#'
#' This is the S3 method for the generic \code{\link{plot}}
#' function. It requires \pkg{ggplot2}.
#'
#' @param x An object of class \code{demog_change_component_df}.
#' @param type The type of plot to produce, e.g., \dQuote{line}.
#' @param ... When \code{type} is \dQuote{ggplot2} (default) passed on
#'     to \code{\link[ggplot2]{ggplot}}. Otherwise passed on to \code{\link{plot}}.
#' @param plot Should the plot be printed?
#' @return When \code{framework} is \dQuote{ggplot2}, a plot
#'     object. Otherwise nothing. The function is mainly called for
#'     the side effect of producing a plot.
#' @author Mark Wheldon
#' @export
plot.demog_change_component_df <-
    function(x, type = c("point", "line", "both"), ...,
             plot = TRUE
             ) {
        type <- match.arg(type)
        dcc_dims_x <- demog_change_component_dims(x)

        if ("non_zero_fert_ages" %in% names(attributes(x))) {
            nzf_ages_present <- TRUE
                    x$fert_rate_ages <-
                        factor(x$age_start %in% non_zero_fert_ages(x),
                               levels = c(TRUE, FALSE),
                               labels = c("Non-zero fertility", "Zero fertility"))
        } else nzf_ages_present <- FALSE

        if (identical("age", dcc_dims_x)) {
            gp <- ggplot2::ggplot(data = x, ggplot2::aes(x = age_start, y = value),
                                  ...)

        } else if (identical("time", dcc_dims_x)) {
            gp <- ggplot2::ggplot(data = x, ggplot2::aes(x = time_start, y = value),
                                  ...)

        } else if (identical("sex", dcc_dims_x)) {
            gp <- ggplot2::ggplot(data = x, ggplot2::aes(x = sex, y = value),
                                  ...)

        } else if (identical(sort(c("age", "sex")), sort(dcc_dims_x))) {
            gp <- ggplot2::ggplot(data = x, ggplot2::aes(x = age_start, y = value, col = sex),
                                  ...)

        } else if (identical(sort(c("time", "sex")), sort(dcc_dims_x))) {
            gp <- ggplot2::ggplot(data = x, ggplot2::aes(x = time_start, y = value, col = sex),
                                  ...)

        } else if (identical(sort(c("time", "age")), sort(dcc_dims_x))) {
            gp <- ggplot2::ggplot(data = x, ggplot2::aes(x = age_start, y = value),
                                  ...) +
                ggplot2::facet_wrap(~ time_start)

        } else if (identical(sort(c("time", "sex", "age")), sort(dcc_dims_x))) {
            gp <- ggplot2::ggplot(data = x, ggplot2::aes(x = age_start, y = value, col = sex),
                                  ...) +
                ggplot2::facet_wrap(~ time_start)
        } else stop("These dimensions not yet implemented.")

        if (!identical("sex", dcc_dims_x)) {
            if (identical(type, "line"))
                gp <- gp + ggplot2::geom_line()
            else if (identical(type, "point")) {
                if (nzf_ages_present) {
                    gp <- gp + ggplot2::geom_point(aes(shape = fert_rate_ages, col = fert_rate_ages)) +
                        ggplot2::scale_shape_manual(name = "Reproductive\nage range",
                                                    values = c("Non-zero fertility" = 19,
                                                               "Zero fertility" = 1)) +
                        ggplot2::scale_colour_manual(name = "Reproductive\nage range",
                                                    values = c("Non-zero fertility" = "black",
                                                               "Zero fertility" = "grey"))
                } else {
                    gp <- gp + ggplot2::geom_point()
                }
            } else if (identical(type, "both"))
                if (nzf_ages_present) {
                    gp <- gp + ggplot2::geom_point(aes(shape = fert_rate_ages, col = fert_rate_ages)) +
                        ggplot2::geom_line(aes(col = fert_rate_ages)) +
                        ggplot2::scale_shape_manual(name = "Reproductive age range",
                                                    values = c("Non-zero fertility" = 19,
                                                               "Zero fertility" = 1)) +
                        ggplot2::scale_colour_manual(name = "Reproductive\nage range",
                                                    values = c("Non-zero fertility" = "black",
                                                               "Zero fertility" = "grey"))
                } else {
                    gp <- gp + ggplot2::geom_point() + ggplot2::geom_line()
                }
        } else {
            gp <- gp + ggplot2::geom_bar()
        }
        if (plot) print(gp)
        return(invisible(gp))
    }


###-----------------------------------------------------------------------------
### * Transformations

#' Aggregate a \code{demog_change_component_df}
#'
#' This is a method for the generic \code{\link[stats]{aggregate}}
#' function for objects inheriting from
#' \code{demog_change_component_df} which conveniently aggregates
#' objects by their demographic dimensions (if \code{by_dimension} is
#' specified) or an arbitrary list (if \code{by} is specified). The
#' \dQuote{value} column is aggregated using function
#' \code{FUN}. You cannot aggregate any other columns of \code{x} with
#' this method; coerce \code{x} to a data frame via
#' \code{\link{as.data.frame}} and use the data frame method if you
#' want to aggregate other columns.
#'
#' Argument \code{by_dimension} is a character vector and can be any
#' of the allowed demographic \dQuote{dimensions} such as
#' \dQuote{age}, \dQuote{time}, \dQuote{sex} (you can see the full
#' list by calling \code{ccmppWPP:::get_all_allowed_dimensions}). The
#' result will have values completely collapsed over the given
#' dimension. E.g., if \code{by_dimension = "age"} then column
#' \code{value} in the result will contain the original \code{value}s
#' aggregated over all remaining dimensions, where aggregation is done
#' by fucnction \code{FUN}.
#'
#' If complete collapsing over a dimension is not desired, use the
#' \code{by} argument instead and supply a \emph{list} of the groups
#' to aggregate within. In this case, operation proceeds as with the
#' \code{data.frame} method (see
#' \code{\link[stats]{aggregate.data.frame}}). Note, though, as
#' mentioned above, only the \code{value} column will be
#' aggregated. Coerce \code{x} to a \code{data.frame} to aggregate any
#' of its other columns.
#'
#' The function will try to return an object of the class given in
#' \code{out_class}, which is just \class{x} by default. If a valid
#' object of this class cannot be created from the result, a warning
#' is given (in interactive sessions).
#'
#' @param x An object inheriting from
#'     \code{demog_change_component_df}.
#' @param by A list defining the groups to aggregate over. See the
#'     same argument for \code{\link[stats]{aggregate}} for details
#'     for how to specify this argument. Exactly one of \code{by} and
#'     \code{by_dimension} must be specified.
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
#'     of \code{x}.
#' @param ... Passed to
#'     \code{\link[stats]{aggregate.data.frame}}. Must not include
#'     arguments named \dQuote{\code{x}}, \dQuote{\code{by}},
#'     \dQuote{\code{FUN}}, or \dQuote{\code{out_class}}.
#' @return A data frame with the column \dQuote{value} after
#'     aggregation by \code{by} (see \dQuote{Description}).
#' @author Mark Wheldon
#' @seealso \code{\link[stats]{aggregate}}
#'     \code{\link{demog_change_component_df}}
#' @examples
#' x1 <- ccmpp_input_df(expand.grid(age_start = 0:5, time_start = 1950:1954, value = 1),
#'                    value_type = "count")
#' x1_agg <- aggregate(x1, by_dimension = "time")
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
#' x2_agg <- aggregate(x2, by_dimension = "age") # exits with an error message
#' }
#'
#' # Coerce object
#' x2_dcc <- demog_change_component_df(x2, value_type = "count", value_scale = 1)
#' x2_dcc_agg <- aggregate(x2_dcc, by_dimension = "age") # OK
#' stopifnot(identical(class(x2_dcc_agg), c("demog_change_component_df", "data.frame")))
#'
#' # Specify 'out_class'
#' x2_dcc_agg <- aggregate(x2, by_dimension = "age", out_class = "demog_change_component_df") # OK
#' stopifnot(identical(class(x2_dcc_agg), c("demog_change_component_df", "data.frame")))
#'
#'
#' ## Coercing 'x' to a data.frame.
#'
#' x3 <- as.data.frame(x1) # will issue a warning that class is dropped
#' s3_agg <- aggregate(x3,
#'                     by = list(time_start = x1$time_start), # no 'by_dimension' argument!
#'                     FUN = "sum")
#'
#' @export
aggregate.demog_change_component_df <- function(x, by = NULL, by_dimension = NULL, FUN = "sum", ..., out_class = class(x)[1]) {
    if (!value_type(x) %in% get_all_aggregatable_value_types())
        stop("'value_type(x)' is '", toString(value_type(x)), "' but the only aggregatable 'value_type's are '", toString(get_all_aggregatable_value_types()), "'.")
    if (!identical(sum(is.null(by), is.null(by_dimension)), 1L))
        stop("Exactly one of 'by' and 'by_dimension' must be non-NULL.")
    if (!is.null(by_dimension)) {
        if (!is.character(by_dimension)) stop("'by_dimension' must be a 'character' vector.")
        by_dimension <- match.arg(by_dimension, get_all_allowed_dimensions(), several.ok = TRUE)
        by_dimension <- get_all_req_col_names_excl_spans_for_dimensions(by_dimension)
        by_dimension <- by_dimension[by_dimension != "value"]
        return(aggregate.demog_change_component_df(x = x, by = x[, by_dimension, drop = FALSE],
                                                   by_dimension = NULL,
                                                   FUN = FUN, ..., out_class = out_class))
    } else {
        if (!is.list(by)) stop("'by' must be a list.")
        stopifnot(identical(length(out_class), 1L))
        class_x <- class(x)
        if (length(class_x) > 1) {
            i <- match(out_class, class_x)
            if (!is.na(i) && i < length(class_x)) out_class <- c(out_class, tail(class_x, -i))
        }
        if (!all(out_class %in% c(get_all_demog_change_component_df_class_names(), "data.frame")))
            stop("'out_class' must only use classes in this list '",
                 toString(c(get_all_demog_change_component_df_class_names(), "data.frame")),
                 "'.")
        value_type_x <- value_type(x)
        value_scale_x <- value_scale(x)
        out <- stats::aggregate.data.frame(x = as.data.frame(x[, "value", drop = FALSE]),
                                           by = by,
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
                    msg <- paste0("The aggregate of 'x' cannot be coerced to the class in argument 'out_class' (i.e., '", toString(out_class), "').\n\tIf you want to aggregate 'x', set 'out_class' to '", class_x[1], "' or coerce it to a '", class_x[1], "' object and use 'aggregate' again (see examples in '?aggregate.demog_change_component_df').")
                    if (identical(class_x[1], "data.frame"))
                        msg <- paste0(msg, " Note that the 'data.frame' method of 'aggregate' will be called so you will need different arguments. Specifically, the 'by_dimension' argument cannot be used with this method. See '?aggregate.data.frame'.")
                    msg <- paste0(msg, " Note that the result will not have the same class as 'x'.")
                    stop(msg)
                }
            }
            stop("Could not aggregate 'x'. Try coercing it to a 'data.frame' and using 'aggregate' on that. Note that the result will not have the same class as 'x'.")
        }
    }
}
