###
### Methods for *existing* generics and functions. See '..._utility_functions.R' for new generics and methods.
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

#' @rdname subset_replace
#' @export
subset.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("'subset'ing a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    return(NextMethod())
}

###-----------------------------------------------------------------------------
### * Col and Row names

#' Change column and row names of a \code{demog_change_component_df}
#'
#' Changing row or columns names of \code{demog_change_component_df}s
#' may result in invalid members of the class. Therefore, the changed
#' object will no longer inherit from class
#' \code{demog_change_component_df} (or subclasses) and the attributes
#' specific to those classes will be lost.
#'
#' @seealso \code{\link{demog_change_component_df}} for class
#'     definitions.
#'
#' @inheritParams base::colnames
#' @return A \code{data.frame},
#'     one-dimensional vector, or scalar depending on the dimensions
#'     of the extracted values.
#' @author Mark Wheldon
#'
#' @name col_row_names
NULL


#' @rdname col_row_names
#' @export
`row.names<-` <- function(x, value) {
    UseMethod("row.names<-")
}

#' @rdname col_row_names
#' @export
`row.names<-.default` <- function(x, value) {
    base::`row.names<-`(x = x, value = value)
}

#' @rdname col_row_names
#' @export
`row.names<-.demog_change_component_df` <- function(x, value) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("changing the 'row.names' of a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    base::`row.names<-`(x = as.data.frame(x), value = value)
}


#' @rdname col_row_names
#' @export
`rownames<-` <- function(x, value) {
    UseMethod("rownames<-")
}

#' @rdname col_row_names
#' @export
`rownames<-.default` <- function(x, value) {
    base::`rownames<-`(x = x, value = value)
}

#' @rdname col_row_names
#' @export
`rownames<-.demog_change_component_df` <- function(x, value) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("changing the 'rownames' of a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    base::`rownames<-`(x = as.data.frame(x), value = value)
}


#' @rdname col_row_names
#' @export
`colnames<-` <- function(x, value) {
    UseMethod("colnames<-")
}

#' @rdname col_row_names
#' @export
`colnames<-.default` <- function(x, value) {
    base::`colnames<-`(x = x, value = value)
}

#' @rdname col_row_names
#' @export
`colnames<-.demog_change_component_df` <- function(x, value) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("changing the 'colnames' of a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    base::`colnames<-`(x = as.data.frame(x), value = value)
}


#' @rdname col_row_names
#' @export
`dimnames<-` <- function(x, value) {
    UseMethod("dimnames<-")
}

#' @rdname col_row_names
#' @export
`dimnames<-.default` <- function(x, value) {
    base::`dimnames<-`(x = x, value = value)
}

#' @rdname col_row_names
#' @export
`dimnames<-.demog_change_component_df` <- function(x, value) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("changing the 'dimnames' of a '",
                         oldClass(x)[1],
                         "' will not preserve the class or attributes.")
    }
    base::`dimnames<-`(x = as.data.frame(x), value = value)
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

## Custom y-asix scales based on class. E.g., mortality rates get log y-axis.
get_y_scale <- function(x) {
    UseMethod("get_y_scale")
}
get_y_scale.demog_change_component_df <- function(x) {
    stopifnot(requireNamespace("scales", quietly = TRUE))
    if (identical("mort_rate_age_sex", oldClass(x)[1]))
        return(scales::log_trans())
    else return(scales::identity_trans())
}


#' Plot \code{demog_change_component_df} objects
#'
#' This is the S3 method for the generic \code{\link{plot}}
#' function. It requires \pkg{ggplot2}.
#'
#' @param x An object inheriting from class \code{demog_change_component_df}.
#' @param type The type of plot to produce, e.g., \dQuote{line}.
#' @param ... Passed on to \code{\link[ggplot2]{ggplot}}.
#' @return A \code{ggplot2} plot object.The function is mainly called for
#'     the side effect of producing a plot.
#' @author Mark Wheldon
#' @export
plot.demog_change_component_df <-
    function(x, type = c("line", "point", "both"), ...) {

        type <- match.arg(type)

        if (!all(requireNamespace("ggplot2", quietly = TRUE),
                 requireNamespace("scales", quietly = TRUE))) {
            message("Install the 'ggplot2' and 'scales' packages for a better plotting experience.")
            return(NextMethod(type = substr(type, 1, 1)))
        }

        ## -------* Base Plot For Dimensions

        dcc_dims_x <- demog_change_component_dims(x)

        if ("non_zero_fert_ages" %in% names(attributes(x))) {
            nzf_ages_present <- TRUE
            fert_rate_ages <-
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
            if (identical(type, "line")) {
                if (nzf_ages_present) {
                    gp <- gp + ggplot2::geom_line(ggplot2::aes(col = fert_rate_ages)) +
                        ggplot2::scale_colour_manual(name = "Reproductive\nage range",
                                                     values = c("Non-zero fertility" = "black",
                                                                "Zero fertility" = "grey"))
                } else {
                    gp <- gp + ggplot2::geom_line()
                }
            } else if (identical(type, "point")) {
                if (nzf_ages_present) {
                    gp <- gp + ggplot2::geom_point(ggplot2::aes(shape = fert_rate_ages, col = fert_rate_ages)) +
                        ggplot2::scale_shape_manual(name = "Reproductive\nage range",
                                                    values = c("Non-zero fertility" = 19,
                                                               "Zero fertility" = 1)) +
                        ggplot2::scale_colour_manual(name = "Reproductive\nage range",
                                                     values = c("Non-zero fertility" = "black",
                                                                "Zero fertility" = "grey"))
                } else {
                    gp <- gp + ggplot2::geom_point()
                }
            } else if (identical(type, "both")) {
                if (nzf_ages_present) {
                    gp <- gp + ggplot2::geom_point(ggplot2::aes(shape = fert_rate_ages, col = fert_rate_ages)) +
                        ggplot2::geom_line(ggplot2::aes(col = fert_rate_ages)) +
                        ggplot2::scale_shape_manual(name = "Reproductive\nage range",
                                                    values = c("Non-zero fertility" = 19,
                                                               "Zero fertility" = 1)) +
                        ggplot2::scale_colour_manual(name = "Reproductive\nage range",
                                                     values = c("Non-zero fertility" = "black",
                                                                "Zero fertility" = "grey"))
                } else {
                    gp <- gp + ggplot2::geom_point() + ggplot2::geom_line()
                }
            }

            ## x-axis breaks
            if ("age" %in% dcc_dims_x) x_var_name <- "age_start"
            else x_var_name <- "time_start"

            if (abs(diff(range(x[[x_var_name]]))) >= 80) gp <- gp + ggplot2::scale_x_continuous(breaks = scales::breaks_width(20))
            else if (abs(diff(range(x[[x_var_name]]))) >= 40) gp <- gp + ggplot2::scale_x_continuous(breaks = scales::breaks_width(10))
            else if (abs(diff(range(x[[x_var_name]]))) >= 20) gp <- gp + ggplot2::scale_x_continuous(breaks = scales::breaks_width(5))
            ## otherwise use the default

        } else {
            gp <- gp + ggplot2::geom_bar()
        }

        ## -------* y-Axis Scale

        gp <- gp + ggplot2::scale_y_continuous(trans = get_y_scale(x))

        ## -------* RETURN

        return(gp)
    }


###-----------------------------------------------------------------------------
### * Transformations

#' Aggregate a \code{demog_change_component_df}
#'
#' A method for the generic \code{\link[stats]{aggregate}}
#' function. This merely calls the
#' \code{\link[stats]{aggregate.data.frame}} method and issues a
#' warning that the class will be dropped (if called
#' interactively). See \code{link{collapse_demog_dimension}} for an
#' alternative if aggregation across an entire demographic dimension
#' (e.g., \dQuote{age}, \dQuote{time}, etc.) is desired.
#'
#' @inheritParams stats::aggregate
#' @author Mark Wheldon
#'
#' @seealso \code{\link[stats]{aggregate}}, \code{link{collapse_demog_dimension}}
#'
#' @importFrom stats aggregate
#' @export
aggregate.demog_change_component_df <- function (x, by, FUN, ..., simplify = TRUE, drop = TRUE) {

    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("'aggregate'ing a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. See ?collapse_demog_dimension for a possible alternative.")
    }
    return(NextMethod())
    }
