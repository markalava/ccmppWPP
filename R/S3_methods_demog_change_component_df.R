################################################################################
###
### S3 Methods for CCMPP Objects
###
################################################################################

###-----------------------------------------------------------------------------
### * Fundamentals

#' Extract or Replace Parts of a \code{demog_change_component_df}
#'
#' These are methods for the subset operator and its companion
#' replacement function for objects of class
#' \code{demog_change_component_df}. The return values differ and,
#' importantly, subsetting does not return an object of class
#' \code{demog_change_component_df} (see \dQuote{Details}).
#'
#' Arbitrary subsets or modifications of
#' \code{demog_change_component_df}s (an objects of subclasses) may
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
        warning("Subsetting a '",
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
        warning("Replacing elements in a '",
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
        warning("Replacing elements in a '",
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
        warning("Replacing elements in a '",
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
        warning("The result of the coercion will not inherit from class '",
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
                warning("The result of the rbind will not inherit from class '",
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

            cat_msg <- paste0("# A '", class(x)[1], "' with ", format(nrow(x), big.mark = ","),
                              " rows.",
                              "\n# dimensions = '", paste(demog_change_component_dimensions(x), collapse = ", "), "'.",
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
            x <- as.matrix(x[seq_len(n),])
            if (!row.names) dimnames(x)[[1]] <- rep("", nrow(x))
            print.table(x,
                        digits = digits, quote = quote, na.print = ".",
                        right = right,
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

        return(structure(list(dimensions = demog_change_component_dimensions(object),
                              time = time, age = age, sex = sex, indicator = indicator,
                              value_type = value_type(object), values = values, table = table),
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
            if (!is.null(x$time))
                msg <- paste0(msg, "      time:  range = [",
                              paste(x$time$range, collapse = ", "),
                              "]")
            if (!is.null(x$time$span))
                msg <- paste0(msg,
                              "\tspan = ",
                              toString(x$time$span, 7))
            msg <- paste0(msg, "\n")

            ## Age
            if (!is.null(x$age))
                msg <- paste0(msg, "       age:  range = [",
                              paste(x$age$range, collapse = ", "),
                              "]")
            if (!is.null(x$age$span))
                msg <- paste0(msg,
                              "\tspan = ",
                              toString(x$age$span, 7))
            msg <- paste0(msg, "\n")

            ## Sex
            if (!is.null(x$sex))
                msg <- paste0(msg, "       sex:  levels = ",
                              paste(x$sex$levels, collapse = ", "),
                              "\n")

            ## Indicator
            if (!is.null(x$indicator))
                msg <- paste0(msg, " indicator:  levels = ",
                              toString(x$indicator$levels, 50),
                              "\n")

            ## Values
            if (!is.null(x$values)) {
                msg <- paste0(msg, "value_type:  ",
                              x$value_type,
                              "\tlevels = ", toString(x$values$levels, 20),
                              "\n")
            } else {
                msg <- paste0(msg, "value_type:  ",
                              x$value_type,
                              "\n")
            }

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
