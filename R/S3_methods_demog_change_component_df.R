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
    if(is_demog_change_component_df(x)) return(as.data.frame(x, restore_columns = FALSE))
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
    if(is_demog_change_component_df(x)) return(as.data.frame(x, restore_columns = FALSE))
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
    if(is_demog_change_component_df(x)) return(as.data.frame(x, restore_columns = FALSE))
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
    if(is_demog_change_component_df(x)) return(as.data.frame(x, restore_columns = FALSE))
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
as.data.frame.demog_change_component_df <- function(x, restore_columns = TRUE, ...) {
    oclass <- oldClass(x)
    if (identical(parent.frame(), .GlobalEnv)) {
        warning("The result of the coercion will not inherit from class '",
                oclass[1],
                "' and will not have any attributes specific to that class.")
    }
    if (is.data.frame(x) && is_demog_change_component_df(x) && restore_columns) {
        ## reinstate age_span and time_span
        cn <- colnames(x)
        demog_change_component_dims_x <- demog_change_component_dimensions(x)
        attr_w_span_names <- get_attr_w_span_names()

    for (att in
         attr_w_span_names[attr_w_span_names %in% demog_change_component_dims_x]
         ) {
        span_name <- paste0(att, "_span")
        if (!(span_name %in% cn)) x[[span_name]] <- attr(x, span_name)
        }
        ## avoid coercion/copying if can just remove the class
        oldClass(x) <- strip_demog_change_component_df_classes_attribute(oldClass(x))
        return(x)
    } else {
        x <- NextMethod()
        if (is_demog_change_component_df(x))
            oldClass(x) <- strip_demog_change_component_df_classes_attribute(oldClass(x))
        return(x)
    }
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
            return(x)
        } else return(x)
    }

###-----------------------------------------------------------------------------
### * Print, Summary, and Friends

#' Print Values of a \code{demog_change_component_df}
#'
#' This is a method for the generic \code{\link{base::print}} function. Only
#' the first \code{n} rows are printed for convenience (by default). If all rows are desired use
#' \code{as.data.frame(x)} or see the definition of argument \code{n}.
#'
#' @inheritParams base::print.data.frame
#'
#' @param x An object of class \code{demog_change_component_df}.
#' @param n Integer controlling how many rows of \code{x} are
#'     printed. Passed to \code{link{head}}; see the documentation of
#'     that function for valid values and associated behaviours.
#' @author Mark Wheldon
#' @export
print.demog_change_component_df <-
    function(x, ..., n = 6L, digits = NULL,
             quote = FALSE, right = TRUE, row.names = FALSE, max = NULL) {

        attr_names <- names(demog_change_component_attributes(x))
        if (inherits(x, "fert_rate_input_df"))
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
            "\n# ", attr_msgs,
            ".\n")
        if (inherits(x, "fert_rate_input_df"))
            cat_msg <- paste0(cat_msg, paste0("# non_zero_fert_ages = '",
                              toString(attr(x, "non_zero_fert_ages"), width = 20),
                              "'.\n"))
        if(is_by_sex(x))
            cat_msg <- paste(cat_msg, "# 'sex' has levels: ", paste(sexes(x), collapse = ", "),
            ".\n",
            sep = "")
        cat(cat_msg)

        if (inherits(x, "fert_rate_input_df") && is_by_age(x) &&
            !is.null(non_zero_fert_ages(x))) {
            x[!(x$age_start %in% non_zero_fert_ages(x)), "value"] <- NA
            print.table(as.matrix(x[seq_len(n),]),
                        digits = digits, quote = quote, na.print = ".",
                        right = right,
                        ...)
        } else {
            print.data.frame(head(x, n = n), ..., digits = digits, quote = quote, right = right,
                             row.names = row.names, max = max)
        }
        cat("# ... etc.\n")

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
                         span = time_span(object))
        } else {
            time <- NULL
        }

        ## Age
        if (is_by_age(object)) {
            age <- list(range = c("age_start" = min(object$age_start),
                                  "age_end" = max(object$age_start)),
                        span = age_span(object))
        } else {
            age <- NULL
        }

        ## Sex
        if (is_by_sex(object)) {
            sex <- list(levels = levels(as.factor(object$sex)))
        } else {
            sex <- NULL
        }

        ## Stats
        table <- NextMethod()

        return(structure(list(dimensions = demog_change_component_dimensions(object),
                              time = time, age = age, sex = sex,
                              value_type = value_type(object), table = table),
                         class = c("summary_demog_change_component_df", "list")))
    }

#' @rdname summary_demog_change_component_df
#' @export
summary.fert_rate_input_df <-
    function(object, maxsum = 7,
             digits = max(3, getOption("digits") - 3), vsep, ...) {
        out <- NextMethod()
        out$non_zero_fert_ages <- non_zero_fert_ages(object)

        return(structure(c(out),
                         class = c("summary_fert_rate_input_df",
                                   "summary_demog_change_component_df", "list")))
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
        msg <- paste0(msg, "dimensions:\t",
                      paste(x$dimensions,
                            collapse = ", "),
                      "\n")

        ## Time
        if (!is.null(x$time))
            msg <- paste0(msg, "      time:\trange = [",
                          paste(x$time$range, collapse = ", "),
                          "]\tspan = ",
                          paste0(x$time$span),
                          "\n")

        ## Age
        if (!is.null(x$age))
            msg <- paste0(msg, "       age:\trange = [",
                          paste(x$age$range, collapse = ", "),
                          "]\tspan = ",
                          paste0(x$age$span),
                          "\n")

        ## Sex
        if (!is.null(x$sex))
            msg <- paste0(msg, "       sex:\tlevels = ",
                          paste(x$sex$levels, collapse = ", "),
                          "\n")

        ## Values
        msg <- paste0(msg, "value_type:\t",
                      x$value_type,
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


#' @rdname demog_change_component_df
#' @export
print.summary_fert_rate_input_df <-
    function(x, vsep, ...) {
        if (missing(vsep))
            vsep <- strrep("-", 0.75 * getOption("width"))
        NextMethod(print_what = "info")
        cat(paste0("non_zero_fert_ages:\t",
               toString(x$non_zero_fert_ages, 30),
               "\n"),
            vsep, "\n",
            sep = "")
        NextMethod(print_what = "table")
        }