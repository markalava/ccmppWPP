###
### Methods for *existing* generics. See '..._utility_functions.R' for new generics and methods.
###

###-----------------------------------------------------------------------------
### * Fundamentals

#' Replace Parts of a \code{ccmpp_input_list}
#'
#' These are methods for the subset-replacement functions for objects
#' of class \code{ccmpp_input_list}. The return values differ and,
#' importantly, are not objects of class \code{ccmpp_input_list} (see
#' \dQuote{Details}).
#'
#' Arbitrary modifications of \code{ccmpp_input_list}s may not be
#' valid members of the class. Therefore, the object resulting from a
#' subset operation via \code{`[`}, or a replacement via \code{`[<-`},
#' and similar, will no longer inherit from class
#' \code{ccmpp_input_list} (or subclasses) and the attributes specific
#' to those classes will be lost.
#'
#' @seealso \code{\link{ccmpp_input_list}} for class definitions,
#'     \code{link{subset_time}} and friends, and
#'     \code{\link{[.demog_change_component_df}} for similar functions
#'     for the constituent data frame objects.
#'
#' @inheritParams base::`[.data.frame`
#' @inheritParams base::`[<-.data.frame`
#' @return A \code{list}.
#' @author Mark Wheldon
#'
#' @name ccmpp_input_list_replace
NULL


#' @rdname ccmpp_input_list_replace
#' @export
`$<-.ccmpp_input_list` <- function(x, name, value) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Replacing elements in a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if(is_ccmpp_input_list(x)) return(as.list(x))
                                # 'NextMethod()' will preseve the
                                # 'ccmpp_input_list' class so the
                                # 'ccmpp_input_list' method for
                                # 'as.list' will be called. This
                                # will then produce a simple data
                                # frame with no extra attributes.
    else return(x)
    }


#' @rdname ccmpp_input_list_replace
#' @export
`[[<-.ccmpp_input_list` <- function(x, i, j, value) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Replacing elements in a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if(is_ccmpp_input_list(x)) return(as.list(x))
                                # 'NextMethod()' will preseve the
                                # 'ccmpp_input_list' class so the
                                # 'ccmpp_input_list' method for
                                # 'as.list' will be called. This
                                # will then produce a simple data
                                # frame with no extra attributes.
    else return(x)
}


###-----------------------------------------------------------------------------
### * Print, Summary, and Friends

#' Print Values of a \code{ccmpp_input_list}
#'
#' This is a method for the generic \code{\link{base::print}}
#' function. Only the first \code{n} rows of each component of
#' \code{x} are printed for convenience (by default).
#'
#' @inheritParams base::print.data.frame
#'
#' @param x An object of class \code{ccmpp_input_list}.
#' @param n Integer controlling how many rows of \code{x} are
#'     printed. Passed to \code{link{head}}; see the documentation of
#'     that function for valid values and associated behaviours.
#' @param print_what What should be printed?
#' @author Mark Wheldon
#' @export
print.ccmpp_input_list <-
    function(x, ..., vsep, n = 6L, digits = NULL,
             quote = FALSE, right = TRUE, row.names = FALSE,
             print_what = c("info", "table")) {

        if (missing(vsep))
            vsep <- strrep("-", 0.75 * getOption("width"))

        for (df_nm in names(x)) {
            cat("\n$", df_nm, "\n", vsep, "\n", sep = "")
            print(x[[df_nm]], ..., n = min(n, nrow(x[[df_nm]])), digits = digits,
                  quote = quote, right = right, row.names = row.names,
                  print_what = print_what)
        }
        return(invisible(x))
    }


#' Summarize a \code{ccmpp_input_list}
#'
#' A method for the generic \code{\link{summary}} function for objects
#' of class \code{\link{ccmpp_input_list}}. Summary statistics are returned invisibly in a named list.
#'
#' @inheritParams base::summary
#'
#' @param object An object of class \code{ccmpp_input_list}.
#' @return The object (invisibly)
#' @author Mark Wheldon
#' @name summary_ccmpp_input_list
#' @export
summary.ccmpp_input_list <-
    function(object, vsep, ...) {

        if (missing(vsep))
            vsep <- strrep("-", 0.75 * getOption("width"))

        x <- summary(mig_net_count_component(object))
        y <- summary(fert_rate_component(object))

        msg <- character()

            ## Time
            if (!is.null(x$time))
                msg <- paste0(msg, "              time:  span = ",
                              toString(x$time$span, 7),
                              "\trange = [",
                              paste(x$time$range, collapse = ", "),
                              "]",
                              "\n")

            ## Age
            if (!is.null(x$age))
                msg <- paste0(msg, "               age:  span = ",
                              toString(x$age$span, 7),
                              "\trange = [",
                              paste(x$age$range, collapse = ", "),
                              "]",
                              "\n")

            ## Sex
            if (!is.null(x$sex))
                msg <- paste0(msg, "               sex:  levels = ",
                              paste(x$sex$levels, collapse = ", "),
                              "\n")

        ## Non zero fert ages
        if (!is.null(y$non_zero_fert_ages))
            msg <- paste0(msg, "non_zero_fert_ages:\t",
                          print_non_zero_fert_ages(y$non_zero_fert_ages, 30),
                          "\n")

            ## Print
            if (length(msg > 0))
                msg <- paste0(msg, vsep, "\n")

        cat(msg, "table:\n", sep = "")

        NextMethod()
    }

###-----------------------------------------------------------------------------
### * Coercion

## Coercion removes the class and all attributes.

#' Coerce a \code{ccmpp_input_list} to a plain list.
#'
#' The result will have \code{\link{class}} \code{list} and
#' class-specific attributes, such as \dQuote{age_span}, will be
#' dropped. Neither the classes nor the class-specific attributes of
#' the component data frames, all of which  inherit from
#' \code{\link{ccmpp_inut_df}}, will be dropped (see
#' \code{\link{as.data.frame.ccmpp_input_df}} if you want to do that).
#'
#' @seealso \code{\link{as.data.frame.ccmpp_input_df}} to drop classes
#'     and attributes of component data frames,
#'     \code{\link{ccmpp_input_list}} to create objects of class \code{ccmpp_input_list}
#'
#' @param x An object of class \code{ccmpp_input_list}.
#' @return A list with special attributes dropped.
#' @author Mark Wheldon
#' @export
as.list.ccmpp_input_list <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("The result of the coercion will not inherit from class '",
                oldClass(x)[1],
                "' and will not have any attributes specific to that class.")
    }
    x <- NextMethod()
    if (is_ccmpp_input_list(x))
        oldClass(x) <- strip_ccmpp_input_list_classes_attribute(oldClass(x))
    for (a in get_all_allowed_attributes()) {
        attr(x, a) <- NULL
    }
        return(x)
}
