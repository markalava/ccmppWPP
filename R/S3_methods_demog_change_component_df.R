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
#' The object resulting from a subset operation via \code{`[`} will no
#' longer inherit from class \code{demog_change_component_df} and the
#' attributes specific to that class will be lost. To create a
#' \code{demog_change_component_df} object from a subset call
#' \code{\link{demog_change_component_df}} explicitly (e.g.,
#' \code{demog_change_component_df(x[x$time_start == 1960,])}).
#'
#' Replacement via \code{`[<-`} will attempt to return a
#' \code{demog_change_component_df}. If the result is not a valid
#' member an error will be signalled.
#'
#' @seealso \code{\link{demog_change_component_df}}.
#'
#' @inheritParams base::`[.data.frame`
#' @inheritParams base::`[<-.data.frame`
#' @return Extraction could return a \code{data.frame},
#'     one-dimensional vector, or scalar depending on the dimensions
#'     of the extracted values. Replacement will
#'     attempt to return a \code{demog_change_component_df}.
#' @author Mark Wheldon
#'
#' @name subset_replace
NULL


#' @rdname subset_replace
#' @export
`[.demog_change_component_df` <- function(x, i, j, drop) {

    if (identical(parent.frame(), .GlobalEnv)) {
        warning("Subsetting a 'demog_change_component_df' will not preserve the class or attributes.")
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
    validate_demog_change_component_df(new_demog_change_component_df(NextMethod(),
                          age_span = attr(x, "age_span"),
                          time_span = attr(x, "time_span"),
                          dimensions = attr(x, "dimensions"),
                          value_type = attr(x, "value_type")
                          ))
    }


#' @rdname subset_replace
#' @export
`$<-.demog_change_component_df` <- function(x, name, value) {
    validate_demog_change_component_df(new_demog_change_component_df(NextMethod(),
                          age_span = attr(x, "age_span"),
                          time_span = attr(x, "time_span"),
                          dimensions = attr(x, "dimensions"),
                          value_type = attr(x, "value_type")
                          ))
    }


#' @rdname subset_replace
#' @export
`[[<-.demog_change_component_df` <- function(x, i, j, value) {
    validate_demog_change_component_df(new_demog_change_component_df(NextMethod(),
                          age_span = attr(x, "age_span"),
                          time_span = attr(x, "time_span"),
                          dimensions = attr(x, "dimensions"),
                          value_type = attr(x, "value_type")
                          ))
    }

###-----------------------------------------------------------------------------
### * Coercion

## Coercion removes the class and all attributes. Note that the
## default 'as.matrix' will remove them; a method is not required.

#' Coerce a \code{demog_change_component_df} to \code{data.frame}
#'
#' The class-specific attributes will be lost.
#'
#' @param x An object of class \code{demog_change_component_df}.
#' @return A \code{data.frame}.
#' @author Mark Wheldon
#' @export
as.numeric.demog_change_component_df <- function(x) {
    if (identical(parent.frame(), .GlobalEnv)) {
        warning("The result of the coercion will not inherit from class 'demog_change_component_df' and will not have any attributes specific to that class.")
    }
    return(as.numeric(c(x)))
}


#' Coerce a \code{demog_change_component_df} to \code{data.frame}
#'
#' The class-specific attributes will be lost.
#'
#' @param x An object of class \code{demog_change_component_df}.
#' @return A \code{data.frame}.
#' @author Mark Wheldon
#' @export
as.data.frame.demog_change_component_df <- function(x) {
    if (identical(parent.frame(), .GlobalEnv)) {
        warning("The result of the coercion will not inherit from class 'demog_change_component_df' and will not have any attributes specific to that class.")
    }
    return(as.data.frame(c(x)))
}


#' Coerce a \code{demog_change_component_df} to \code{list}
#'
#' The class-specific attributes will be lost.
#'
#' @param x An object of class \code{demog_change_component_df}.
#' @return A \code{list}.
#' @author Mark Wheldon
#' @export
as.list.demog_change_component_df <- function(x) {
    if (identical(parent.frame(), .GlobalEnv)) {
        warning("The result of the coercion will not inherit from class 'demog_change_component_df' and will not have any attributes specific to that class.")
    }
    return(as.list(c(x)))
}

###-----------------------------------------------------------------------------
### * Combine via c, rbind, etc.

rbind.demog_change_component_df <-
    function(..., deparse.level = 1, make.row.names = TRUE,
             stringsAsFactors = default.stringsAsFactors(),
             factor.exclude = TRUE) {
        warning("NOT TESTED---UNDER DEVELOPMENT")
        ldots <- list(...)
        if (!length(ldots)) NextMethod()
        l1 <- ldots[[1]]
        if (length(ldots) > 1) {
            l1_attr <- demog_change_component_attributes(l1)
            for(i in seq(from = 2, to = length(ldots), by = 1))
                if (!isTRUE(all.equal(l1_attr,
                                      demog_change_component_attributes(ldots[[i]]))))
                    stop("'Objects to 'rbind' do not all have the same 'demog_change_component_attributes'.")
        }
        validate_demog_change_component_df(new_demog_change_component_df(NextMethod(),
                          age_span = attr(l1, "age_span"),
                          time_span = attr(l1, "time_span"),
                          dimensions = attr(l1, "dimensions"),
                          value_type = attr(l1, "value_type")))
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
             quote = FALSE, right = TRUE, row.names = TRUE, max = NULL) {

        attr_names <- names(demog_change_component_attributes(x))
        attr_msgs <- sapply(attr_names, function(z) {
            paste0(z, " = '", paste(attr(x, z), collapse = ", "), "'")
        })
        attr_msgs <- paste(attr_msgs, collapse = ", ")

        cat_msg <- paste0("# A '", class(x)[1], "' with ", format(nrow(x), big.mark = ","),
            " rows.",
            "\n# ", attr_msgs,
            ".\n")
        if(is_by_sex(x))
            cat_msg <- paste(cat_msg, "# 'sex' has levels: ", paste(sexes(x), collapse = ", "),
            ".\n",
            sep = "")
        cat(cat_msg)
        print.data.frame(head(x, n = n), ..., digits = digits, quote = quote, right = right,
                         row.names = row.names, max = max)
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
#'
#' @seealso \code{\link{summary.demog_change_component_df}}.
#'
#' @export
print.summary_demog_change_component_df <-
    function(x, vsep, ...) {

        if (missing(vsep))
            vsep <- strrep("-", 0.75 * getOption("width"))

        msg <- character(0)

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

        cat(msg, "table:\n", sep = "")
        print(x$table)

        return(invisible(x))
    }

###-----------------------------------------------------------------------------
### * Subset

#' Subsetting \code{demog_change_component_df}s
#'
#' A method for \code{\link{base::subset}} for objects of class
#' \code{demog_change_component_df}s.
#'
#' The arguments and defaults are the same as for the
#' \code{data.frame} method. Notably, the actual column names must be
#' used, (e.g., \code{time_start}, \emph{not} \code{time}). To subset
#' using conceptual dimension names see, e.g.,
#' \code{\link{subset_time}}.
#'
#' @param x An object of class \code{demog_change_component_df}.
#' @param ... Passed to \code{\link{subset.data.frame}}. See the help
#'     file there for required arguments and defaults.
#' @return A \code{demog_change_component_df} if valid after
#'     subsetting, otherwise a \code{data.frame}.
#' @author Mark Wheldon
#' @export
subset.demog_change_component_df <- function(x, ...) {
    dcc_att <- demog_change_component_attributes(x)
    x <- NextMethod()
    y <- try(suppressMessages(demog_change_component_df(x,
                                       time_span = dcc_att$time_span,
                                       age_span = dcc_att$age_span,
                                       dimensions = NULL,
                                       value_type = dcc_att$value_type)),
             silent = TRUE)
    if (inherits(y, "try-error")) {
        warning("Subset result is not a valid 'demog_change_component_df'; returning a data frame.")
        return(x)
    } else {
        return(y)
    }
}
