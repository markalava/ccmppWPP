################################################################################
###
### S3 Methods for CCMPP Objects
###
################################################################################

###-----------------------------------------------------------------------------
### * Fundamentals

#' Extract or Replace Parts of a \code{vital_rate_df}
#'
#' These are methods for the subset operator and its companion
#' replacement function for objects of class
#' \code{vital_rate_df}. The return values differ and,
#' importantly, subsetting does not return an object of class
#' \code{vital_rate_df} (see \dQuote{Details}).
#'
#' The object resulting from a subset operation via \code{`[`} will no
#' longer inherit from class \code{vital_rate_df} and the
#' attributes specific to that class will be lost. To create a
#' \code{vital_rate_time_age_sex_df} object from a subset call
#' \code{\link{vital_rate_df}} explicitly (e.g.,
#' \code{vital_rate_df(x[x$time_start == 1960,])}).
#'
#' Replacement via \code{`[<-`} will attempt to return a
#' \code{vital_rate_matrix}. The user is responsible for making sure
#' the new object is a valid member of this class (see
#' \code{\link{vital_rate_df}}). If the result is not a valid
#' member an error will be signalled.
#'
#' @seealso \code{\link{vital_rate_df}}.
#'
#' @inheritParams base::`[.data.frame`
#' @inheritParams base::`[<-.data.frame`
#' @return Extraction could return a \code{data.frame},
#'     one-dimensional vector, or scalar depending on the dimensions
#'     of the extracted values. Replacement will
#'     attempt to return a \code{vital_rate_df}.
#' @author Mark Wheldon
#'
#' @name subset_replace
NULL


#' @rdname subset_replace
#' @export
`[.vital_rate_df` <- function(x, i, j, drop) {

    if (identical(parent.frame(), .GlobalEnv)) {
        warning("Subsetting a 'vital_rate_df' will not preserve the class or attributes.")
    }

    x <- NextMethod()
    if(is.vital_rate_df(x)) return(as.data.frame(x))
                                # 'NextMethod()' will preseve the
                                # 'vital_rate_df' class so the
                                # 'vital_rate_df' method for
                                # 'as.data.frame' will be called. This
                                # will then produce a simple data
                                # frame with no extra attributes.
    else return(x)
}


#' @rdname subset_replace
#' @export
`[<-.vital_rate_df` <- function(x, i, j, value) {
    validate_vital_rate_df(new_vital_rate_df(NextMethod(),
                          age_span = attr(x, "age_span"),
                          time_span = attr(x, "time_span"),
                          dimensions = attr(x, "dimensions")
                          ))
    }


#' @rdname subset_replace
#' @export
`$<-.vital_rate_df` <- function(x, name, value) {
    validate_vital_rate_df(new_vital_rate_df(NextMethod(),
                          age_span = attr(x, "age_span"),
                          time_span = attr(x, "time_span"),
                          dimensions = attr(x, "dimensions")
                          ))
    }


#' @rdname subset_replace
#' @export
`[[<-.vital_rate_df` <- function(x, i, j, value) {
    validate_vital_rate_df(new_vital_rate_df(NextMethod(),
                          age_span = attr(x, "age_span"),
                          time_span = attr(x, "time_span"),
                          dimensions = attr(x, "dimensions")
                          ))
    }

###-----------------------------------------------------------------------------
### * Coercion

## Coercion removes the class and all attributes. Note that the
## default 'as.matrix' will remove them; a method is not required.

#' Coerce a \code{vital_rate_df} to \code{data.frame}
#'
#' The class-specific attributes will be lost.
#'
#' @param x An object of class \code{vital_rate_df}.
#' @return A \code{data.frame}.
#' @author Mark Wheldon
#' @export
as.numeric.vital_rate_df <- function(x) {
    if (identical(parent.frame(), .GlobalEnv)) {
        warning("The result of the coercion will not inherit from class 'vital_rate_matrix' and will not have any attributes specific to that class.")
    }
    return(as.numeric(c(x)))
}


#' Coerce a \code{vital_rate_df} to \code{data.frame}
#'
#' The class-specific attributes will be lost.
#'
#' @param x An object of class \code{vital_rate_df}.
#' @return A \code{data.frame}.
#' @author Mark Wheldon
#' @export
as.data.frame.vital_rate_df <- function(x) {
    if (identical(parent.frame(), .GlobalEnv)) {
        warning("The result of the coercion will not inherit from class 'vital_rate_matrix' and will not have any attributes specific to that class.")
    }
    return(as.data.frame(c(x)))
}


#' Coerce a \code{vital_rate_df} to \code{list}
#'
#' The class-specific attributes will be lost.
#'
#' @param x An object of class \code{vital_rate_df}.
#' @return A \code{list}.
#' @author Mark Wheldon
#' @export
as.list.vital_rate_df <- function(x) {
    if (identical(parent.frame(), .GlobalEnv)) {
        warning("The result of the coercion will not inherit from class 'vital_rate_matrix' and will not have any attributes specific to that class.")
    }
    return(as.list(c(x)))
}

###-----------------------------------------------------------------------------
### * Print, Summary, and Friends

#' Print Values of a \code{vital_rate_df}
#'
#' This is a method for the generic \code{\link{base::print}} function. Only
#' the first \code{n} rows are printed for convenience (by default). If all rows are desired use
#' \code{as.data.frame(x)} or see the definition of argument \code{n}.
#'
#' @inheritParams base::print.data.frame
#'
#' @param x An object of class \code{vital_rate_df}.
#' @param n Integer controlling how many rows of \code{x} are
#'     printed. Passed to \code{link{head}}; see the documentation of
#'     that function for valid values and associated behaviours.
#' @author Mark Wheldon
#' @export
print.vital_rate_df <-
    function(x, ..., n = 6L, digits = NULL,
             quote = FALSE, right = TRUE, row.names = TRUE, max = NULL) {

        attr_names <- names(vital_rate_attributes(x))
        attr_msgs <- sapply(attr_names, function(z) {
            paste0(z, " = '", paste(attr(x, z), collapse = ", "), "'")
        })
        attr_msgs <- paste(attr_msgs, collapse = ", ")

        cat_msg <- paste0("# A '", class(x)[1], "' with ", format(nrow(x), big.mark = ","),
            " rows.",
            "\n# ", attr_msgs,
            ".\n")
        if(is_by_sex(x))
            cat_msg <- c(cat_msg, "# 'sex' has levels: ", paste(sexes(x), collapse = ", "),
            ".\n",
            sep = "")
        cat(cat_msg)
        print.data.frame(head(x, n = n), ..., digits = digits, quote = quote, right = right,
                         row.names = row.names, max = max)
        cat("# ... etc.\n")
    }


#' Summarize a \code{vital_rate_df}
#'
#' A method for the generic \code{\link{summary}} function for objects
#' of class \code{vital_rate_df}}.
#'
#' @inheritParams base::summary
#'
#' @param object An object of class \code{vital_rate_df}.
#' @author Mark Wheldon
#' @export
summary.vital_rate_df <-
    function(object, maxsum = 7,
             digits = max(3, getOption("digits") - 3), ...) {

        if(is_by_sex(object))
            cat(## As long as 'sex' is not required to be a factor print
            ## its levels. If it is subsequently required to be a
            ## factor this next bit isn't necessary as the default for
            ## summary will display the levels.
            "# 'sex' has levels: ", paste(levels(factor(object$sex)), collapse = ", "),
            ".\n",
            sep = "")

        NextMethod(object = object)
    }


###-----------------------------------------------------------------------------
### * Misc.
