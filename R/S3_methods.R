################################################################################
###
### S3 Methods for CCMPP Objects
###
################################################################################

###-----------------------------------------------------------------------------
### * Fundamentals

#' Extract or Replace Parts of a \code{vital_rate_df_time_age_sex}
#'
#' These are methods for the subset operator and its companion
#' replacement function for objects of class
#' \code{vital_rate_df_time_age_sex}. The return values differ and,
#' importantly, subsetting does not return an object of class
#' \code{vital_rate_df_time_age_sex} (see \dQuote{Details}).
#'
#' The object resulting from a subset operation via \code{`[`} will no
#' longer inherit from class \code{vital_rate_df_time_age_sex} and the
#' attributes specific to that class will be lost. To create a
#' \code{vital_rate_time_age_sex_df} object from a subset call
#' \code{\link{vital_rate_df_time_age_sex}} explicitly (e.g.,
#' \code{vital_rate_df_time_age_sex(x[x$time_start == 1960,])}).
#'
#' Replacement via \code{`[<-`} will attempt to return a
#' \code{vital_rate_matrix}. The user is responsible for making sure
#' the new object is a valid member of this class (see
#' \code{\link{vital_rate_df_time_age_sex}}). If the result is not a valid
#' member an error will be signalled.
#'
#' @seealso \code{\link{vital_rate_df_time_age_sex}}.
#'
#' @inheritParams base::`[.data.frame`
#' @inheritParams base::`[<-.data.frame`
#' @return Extraction could return a \code{data.frame},
#'     one-dimensional vector, or scalar depending on the dimensions
#'     of the extracted values. Replacement will
#'     attempt to return a \code{vital_rate_df_time_age_sex}.
#' @author Mark Wheldon
#'
#' @name subset_replace
NULL


#' @rdname subset_replace
#' @export
`[.vital_rate_df_time_age_sex` <- function(x, i, j, drop) {

    if (identical(parent.frame(), .GlobalEnv)) {
        warning("Subsetting a 'vital_rate_df_time_age_sex' will not preserve the class or attributes.")
    }

    ## 'NextMethod()' will preseve the 'vital_rate_df_time_age_sex' class
    ## so the 'vital_rate_df_time_age_sex' method for 'as.data.frame' will
    ## be called. This will then produce a simple data frame with no
    ## extra attributes.
    as.data.frame(NextMethod())
}


#' @rdname subset_replace
#' @export
`[<-.vital_rate_df_time_age_sex` <- function(x, i, j, value) {
    validate_vital_rate_df_time_age_sex(new_vital_rate_df_time_age_sex(NextMethod(),
                          age_span = attr(x, "age_span"),
                          time_span = attr(x, "time_span")
                          ))
    }


#' @rdname subset_replace
#' @export
`$<-.vital_rate_df_time_age_sex` <- function(x, name, value) {
    validate_vital_rate_df_time_age_sex(new_vital_rate_df_time_age_sex(NextMethod(),
                          age_span = attr(x, "age_span"),
                          time_span = attr(x, "time_span")
                          ))
    }


#' @rdname subset_replace
#' @export
`[[<-.vital_rate_df_time_age_sex` <- function(x, i, j, value) {
    validate_vital_rate_df_time_age_sex(new_vital_rate_df_time_age_sex(NextMethod(),
                          age_span = attr(x, "age_span"),
                          time_span = attr(x, "time_span")
                          ))
    }

###-----------------------------------------------------------------------------
### * Coercion

## Coercion removes the class and all attributes. Note that the
## default 'as.matrix' will remove them; a method is not required.

#' Coerce a \code{vital_rate_df_time_age_sex} to \code{data.frame}
#'
#' The class-specific attributes will be lost.
#'
#' @param x An object of class \code{vital_rate_df_time_age_sex}.
#' @return A \code{data.frame}.
#' @author Mark Wheldon
#' @export
as.data.frame.vital_rate_df_time_age_sex <- function(x) {
    if (identical(parent.frame(), .GlobalEnv)) {
        warning("The result of the coercion will not inherit from class 'vital_rate_matrix' and will not have any attributes specific to that class.")
    }
    return(as.data.frame(c(x)))
}


#' Coerce a \code{vital_rate_df_time_age_sex} to \code{list}
#'
#' The class-specific attributes will be lost.
#'
#' @param x An object of class \code{vital_rate_df_time_age_sex}.
#' @return A \code{list}.
#' @author Mark Wheldon
#' @export
as.list.vital_rate_df_time_age_sex <- function(x) {
    if (identical(parent.frame(), .GlobalEnv)) {
        warning("The result of the coercion will not inherit from class 'vital_rate_matrix' and will not have any attributes specific to that class.")
    }
    return(as.list(c(x)))
}

###-----------------------------------------------------------------------------
### * Print, Summary, and Friends

#' Print Values of a \code{vital_rate_df_time_age_sex}
#'
#' This is a method for the generic \code{\link{print}} function. Only
#' the first \code{n} rows are printed for convenience (by default). If all rows are desired use
#' \code{as.data.frame(x)} or see the definition of argument \code{n}.
#'
#' @inheritParams base::print.data.frame
#'
#' @param x An object of class \code{vital_rate_df_time_age_sex}.
#' @param n Integer controlling how many rows of \code{x} are
#'     printed. Passed to \code{link{head}}; see the documentation of
#'     that function for valid values and associated behaviours.
#' @author Mark Wheldon
#' @export
print.vital_rate_df_time_age_sex <-
    function(x, ..., n = 6L, digits = NULL,
             quote = FALSE, right = TRUE, row.names = TRUE, max = NULL) {

        attr_names <- names(vital_rate_attributes(x))
        attr_msgs <- sapply(attr_names, function(z) {
            paste0(z, " = '", attr(x, z), "'")
        })
        attr_msgs <- paste(attr_msgs, collapse = ", ")

        cat("# A '", class(x)[1], "' with ", format(nrow(x), big.mark = ","),
            " rows and attributes: ", attr_msgs,
            ".\n",
            "# 'sex' has levels: ", paste(levels(factor(x$sex)), collapse = ", "),
            ".\n",
            sep = "")
        print.data.frame(head(x, n = n), ..., digits = digits, quote = quote, right = right,
                         row.names = row.names, max = max)
        cat("# ... etc.\n")
    }


#' Summarize a \code{vital_rate_df_time_age_sex}
#'
#' A method for the generic \code{\link{summary}} function for objects
#' of class \code{vital_rate_df_time_age_sex}}.
#'
#' @inheritParams base::summary
#'
#' @param object An object of class \code{vital_rate_df_time_age_sex}.
#' @author Mark Wheldon
#' @export
summary.vital_rate_df_time_age_sex <-
    function(object, maxsum = 7,
             digits = max(3, getOption("digits") - 3), ...) {

        cat("# A '", class(x)[1], "' with ", format(nrow(x), big.mark = ","),
            " rows and attributes: ", attr_msgs,
            ".\n",
            ## As long as 'sex' is not required to be a factor print
            ## its levels. If it is subsequently required to be a
            ## factor this next bit isn't necessary as the default for
            ## summary will display the levels.
            "# 'sex' has levels: ", paste(levels(factor(object$sex)), collapse = ", "),
            ".\n",
            sep = "")
        NextMethod(object = object)
    }

###-----------------------------------------------------------------------------
### * Get Attributes

#' Extract attributes specific to \code{vital_rate_df_time_age_sex}s
#'
#' All attributes that are not attributes of \code{data.frame} objects
#' are extracted. This function is designed to work with objects of
#' class \code{vital_rate_df_time_age_sex}; behaviour for other classes is
#' not defined.
#'
#' @param x An object from which to extract attributes.
#' @return A list of attributes.
#' @author Mark Wheldon
#' @export
vital_rate_attributes <- function(x) {
    UseMethod("vital_rate_attributes")
}


#' @author Mark Wheldon
#' @export
vital_rate_attributes.vital_rate_df_time_age_sex <- function(x) {
    attrx <- attributes(x)
    attr_names <- names(attrx)
    attr_names <- attr_names[!(attr_names %in% c("names", "row.names", "class"))]
    return(attrx[attr_names])
}


###-----------------------------------------------------------------------------
### * Misc.
