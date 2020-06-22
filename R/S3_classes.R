################################################################################
###
### S3 Classes for CCMPP Objects
###
################################################################################

###-----------------------------------------------------------------------------
### * Helper Functions

## Helper functions defining the proper sort order of the class
sort_vital_rate_df <- function(x) {
    x <- x[order(x$time_start, rev(x$sex), x$age_start), ]
}

###-----------------------------------------------------------------------------
### * Base Type Class Definitions/Constructors

###-----------------------------------------------------------------------------
### ** Vital Rate Data Frame Age and Time and Sex




## !!!!!!!!!!!!!!!!!!! HOW TO HANDLE DIFFERENT TYPES OF DFs
##
## 1. Create different classes: time, time_age, time_age_sex.
## 2. Have an attribute that classifies vital_rate_df as one of the three subclasses.
## 3. Add a 'sex' attribute to the existing class definition. Set
## combinations of 'age_span', 'time_span', 'sex' to 'NULL' to
## indicate which subclass it is.
##
## Pref: (2). Seems a good compromise. (1) is too much coding and hard
## to make it efficient [how would inheritance work? - would be very
## complicated]. (3) feels like its asking attributes to do double
## duty and could lead to conflicts if extensions are added later.
##
## With (2), would hopefully just need to add some 'if/else' clauses
## to the existing functions to turn on/off the various validation and
## construction parts. The 'new_' constructor maybe doesn't need
## changing at all.
##
## OTHER: Add functions to compute years, ages, and sexes present in
## the object. Don't think these need to be attributes.





#' Low-level constructor for class \code{vital_rate_df}.
#'
#' @description
#' Creates an object of class \code{vital_rate_df}. Minimal
#' checks are done; for interactive use see
#' \code{\link{validate_vital_rate_df}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{vital_rate_df}}.
#'
#' @seealso vital_rate_df
#'
#' @family vital_rate_df class non-exported functions
#'
#' @param age_span Scalar indicating the span of the age groups.
#' @param time_span Scalar indicating the span of the time periods.
#' @return An object of class \code{vital_rate_df}.
#' @author Mark Wheldon
new_vital_rate_df <-
    function(x, age_span, time_span,
             ..., class = character()) {
        stopifnot(is.data.frame(x))
        structure(x,
                  age_span = age_span,
                  time_span = time_span,
                  ...,
                  class = c(class, "vital_rate_df", "data.frame"))
    }


#' Constructor for class \code{vital_rate_df}
#'
#' @description
#'
#' \describe{
#' \item{\code{vital_rate_df}}{Creates objects of class \code{vital_rate_df} from a given set of values.}
#' \item{\code{as.vital_rate_df}}{Attempts to turn its argument into a \code{vital_rate_df}.}
#' \item{\code{is.vital_rate_df}}{Tests if its argument is a \code{vital_rate_df}. \emph{Note:} This only checks inheritance (via \code{\link{base::inherits}}), not validity. To validate an object use \code{\link{validate_vital_rate_df}}.}
#'
#' \code{vital_rate_df} is the
#' parent class of a family of classes for holding age- time-specific
#' demographic vital rates, such as fertility and mortality rates,
#' population counts, and migration rates. It is not intended as a
#' class for raw data; only one value per age and period is
#' permitted. Rather, it is intended to be useful to ensure that
#' objects passed to functions that expect demographic values as
#' input are correctly formatted.
#'
#' Objects of this class have class attribute
#' \code{c("vital_rate_df", "data_frame")}.
#'
#' @details
#'
#' The class is based on a data frame with age-sex-specific vital rates for
#' one or more time periods in a column named \dQuote{value}.
#'
#' See the help files for the other constructor functions under
#' \dQuote{See also} for further details specific to each type of vital rate
#' or parameter.
#'
#' @section Class Definition:
#'
#' **TBC** (details of the class)
#' 1. Must be sorted properly.
#'
#' \subsection{Attributes of objects with this class}
#'
#' **TBC**
#'
#' **ISSUES OUTSTANDING**
#' ======================
#'
#' SORTING:
#' --------
#'
#' Correct ordering by 'age_start' and 'time_start' is required for
#' the ccmpp functions (right?) What about by 'sex'? The sample
#' inputs are sorted by sex first (varying slowest) but with 'male'
#' first, so in reverse alphabetical order.
#'
#' FACTORS vs CHARACHTERS:
#' -----------------------
#'
#' 'sex' is a categorical variable and encoding it as a factor could
#' be justified. However it may be easier to encode as character and
#' have methods convert to factor when advantageous.
#'
#'
#' @family vital_rate_df constructor functions
#'
#' @param x A data frame with columns \dQuote{age_start},
#'     \dQuote{age_span}, \dQuote{sex}, \dQuote{time_start},
#'     \dQuote{time_span}, and \dQuote{value}.
#' @param age_span Scalar indicating the span of the age groups. If
#'     \code{NULL} and a column \dQuote{age_span} is present in
#'     \code{x} an attempt will be made to infer it from that column.
#' @param time_span Scalar indicating the span of the time periods; similar behaviour to \code{age_span}.
#' @param ... Passed to the low-level constructor.
#' @return An object of class \code{vital_rate_df}.
#' @author Mark Wheldon
#'
#' @name construct_vital_rate_df
NULL


#' @rdname construct_vital_rate_df
#' @export
vital_rate_df <-
    function(x, age_span = NULL, time_span = NULL, ...) {

        if (!is.data.frame(x))
            stop("'x' is not a data.frame.")

        ## List required columns for output and input
        req_cols_out <- c("age_start", "sex", "time_start", "value")
        req_cols_out_classes <- c("numeric", "character", "numeric", "numeric")
        req_cols_in <- req_cols_out
        req_cols_in_classes <- req_cols_out_classes

        ## Check required columns are present in 'x'. If 'age_span'
        ## and 'time_span' are not specified as arguments, take them
        ## from 'x'.
        if (is.null(age_span)) {
            req_cols_in <- c(req_cols_in, "age_span")
            req_cols_in_classes <- c(req_cols_in_classes, "numeric")
            message("Argument 'age_span' is 'NULL'; taking 'age_span' from 'x$age_span'.")
        }
        if (is.null(time_span)) {
            req_cols_in <- c(req_cols_in, "time_span")
            req_cols_in_classes <- c(req_cols_in_classes, "numeric")
            message("Argument 'time_span' is 'NULL'; taking 'time_span' from 'x$time_span'.")
        }
        if (!all(req_cols_in %in% names(x)))
            stop("'x' must have columns '",
                 paste(req_cols_in, collapse = "', '"),
                 "'.")

        ## If arguments 'age_span' or 'time_span' are 'NULL' extract
        ## them from 'x' (already checked that they are there).
        if (is.null(age_span)) age_span <- x$age_span[1]
        if (is.null(time_span)) time_span <- x$time_span[1]

        ## Clean 'x' and make sure there are no factor columns
        x <- x[, req_cols_out]
        num_cols <- which(req_cols_out_classes == "numeric")
        char_cols <- which(req_cols_out_classes == "character")
        for (j in num_cols) {
            x[, j] <- as.numeric(x[, j])
        }
        for (j in char_cols) {
            x[, j] <- as.character(x[, j])
        }

        ## Sort by time, then sex, then age
        x <- sort_vital_rate_df(x)

        ## row.names might have been muddled by sorting so reset
        row.names(x) <- NULL

        ## Create/Validate
        validate_vital_rate_df(
            new_vital_rate_df(x,
                                      age_span = age_span,
                                      time_span = time_span,
                                      ...
                                      )
        )
    }


#' @rdname construct_vital_rate_df
#' @export
as.vital_rate_df <- function(x, ...) {
    if (is.vital_rate_df(x)) return(x)
    x <- as.data.frame(x)
    vital_rate_df(x)
}


#' @rdname construct_vital_rate_df
#' @export
is.vital_rate_df <- function(x) {
    inherits(x, "vital_rate_df")
}

###-----------------------------------------------------------------------------
### * Validation Functions

###-----------------------------------------------------------------------------
### ** Validation Helpers

## validate_vital_rate_df_basics <- function(x, req_cols, req_attr) {


###-----------------------------------------------------------------------------
### ** Validate time_age_sex version

#' Validate objects of class \code{vital_rate_df}.
#'
#' @description
#' Checks that an object with \code{class} attribute
#' \code{vital_rate_data_frame} is a valid object of this type.
#'
#' This function is not exported.
#'
#' @seealso vital_rate_data_frame
#'
#' @family vital_rate_data_frame class non-exported functions
#'
#' @param x An object of class \code{vital_rate_data_frame}.
#' @return Either an error or the object \code{x}.
#' @author Mark Wheldon
#' @export
validate_vital_rate_df <-
    function(x) {

        ## -------* Colnames

        coln_x <- colnames(x)
        req_cols <- c("age_start", "time_start", "sex", "value")
        opt_cols <- c("age_span", "time_span")

        if (!all(req_cols %in% coln_x))
            stop("'x' must have columns '",
                 paste0(req_cols, collapse = "', '"),
                 "'; some or all are missing.")

        if (!all(coln_x %in% c(req_cols, opt_cols))) {
                                # the converse has already been verified so this is a
                                # test for set equality
            superf_cols <- coln_x[!(coln_x %in% c(req_cols, opt_cols))]
            stop("'x' has superfluous columns. The following are not permitted: '",
                    paste0(superf_cols, collapse = "', '"),
                    "'.")
        }

        ## -------* Values

        if (!inherits(x, "data.frame"))
            stop("'x' does not inherit from 'data.frame'.")

        if (!all(is.finite(x$values)))
            stop("Not all 'x$value' are finite and non-missing.")

        ## -------* Attributes

        req_attr <-
            c("class", "names", "row.names", "age_span", "time_span")
        if (!all(req_attr %in% names(attributes(x))))
            stop("'x' must have attributes '",
                 paste(req_attr, collapse = "', '"),
                 "'; some are missing.")

        ## -------* Must be Sorted

        ## If not sorted, at least by age and sex within time, the
        ## single-step ccmpp function will turn out incorrect
        ## results. The class imposes full sorting.

        order_cols <- which(colnames(x) != "value")
        if (!identical(x[, order_cols],
                       sort_vital_rate_df(x)[, order_cols]))
            stop("'x' must be sorted by rev(sex), age_start, time_start (i.e., x <- x[order(rev(x$sex), x$time_start, x$age_start), ]).")

        ## -------* Check squareness

        x_tbl <- table(x$time_start, x$age_start, x$sex)
        if (!identical(as.double(sum(x_tbl != 1)), 0))
            stop("'x' does not have exactly one 'value' per 'age_start' * 'time_start' combination.")

        ## -------* Age and Time

        for (att in c("age", "time")) {
            ## Create names of the '_span' and '_start' variables for
            ## use later.
            span_name <- paste0(att, "_span")
            start_name <- paste0(att, "_start")

            ## Get the values of the attribute and column from x for
            ## use later.
            span_attr <- attr(x, span_name)
            start_col <- x[[start_name]]

            ## Need to know the diffs between successive 'age_start'
            ## and 'time_start', but these need to be done within
            ## levels of the other '_start'. Will end up with a list
            ## with elements for each distinct level of the other
            ## '_start'.
            other_start_col_name <-
                switch(start_name,
                       age_start = "time_start",
                       time_start = "age_start",
                       NA)
            start_1st_diff_unlist <-
                unlist(tapply(start_col,
                              INDEX = list(x[[other_start_col_name]], x$sex),
                              "diff", differences = 1, simplify = FALSE))

            ## Do the tests now:
            if (!is.numeric(span_attr))
                stop("'", span_name, "' is not numeric.")
            if (!identical(length(span_attr), 1L))
                stop("'", span_name, "' is not of length 1.")

            if (!is.numeric(start_col))
                stop("'x$", start_name, "' is not numeric.")
            if (!identical(as.double(sum(start_1st_diff_unlist != span_attr)), 0))
                stop("Spacings between each 'x$", start_name,
                     "' do not equal 'attr(x, \"", span_name, "\")'.")
        }

        ## Age must start at zero
        min_age_start <-
            tapply(x$age_start, INDEX = list(x$sex, x$time_start), FUN = "min")
        if (!all(min_age_start == 0))
            stop("'age_start' does not start at '0' for each time * sex combination.")

        ## -------* Sex

        if (!all(x$sex %in% c("female", "male", "both")))
            stop("Not all 'x$sex' are in ('female', 'male', 'both'); values other than these are not supported.")

    return(x)
}

###-----------------------------------------------------------------------------
### * Get Attributes

#' Extract attributes specific to \code{vital_rate_df}s
#'
#' All attributes that are not attributes of \code{data.frame} objects
#' are extracted. This function is designed to work with objects of
#' class \code{vital_rate_df}; behaviour for other classes is
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
vital_rate_attributes.vital_rate_df <- function(x) {
    attrx <- attributes(x)
    attr_names <- names(attrx)
    attr_names <- attr_names[!(attr_names %in% c("names", "row.names", "class"))]
    return(attrx[attr_names])
}
