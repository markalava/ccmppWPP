################################################################################
###
### S3 Classes for CCMPP Objects
###
###-----------------------------------------------------------------------------
###
### TO DO:
###
### - Look through SH and PG's comments in the vignette.
###
### - The restriction that 'time_span' equals 'age_span' only applies to ccmpp
###   inputs. Need to decide how to handle this. A nested generic class, e.g.,
###   c("ccmpp_input", "demog_change_component_df")? The subclasses 'fert_rate',
###   'survival_ratio', etc., would not be real classes, but would be created
###   using eponymous wrappers to 'demog_change_component_df'. That way we can
###   have lots of different functions, e.g., 'fert_rate_ccmpp_input_df()',
###   'fert_rate_ccmpp_output_df()', etc. etc., without getting tied up in
###   complicated nesting/crossing of classes. All will just inherit be
###   of the main class, but will be created such that the attributes are set
###   appropriately. This will also make it a lot easier for others to create
###   their own types of objects just be writing wrapper functions, rather
###   than getting into the mess of the classes.
###
### - We need a better name:
###   - dem_comp_change_df
###   - dem_change_comp_df
###   - dem_comp_df
###   - pop_change_comp_df
###   - pop_comp_change_df
###
################################################################################

###-----------------------------------------------------------------------------
### * Base Type Class Definitions/Constructors

#' Low-level constructor for class \code{demog_change_component_df}.
#'
#' @description
#' Creates an object of class \code{demog_change_component_df}. Minimal
#' checks are done; for interactive use see
#' \code{\link{demog_change_component_df}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{demog_change_component_df}}.
#'
#' @seealso demog_change_component_df
#'
#' @family demog_change_component_df class non-exported functions
#'
#' @param age_span Scalar indicating the span of the age groups.
#' @param time_span Scalar indicating the span of the time periods.
#' @param dimensions Character vector listing the dimensions such as
#'     \dQuote{time}, \dQuote{age}, \dQuote{sex}.
#' @param value_type Scalar indicating the type of the \dQuote{value}
#'     column (e.g., \dQuote{count}, \dQuote{rate}, etc.).
#' @return An object of class \code{demog_change_component_df}.
#' @author Mark Wheldon
new_demog_change_component_df <-
    function(x, dimensions = character(),
             age_span = double(),
             time_span = double(),
             value_type = character(),
             ..., class = character()) {
        if (missing(x)) x <- data.frame()
        stopifnot(is.data.frame(x))
        stopifnot(is.numeric(age_span))
        stopifnot(is.numeric(time_span))
        stopifnot(is.character(class))
        structure(x,
                  dimensions = dimensions,
                  age_span = age_span,
                  time_span = time_span,
                  value_type = value_type,
                  ...,
                  class = c(class, "demog_change_component_df", "data.frame"))
    }


#' Constructor for class \code{demog_change_component_df}
#'
#' \describe{
#' \item{\code{demog_change_component_df}}{Creates objects of class
#' \code{demog_change_component_df} from a given set of values.}
#' \item{\code{coerce_demog_change_component_df}}{Attempts to turn its argument into a
#' \code{demog_change_component_df}.}
#' \item{\code{is_demog_change_component_df}}{Tests if its
#' argument is a \code{demog_change_component_df}. \emph{Note:} This only checks
#' inheritance (via \code{\link{base::inherits}}), not validity. To
#' validate an object use \code{\link{validate_ccmpp_object}}.}
#'
#' \code{demog_change_component_df} is the
#' parent class of a family of classes for holding age- time-specific
#' demographic vital rates, such as fertility and mortality rates,
#' population counts, and migration rates. It is not intended as a
#' class for raw data; only one value per age and period is
#' permitted. Rather, it is intended to be useful to ensure that
#' objects passed to functions that expect demographic values as
#' input are correctly formatted.
#'
#' Objects of this class have class attribute
#' \code{c("demog_change_component_df", "data_frame")}.
#'
#' @details
#' The class is based on a data frame with age-sex-specific vital rates for
#' one or more time periods in a column named \dQuote{value}.
#'
#' See the help files for the other constructor functions under
#' \dQuote{See also} for further details specific to each type of vital rate
#' or parameter.
#'
#' @section Class Definition:
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
#' @family demog_change_component_df constructor functions
#'
#' @param x For \code{demog_change_component_df}: A data frame with
#'     columns \dQuote{age_start}, \dQuote{age_span}, \dQuote{sex},
#'     \dQuote{time_start}, \dQuote{time_span}, and \dQuote{value}.
#' @param age_span Scalar indicating the span of the age groups. If
#'     \code{NULL} and a column \dQuote{age_span} is present in
#'     \code{x} an attempt will be made to infer it from that column.
#' @param time_span Scalar indicating the span of the time periods;
#'     similar behaviour to \code{age_span}.
#' @param dimensions Character vector listing the dimensions such as
#'     \dQuote{time}, \dQuote{age}, \dQuote{sex}.
#' @param value_type Scalar indicating the type of the \dQuote{value}
#'     column (e.g., \dQuote{count}, \dQuote{rate}, etc.).
#' @param ... Passed to the low-level constructor.
#' @return An object of class \code{demog_change_component_df}.
#' @author Mark Wheldon
#' @export
demog_change_component_df <-
    function(x,
             dimensions = attr(x, "dimensions"),
             age_span = attr(x, "age_span"),
             time_span = attr(x, "time_span"),
             value_type = attr(x, "value_type"),
             ...) {

        if (is_demog_change_component_df(x)) {
            x <- validate_ccmpp_object(x)
            return(as_demog_change_component_df(x))
        }

        if (!is.data.frame(x))
            stop("'x' is not a data.frame.")

        ## -------* 'dimensions' attribute

        if (is.null(dimensions)) {
            dimensions <- guess_dimensions_from_df_cols(x)
            message("Argument 'dimensions' is 'NULL'; setting 'dimensions' to '",
                    paste(dimensions, collapse = ", "),
                    "' based on column names of 'x'.")
        } else {
            ## Check 'dimensions'
            allowed_dimensions <- get_all_allowed_dimensions()
            if (!all(dimensions %in% allowed_dimensions))
                stop("'dimensions' can only include '",
                     paste(allowed_dimensions, collapse = "', '"),
                     "'.")
        }

        is_by_age <- is_by_age(x) # 'data.frame' method used here.

        ## -------* Columns

        ## List required columns for output and input
        req_cols_out <- get_all_req_col_names_for_dimensions(dimensions)
        req_cols_out_types <- get_all_req_col_types_for_dimensions(dimensions)
        req_cols_in <- req_cols_out
        req_cols_in_types <- req_cols_out_types

        ## Check required columns are present in 'x'. If 'age_span'
        ## and 'time_span' are not specified as arguments, take them
        ## from 'x'.
        if (is.null(time_span)) {
            req_cols_in <- c(req_cols_in, "time_span")
            req_cols_in_types <- c(req_cols_in_types, "numeric")
            message("Argument 'time_span' is 'NULL'; taking 'time_span' from 'x$time_span'.")
        }
        if (is.null(age_span) && is_by_age) {
            req_cols_in <- c(req_cols_in, "age_span")
            req_cols_in_types <- c(req_cols_in_types, "numeric")
            message("Argument 'age_span' is 'NULL'; taking 'age_span' from 'x$age_span'.")
        }
        if (!all(req_cols_in %in% names(x)))
            stop("'x' must have columns '",
                 paste(req_cols_in, collapse = "', '"),
                 "'.")

        ## -------* Detect attributes from columns

        ## If arguments 'age_span' or 'time_span' are 'NULL' extract
        ## them from 'x' (already checked that they are there).
        if (is.null(time_span)) time_span <- x$time_span[1]
        if (is.null(age_span) && is_by_age) age_span <- x$age_span[1]

        ## -------* Values

        if (is.null(value_type)) {
            value_type <- "real"
            message("Argument 'value_type' is 'NULL'; setting 'value_type' to 'real'.")
        }

        ## -------* Other

        ## Clean 'x' and convert factors to character
        x <- x[, req_cols_out]
        char_cols <- which(req_cols_out_types == "character")
        for (j in char_cols) {
            if (is.factor(x[, j])) x[, j] <- as.character(x[, j])
        }

        ## Sort by time, then sex, then age
        x <- sort_demog_change_component_df(x)

        ## row.names might have been muddled by sorting so reset
        row.names(x) <- NULL

        ## -------* End

        ## Create/Validate
        validate_ccmpp_object(
            new_demog_change_component_df(x,
                              dimensions = dimensions,
                              age_span = (if (is.null(age_span)) double() else age_span),
                              time_span = (if (is.null(time_span)) double() else time_span),
                              value_type = value_type,
                              ...
                              )
        )
    }


#' Coerce to a \code{demog_change_component_df}
#'
#' These functions coerce an object to a
#' \code{demog_change_component_df} (or subclasses, as appropriate) if
#' possible, or check if it is one.
#'
#' @section Note on Validation:
#' The method for \code{demog_change_component_df} and subclasses (i.e.,
#' \code{as_demog_change_component_df.demog_change_component_df}) does
#' \emph{not} check for validity. If \code{x} is somehow an invalid
#' \code{demog_change_component_df} object, applying
#' \code{as_demog_change_component_df} will simply return it (with
#' \code{attr(x, "class")} potentially modifed), not signal an
#' error. Use \code{demog_change_component_df} on the object to ensure
#' validity is checked.
#'
#' @param x An object to coerce or check.
#' @param ... Further arguments passed to specific methods.
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_demog_change_component_df
#' @export
as_demog_change_component_df <- function(x, ...) {
    UseMethod("as_demog_change_component_df")
}

#' @export
as_demog_change_component_df.default <- function(x, ...) {
    if (is_demog_change_component_df(x)) return(x)
    stop("Cannot coerce 'x' to 'demog_change_component_df'.")
}

#' @rdname coerce_demog_change_component_df
#' @export
as_demog_change_component_df.data.frame <- function(x, ...) {
    demog_change_component_df(as.data.frame(x))
}

#' @rdname coerce_demog_change_component_df
#' @export
as_demog_change_component_df.matrix <- function(x, ...) {
    dn <- dimnames(x)
    dncol <- dn[[2]]
    if (is.null(dn))
        stop("'dimnames(x)' is 'NULL'; cannot coerce to 'demog_change_component_df'.")
    if (!("value" %in% dncol))
        stop("'x' must have a column called 'value'.")
    req_cn <- get_all_req_col_names_for_dimensions()
    if (!sum(req_cn %in% dn[[2]]) >= 2)
        stop("'x' must have time, age, or sex dimensions. Column names should be taken from '",
             paste(req_cn, collapse = "', '"),
             "'.")
    keep_cols <- which(dncol %in% req_cn)
    x <- x[, keep_cols]
    as_demog_change_component_df(as.data.frame(x))
}

#' @export
as_demog_change_component_df.demog_change_component_df <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("demog_change_component_df", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(x)
}

#' @rdname coerce_demog_change_component_df
#' @export
is_demog_change_component_df <- function(x) {
    inherits(x, "demog_change_component_df")
}

