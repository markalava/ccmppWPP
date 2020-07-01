################################################################################
###
### S3 Classes for CCMPP Objects
###
###-----------------------------------------------------------------------------
###
### TO DO:
###
### * Look through SH and PG's comments in the vignette.
### * The restriction that 'time_span' equals 'age_span' only applies to ccmpp
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
################################################################################

###-----------------------------------------------------------------------------
### * Base Type Class Definitions/Constructors

###-----------------------------------------------------------------------------
### ** Vital Rate Data Frame Age and Time and Sex

#' Low-level constructor for class \code{demog_change_component_df}.
#'
#' @description
#' Creates an object of class \code{demog_change_component_df}. Minimal
#' checks are done; for interactive use see
#' \code{\link{validate_demog_change_component_df}}.
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
    function(x, dimensions, age_span, time_span, value_type,
             ..., class = character()) {
        stopifnot(is.data.frame(x))
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
#' @description
#'
#' \describe{ \item{\code{demog_change_component_df}}{Creates objects of class
#' \code{demog_change_component_df} from a given set of values.}
#' \item{\code{as.demog_change_component_df}}{Attempts to turn its argument into a
#' \code{demog_change_component_df}.}  \item{\code{is.demog_change_component_df}}{Tests if its
#' argument is a \code{demog_change_component_df}. \emph{Note:} This only checks
#' inheritance (via \code{\link{base::inherits}}), not validity. To
#' validate an object use \code{\link{validate_demog_change_component_df}}.}
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
#' @family demog_change_component_df constructor functions
#'
#' @param x A data frame with columns \dQuote{age_start},
#'     \dQuote{age_span}, \dQuote{sex}, \dQuote{time_start},
#'     \dQuote{time_span}, and \dQuote{value}.
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
#'
#' @name construct_demog_change_component_df
NULL

#' @rdname construct_demog_change_component_df
#' @export
demog_change_component_df <-
    function(x,
             dimensions = NULL,
             age_span = NULL, time_span = NULL,
             value_type = NULL,
             ...) {

        if (!is.data.frame(x))
            stop("'x' is not a data.frame.")

        ## -------* 'dimensions' attribute

        if (is.null(dimensions)) {
            ## Attempt to guess dimensions
            col_info <-
                get_dim_col_info(dimensions = get_allowed_dimensions())
            dimensions <-
                col_info$dimension[col_info$colname %in% colnames(x)]
            message("Argument 'dimensions' is 'NULL'; setting 'dimensions' to '",
                    paste(dimensions, collapse = ", "),
                    "' based on column names of 'x'.")
        } else {
            ## Check 'dimensions'
            allowed_dimensions <- get_allowed_dimensions()
            if (!all(dimensions %in% allowed_dimensions))
                stop("'dimensions' can only include '",
                     paste(allowed_dimensions, collapse = "', '"),
                     "'.")
        }

        is_by_age <- is_by_age(x) # 'data.frame' method used here.

        ## -------* Columns

        ## List required columns for output and input
        req_cols_out <- get_all_req_col_names(dimensions)
        req_cols_out_types <- get_all_req_col_types(dimensions)
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

        ## Clean 'x' and make sure there are no factor columns
        x <- x[, req_cols_out]
        num_cols <- which(req_cols_out_types == "numeric")
        char_cols <- which(req_cols_out_types == "character")
        for (j in num_cols) {
            x[, j] <- as.numeric(x[, j])
        }
        for (j in char_cols) {
            x[, j] <- as.character(x[, j])
        }

        ## Sort by time, then sex, then age
        x <- sort_demog_change_component_df(x)

        ## row.names might have been muddled by sorting so reset
        row.names(x) <- NULL

        ## -------* End

        ## Create/Validate
        validate_demog_change_component_df(
            new_demog_change_component_df(x,
                              dimensions = dimensions,
                              age_span = age_span,
                              time_span = time_span,
                              value_type = value_type,
                              ...
                              )
        )
    }


#' @rdname construct_demog_change_component_df
#' @export
as.demog_change_component_df <- function(x, ...) {
    if (is.demog_change_component_df(x)) return(x)
    x <- as.data.frame(x)
    demog_change_component_df(x)
}


#' @rdname construct_demog_change_component_df
#' @export
is.demog_change_component_df <- function(x) {
    inherits(x, "demog_change_component_df")
}

###-----------------------------------------------------------------------------
### * Validation Functions

###-----------------------------------------------------------------------------
### ** Validate time_age_sex version

#' Validate objects of class \code{demog_change_component_df}.
#'
#' @description
#' Checks that an object with \code{class} attribute
#' \code{demog_change_component_df} is a valid object of this type.
#'
#' @seealso demog_change_component_df
#'
#' @family demog_change_component_df class non-exported functions
#'
#' @param x An object of class \code{demog_change_component_df}.
#' @return Either an error or the object \code{x}.
#' @author Mark Wheldon
#' @export
validate_demog_change_component_df <-
    function(x) {

        ## -------* Inherits

        if (!is.demog_change_component_df(x))
            stop("'x' does not inherit from class 'demog_change_component_df'.")

        ## -------* Attributes

        demog_change_component_dims_x <- demog_change_component_dimensions(x)
        if (is.na(demog_change_component_dims_x) || !is.character(demog_change_component_dims_x) ||
            length(demog_change_component_dims_x) < 1 ||
            !all(demog_change_component_dims_x %in% get_allowed_dimensions()))
            stop("'dimensions' attribute of 'x' is not valid. 'dimensions' must be in '",
                 paste(get_allowed_dimensions(), collapse = ", "),
                 "' and cannot be missing or duplicated. See ?demog_change_component_df for class definition.")

        req_attr <- get_req_attr_names(demog_change_component_dims_x)
        if (!all(req_attr %in% names(attributes(x))))
            stop("'x' must have attributes '",
                 paste(req_attr, collapse = "', '"),
                 "'; some are missing.")

        ## -------* Colnames

        coln_x <- colnames(x)
        req_cols <-
            get_all_req_col_names(dimensions = demog_change_component_dims_x)

        if (!all(req_cols %in% coln_x))
            stop("'x' must have columns '",
                 paste0(req_cols, collapse = "', '"),
                 "'; some or all are missing.")

        if (!all(coln_x %in% req_cols)) {
                                # the converse has already been verified so this is a
                                # test for set equality
            superf_cols <- coln_x[!(coln_x %in% req_cols)]
            stop("'x' has superfluous columns. The following are not permitted: '",
                    paste0(superf_cols, collapse = "', '"),
                    "'.")
        }

        ## -------* Values

        if (!inherits(x, "data.frame"))
            stop("'x' does not inherit from 'data.frame'.")

        value_type <- attr(x, "value_type")

        if (!identical(length(value_type), 1L) || !is.character(value_type)) {
            stop("'value_type' must be a single character string, or 'NULL'.")
        }

        allowed_value_types <- get_allowed_value_types()
        if (!(value_type %in% allowed_value_types))
            stop("'value_type' must be one of '",
                 paste(allowed_value_types, collapse = "', '"),
                 "'.")

        check_value_type(value = x$value, type = value_type)

        ## -------* Must be Sorted

        ## If not sorted, at least by age and sex within time, the
        ## single-step ccmpp function will turn out incorrect
        ## results. The class imposes full sorting.

        order_cols <-
            get_dim_col_info(dimensions = demog_change_component_dims_x)$colname
        if (!identical(x[, order_cols],
                       sort_demog_change_component_df(x)[, order_cols]))
            stop("'x' must be sorted by time, rev(sex), age_start (see ?demog_change_component_df for class definition).")

        ## -------* Check squareness

        x_tbl <- tabulate_demog_change_component_df(x)
        if (!identical(as.double(sum(x_tbl != 1)), 0))
            stop("'x' does not have exactly one 'value' per 'age' * 'sex' * 'time' combination (see ?demog_change_component_df for class definition).")

        ## -------* Spans

        attr_w_span_names <- get_attr_w_span_names()

        for (att in
             attr_w_span_names[attr_w_span_names %in% demog_change_component_dims_x]
             ) {
            ## Create names of the '_span' and '_start' variables for
            ## use later.
            span_name <- paste0(att, "_span")
            start_name <- paste0(att, "_start")

            ## Get the values of the attribute and column from x for
            ## use later.
            span_attr <- attr(x, span_name)
            start_col <- x[[start_name]]

            ## Diffs of unique values
            start_1st_diff <-
                diff(sort(unique(start_col)), differences = 1)

            ## Do the tests now:
            if (!is.numeric(span_attr))
                stop("'", span_name, "' is not numeric.")
            if (!identical(length(span_attr), 1L))
                stop("'", span_name, "' is not of length 1.")

            if (!is.numeric(start_col))
                stop("'x$", start_name, "' is not numeric.")
            if (!identical(as.double(sum(start_1st_diff != span_attr)), 0))
                stop("Spacings between each 'x$", start_name,
                     "' do not equal 'attr(x, \"", span_name, "\")'.")
        }

        ## -------* Age

        if (is_by_age(x)) {
        min_age_start <- get_min_age_in_dims(x)
        if (!all(min_age_start == 0))
            stop("'age_start' does not start at '0' for each time * sex combination.")
        }

        ## -------* Sex

        allowed_sexes <- get_allowed_sexes()
        if (!all(x$sex %in% c("female", "male", "both")))
            stop("Not all 'x$sex' are in '",
                 paste0(allowed_sexes, collapse = "', '"),
            "'; values other than these are not supported.")

    return(x)
}
