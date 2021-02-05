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
#' @param age_span Scalar indicating the span of the age groups.
#' @param time_span Scalar indicating the span of the time periods.
#' @param value_scale \emph{Numeric} scalar indicating the value_scale of the
#'     counts, e.g., \code{1}, \code{1000}, \code{1e6}, etc.
#' @param dimensions Character vector listing the dimensions such as
#'     \dQuote{time}, \dQuote{age}, \dQuote{sex}.
#' @param value_type Scalar indicating the type of the \dQuote{value}
#'     column (e.g., \dQuote{count}, \dQuote{rate}, etc.).
#' @return An object of class \code{demog_change_component_df}.
#' @author Mark Wheldon
new_demog_change_component_df <-
    function(x, dimensions = character(),
             value_type = character(),
             value_scale = double(),
             ..., class = character()) {
        if (missing(x)) x <- data.frame()
        stopifnot(is.data.frame(x))
        stopifnot(is.character(class))
        structure(x,
                  dimensions = dimensions,
                  ## age_span = age_span,
                  ## time_span = time_span,
                  value_type = value_type,
                  value_scale = value_scale,
                  ...,
                  class = c(class, "demog_change_component_df", "data.frame"))
    }


#' Prepare a data frame for conversion to a \code{demog_change_component_df}
#'
#' Converts and cleans \code{x} into a form suitable for
#' \code{\link{demog_change_component_df}.
#'
#' @inheritParams demog_change_component_df
#' @return A data frame
#' @author Mark Wheldon
prepare_df_for_demog_change_component_df <- function(x,
                                                     dimensions = attr(x, "dimensions"),
                                                     value_type = attr(x, "value_type"),
                                                     value_scale = attr(x, "value_scale")) {

    if (!is.data.frame(x))
        stop("'x' is not a data.frame.")

    ## Coerce so things classes like tibbles are dropped. These cause
    ## problems later on.
    x <- as.data.frame(x)

    coln_x <- colnames(x)

    ## -------* 'dimensions' attribute

    if (is.null(dimensions)) {
        dimensions <- guess_dimensions_from_df_cols(x)
        S3_class_message("Argument 'dimensions' is 'NULL'; setting 'dimensions' to '",
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

    ## -------* Check columns are present

    req_cols_spans <- get_all_req_col_names_excl_spans_for_dimensions(dimensions)

    if (!all(req_cols_spans %in% coln_x))
        stop("'x' must have columns '",
             paste0(req_cols_spans, collapse = "', '"),
             "'; some or all are missing.")

    ## -------* Coerce numeric and character columns

    req_cols <- get_all_req_col_names_for_dimensions(dimensions)
    req_cols_types <- get_all_req_col_types_for_dimensions(dimensions)
    char_cols <- intersect(req_cols[req_cols_types == "character"],
                           coln_x)
    for (j in char_cols) {
        if (!is.character(x[, j])) {
            if (is.factor(x[, j])) x[, j] <- as.character(x[, j])
            else stop("Cannot coerce column '", j, "' to 'character'.")
        }
    }
    num_cols <- intersect(req_cols[req_cols_types == "numeric"],
                          coln_x)
    for (j in num_cols) {
        if (!is.double(x[, j])) {
            if (is.numeric(x[, j]))
                x[, j] <- as.double(x[, j])
            else if (is.factor(x[, j]))
                x[, j] <- as.double(levels(x[, j])[x[, j]])
            else if (is.character(x[, j]))
                x[, j] <- as.double(x[, j])
            else
                stop("Cannot coerce column '", j, "' to 'double'.")
        }
        if (!identical(length(as.double(as.numeric(na.omit(x[, j])))),
                       nrow(x)))
            stop("Coercing column '", j, "' to 'double' resulted in 'NA's.")
    }

    ## -------* Set-up _span columns

    time_span_undefined <- TRUE
    age_span_undefined <- TRUE

    if ("time" %in% dimensions) {
        if ("time_span" %in% coln_x) time_span_undefined <- FALSE
        else {
            diff_x <- diff(unique(x$time_start))[1]
            if (!(is.numeric(diff_x) & is.finite(diff_x))) {
                time_span_undefined <- TRUE
            } else {
                S3_class_message("'time_span' is not a column in 'x'; setting 'x$time_span' to '",
                                 diff_x,
                                 "'.")
                x$time_span <- diff_x
                time_span_undefined <- FALSE
            }
        }
    }

    if ("age" %in% dimensions) {
        if ("age_span" %in% coln_x) age_span_undefined <- FALSE
        else {
            diff_x <- diff(unique(x$age_start))[1]
            if (!(is.numeric(diff_x) & is.finite(diff_x))) {
                age_span_undefined <- TRUE
            } else {
                S3_class_message("'age_span' is not a column in 'x'; setting 'x$age_span' to '",
                                 diff_x,
                                 "'.")
                x$age_span <- diff_x
                age_span_undefined <- FALSE
            }
        }
    }

    ## If only one of age or time span undeterimined, set it equal to
    ## the other.
    if ("age" %in% dimensions && "time" %in% dimensions) {
        if (identical(time_span_undefined + age_span_undefined, 2L))
            stop("'time_span' is not a column in 'x' and cannot determine it from 'x$time_span'.",
                 "\n",
                 "'age_span' is not a column in 'x' and cannot determine it from 'x$age_span'.")
        else if (identical(time_span_undefined + age_span_undefined, 1L)) {
            if (time_span_undefined) {
                x$time_span <- x$age_span
                S3_class_message("'time_span' is not a column in 'x'; setting 'x$time_span' to 'age_span', which is '",
                                 unique(x$age_span)[1],
                                 "'.")
            } else if (age_span_undefined) {
                x$age_span <- x$time_span
                S3_class_message("'age_span' is not a column in 'x'; setting 'x$age_span' to 'time_span', which is '",
                                 unique(x$time_span)[1],
                                 "'.")
            }
        }
    }

### !!!!!!!!!! TEMPORARY !!!!!!!!!!!!
    ## Replace '1000' in x$age_span
    if ("age" %in% dimensions && !is.null(x$age_span) && !is.null(age_span)) {
        x$age_span[x$age_span == 1000] <- unique(x$age_span)[1]
        ##age_span <- age_span[!age_span == 1000]
    }
### !!!!!!!!!! TEMPORARY !!!!!!!!!!!!

    ## -------* Values

    if (is.null(value_type)) {
        if (all(is.na(x$value))) {
            S3_class_message("Argument 'value_type' is 'NULL' and all 'value' column entries are 'NA'; setting 'value_type' to 'real'.")
            value_type <- "real"
        } else if (is.numeric(x$value)) {
            S3_class_message("Argument 'value_type' is 'NULL'; setting 'value_type' to 'real'.")
            value_type <- "real"
        } else if (is.character(x$value)) {
            S3_class_message("Argument 'value_type' is 'NULL'; setting 'value_type' to 'categorical'.")
            value_type <- "categorical"
        }
    }

    ## -------* Value_Scale

    if (is.null(value_scale)) {
        if (value_type %in% get_value_types_w_non_NA_value_scale()) {
            S3_class_message("Argument 'value_scale' is 'NULL'; setting 'value_scale' to '1'.")
            value_scale <- 1
        } else {
            value_scale <- NA
            S3_class_message("Argument 'value_scale' is 'NULL'; setting 'value_scale' to 'NA'.")
        }
    }

    ## -------* Other

    ## Keep only required columns
    x <- x[, req_cols]

    ## Sort by time, then sex, then age
    x <- sort_demog_change_component_df(x)

    ## row.names might have been muddled by sorting so reset
    row.names(x) <- NULL

    return(list(df = x, dimensions = dimensions, value_type = value_type, value_scale = value_scale))
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
#' @param dimensions Character vector listing the dimensions such as
#'     \dQuote{time}, \dQuote{age}, \dQuote{sex}.
#' @param value_type Scalar indicating the type of the \dQuote{value}
#'     column (e.g., \dQuote{count}, \dQuote{rate}, etc.).
#' @param value_scale \emph{Numeric} scalar indicating the value_scale of the
#'     counts, e.g., \code{1}, \code{1000}, \code{1e6}, etc.
#' @param ... Passed to the low-level constructor.
#' @return An object of class \code{demog_change_component_df}.
#' @author Mark Wheldon
#' @export
demog_change_component_df <-
    function(x,
             dimensions = attr(x, "dimensions"),
             value_type = attr(x, "value_type"),
             value_scale = attr(x, "value_scale"),
             ...) {

        li <-
            prepare_df_for_demog_change_component_df(
                x,
                dimensions = dimensions,
                value_type = value_type,
                value_scale = value_scale)

        ## Create/Validate
        validate_ccmpp_object(
            new_demog_change_component_df(li$df,
                              dimensions = li$dimensions,
                              ## age_span = (if (is.null(age_span)) double() else age_span),
                              ## time_span = (if (is.null(time_span)) double() else time_span),
                              value_type = li$value_type,
                              value_scale = li$value_scale,
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
#' @param x An object to coerce or check.
#' @param ... Further arguments passed to specific methods.
#' @inheritParams demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#'
#' @family ccmpp_input_objects
#' @name coerce_demog_change_component_df
#' @export
as_demog_change_component_df <- function(x, ...) {
    UseMethod("as_demog_change_component_df")
}

#' @rdname coerce_demog_change_component_df
#' @export
as_demog_change_component_df.default <- function(x, ...) {
    if (is_demog_change_component_df(x)) return(x)
    stop("Cannot coerce 'x' to 'demog_change_component_df'.")
}

#' @rdname coerce_demog_change_component_df
#' @export
as_demog_change_component_df.data.frame <- function(x, value_type = attr(x, "value_type"),
                                                    value_scale = attr(x, "value_scale"), ...) {
    demog_change_component_df(as.data.frame(x), value_type = value_type, value_scale = value_scale)
}

#' @rdname coerce_demog_change_component_df
#' @export
as_demog_change_component_df.matrix <- function(x, value_type = attr(x, "value_type"),
                                                value_scale = attr(x, "value_scale"), ...) {
    dn <- dimnames(x)
    demog_dim_colnames_df <- get_master_df_of_dimensions_colnames_coltypes()
    if (all(names(dn) %in% demog_dim_colnames_df$colname)) {
        idx <- which(demog_dim_colnames_df$colname %in% names(dn))
        dn_numeric <-
            which(demog_dim_colnames_df[idx, "type"] == "numeric")
        for (j in dn_numeric) {
            if (!identical(length(as.numeric(na.omit(as.numeric(dn[[j]])))),
                           dim(x)[j])) {
                stop("'x' has dimname named '",
                     dn[j],
                     "' which is numeric but the dimnames themselves are not all numeric.")
            }
        }
        df <- data.frame(expand.grid(dimnames(x)), value = c(x))
        return(demog_change_component_df(df))
    } else {
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
        return(as_demog_change_component_df(as.data.frame(x), value_type = value_type, value_scale = value_scale))
    }
}

#' @rdname coerce_demog_change_component_df
#' @export
as_demog_change_component_df.demog_change_component_df <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("demog_change_component_df", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmpp_object(x))
}

#' @rdname coerce_demog_change_component_df
#' @export
is_demog_change_component_df <- function(x) {
    inherits(x, "demog_change_component_df")
}

