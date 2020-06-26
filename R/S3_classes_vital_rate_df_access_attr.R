

###-----------------------------------------------------------------------------
### * Access Attributes

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
    attr_names <-
        attr_names[!(attr_names %in% c("names", "row.names", "class"))]
    return(attrx[attr_names])
}


#' Extract the \dQuote{dimensions} attribute
#'
#' Extracts the non-data.frame attributes. The only method that exists
#' is for \code{vital_rate_df} objects.
#'
#' @param x An object from which to extract attributes.
#' @return The extracted attribute
#' @author Mark Wheldon
#' @export
vital_rate_dimensions <- function(x) {
    UseMethod("vital_rate_dimensions")
}

#' @author Mark Wheldon
#' @export
vital_rate_dimensions.vital_rate_df <- function(x) {
    attr(x, "dimensions")
}


#' Extract specific vital rate attributes
#'
#' These functions extract specific attributes of
#' \code{vital_rate_df}s. Methods for other classes are not defined.
#'
#' @param x An object from which to extract attributes.
#' @return The extracted attribute.
#' @author Mark Wheldon
#' @name extract_vital_rate_attributes
NULL

#' @author Mark Wheldon
#' @rdname extract_vital_rate_attributes
#' @export
age_span <- function(x) {
    UseMethod("age_span")
}

#' @author Mark Wheldon
#' @export
age_span.vital_rate_df <- function(x) {
    if (!is_by_age(x))
        stop("'age' is not a dimension of 'x'.")
    attr(x, "age_span")
}

#' @author Mark Wheldon
#' @rdname extract_vital_rate_attributes
#' @export
time_span <- function(x) {
    UseMethod("time_span")
}

#' @author Mark Wheldon
#' @export
time_span.vital_rate_df <- function(x) {
    if (!is_by_time(x))
        stop("'time' is not a dimension of 'x'.")
    attr(x, "time_span")
}

#' @author Mark Wheldon
#' @rdname extract_vital_rate_attributes
#' @export
sexes <- function(x) {
    UseMethod("sexes")
}

#' @author Mark Wheldon
#' @export
sexes.vital_rate_df <- function(x) {
    if (!is_by_sex(x))
        stop("'sex' is not a dimension of 'x'.")
    levels(factor(x$sex))
}


#' Test for vital rate dimensions
#'
#' These functions test whether an object has a particular vital rate
#' dimension. For objects of class \code{vital_rate_df} this uses the
#' attributes. For data frames it merely checks the presence of the
#' appropriate column.
#'
#' @param x An object to test.
#' @return \code{TRUE} or \code{FALSE} depending on the result.
#' @author Mark Wheldon
#' @name test_vital_rate_dimensions
NULL

#' @author Mark Wheldon
#' @rdname test_vital_rate_dimensions
#' @export
is_by_time <- function(x) {
    UseMethod("is_by_time")
}

#' @author Mark Wheldon
#' @rdname test_vital_rate_dimensions
#' @export
is_by_time.vital_rate_df <- function(x) {
    isTRUE(get_allowed_dimensions()["time"] %in% vital_rate_dimensions(x))
    }

#' @author Mark Wheldon
#' @rdname test_vital_rate_dimensions
#' @export
is_by_time.data.frame <- function(x) {
    time_col_name <- get_attr_col_name("time")
    isTRUE(time_col_name %in% colnames(x))
    }

#' @author Mark Wheldon
#' @rdname test_vital_rate_dimensions
#' @export
is_by_age <- function(x) {
    UseMethod("is_by_age")
}

#' @author Mark Wheldon
#' @rdname test_vital_rate_dimensions
#' @export
is_by_age.vital_rate_df <- function(x) {
    isTRUE(get_allowed_dimensions()["age"] %in% vital_rate_dimensions(x))
}

#' @author Mark Wheldon
#' @rdname test_vital_rate_dimensions
#' @export
is_by_age.data.frame <- function(x) {
    age_col_name <- get_attr_col_name("age")
    isTRUE(age_col_name %in% colnames(x))
    }

#' @author Mark Wheldon
#' @rdname test_vital_rate_dimensions
#' @export
is_by_sex <- function(x) {
    UseMethod("is_by_sex")
}

#' @author Mark Wheldon
#' @rdname test_vital_rate_dimensions
#' @export
is_by_sex.vital_rate_df <- function(x) {
    isTRUE(get_allowed_dimensions()["sex"] %in% vital_rate_dimensions(x))
    }

#' @author Mark Wheldon
#' @rdname test_vital_rate_dimensions
#' @export
is_by_sex.data.frame <- function(x) {
    sex_col_name <- get_attr_col_name("sex")
    isTRUE(sex_col_name %in% colnames(x))
    }
