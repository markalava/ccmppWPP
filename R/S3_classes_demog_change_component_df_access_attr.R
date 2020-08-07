

###-----------------------------------------------------------------------------
### * Access Attributes

#' Extract attributes specific to \code{demog_change_component_df}s
#'
#' All attributes that are not attributes of \code{data.frame} objects
#' are extracted. This function is designed to work with objects of
#' class \code{demog_change_component_df}; behaviour for other classes is
#' not defined.
#'
#' @param x An object from which to extract attributes.
#' @return A list of attributes.
#' @author Mark Wheldon
#' @name demog_change_component_attributes
#' @export
demog_change_component_attributes <- function(x) {
    UseMethod("demog_change_component_attributes")
}

#' @rdname demog_change_component_attributes
#' @export
demog_change_component_attributes.demog_change_component_df <- function(x) {
    attrx <- attributes(x)
    attr_names <- names(attrx)
    attr_names <-
        attr_names[!(attr_names %in% c("names", "row.names", "class"))]
    return(attrx[attr_names])
}


#' Extract the \dQuote{dimensions} attribute
#'
#' Extracts the non-data.frame attributes. The only method that exists
#' is for \code{demog_change_component_df} objects.
#'
#' @param x An object from which to extract attributes.
#' @return The extracted attribute
#' @author Mark Wheldon
#' @name demog_change_component_dimensions
#' @export
demog_change_component_dimensions <- function(x) {
    UseMethod("demog_change_component_dimensions")
}

#' @rdname demog_change_component_dimensions
#' @export
demog_change_component_dimensions.demog_change_component_df <- function(x) {
    attr(x, "dimensions")
}


#' Extract specific components
#'
#' These functions extract specific components (attributes, factor
#' levels, values, etc.) from objects inheriting from
#' \code{demog_change_component_df}s. In general they return the
#' \emph{\dQuote{levels}} of the component, not the whole column from the data
#' frame; see \dQuote{Details} for an important exception.
#'
#' All functions will attempt to return the unique values, or
#' \dQuote{levels}, of the component being extracted. If the whole
#' column is required use the usual subsetting functions such as
#' \code{\link{base::[}}.
#'
#' An important exception is that \code{values} will return the whole
#' column if \code{\link{value_type}} does not return
#' \code{"categorical"}.
#'
#' @param x An object from which to extract attributes.
#' @return The \dQuote{levels} of the extracted component
#'     (except possibly for \code{values}; see \dQuote{Details}).
#' @author Mark Wheldon
#' @name extract_demog_change_component_attributes
#' @family extract_attributes
NULL

#' @rdname extract_demog_change_component_attributes
#' @export
values <- function(x) {
    UseMethod("values")
}

#' @rdname extract_demog_change_component_attributes
#' @export
values.demog_change_component_df <- function(x) {
    if (identical(value_type(x), "categorical"))
        levels(factor(x$value))
    else x$value
}

#' @rdname extract_demog_change_component_attributes
#' @export
ages <- function(x) {
    UseMethod("ages")
}

#' @rdname extract_demog_change_component_attributes
#' @export
ages.demog_change_component_df <- function(x) {
    if (!is_by_age(x))
        stop("'age' is not a dimension of 'x'.")
    unique(x$age_start)
}

#' @rdname extract_demog_change_component_attributes
#' @export
times <- function(x) {
    UseMethod("times")
}

#' @rdname extract_demog_change_component_attributes
#' @export
times.demog_change_component_df <- function(x) {
    if (!is_by_time(x))
        stop("'time' is not a dimension of 'x'.")
    unique(x$time_start)
}

#' @rdname extract_demog_change_component_attributes
#' @export
sexes <- function(x) {
    UseMethod("sexes")
}

#' @rdname extract_demog_change_component_attributes
#' @export
sexes.demog_change_component_df <- function(x) {
    if (!is_by_sex(x))
        stop("'sex' is not a dimension of 'x'.")
    levels(factor(x$sex))
}

#' @rdname extract_demog_change_component_attributes
#' @export
indicators <- function(x) {
    UseMethod("indicators")
}

#' @rdname extract_demog_change_component_attributes
#' @export
indicators.demog_change_component_df <- function(x) {
    if (!is_by_indicator(x))
        stop("'indicator' is not a dimension of 'x'.")
    levels(factor(x$indicator))
}

#' @rdname extract_demog_change_component_attributes
#' @export
value_type <- function(x) {
    UseMethod("value_type")
}

#' @rdname extract_demog_change_component_attributes
#' @export
value_type.demog_change_component_df <- function(x) {
    attr(x, "value_type")
}

#' Test for vital rate dimensions
#'
#' These functions test whether an object has a particular vital rate
#' dimension. For objects of class \code{demog_change_component_df} this uses the
#' attributes. For data frames it merely checks the presence of the
#' appropriate column.
#'
#' @param x An object to test.
#' @return \code{TRUE} or \code{FALSE} depending on the result.
#' @author Mark Wheldon
#' @name test_demog_change_component_dimensions
NULL

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_time <- function(x) {
    UseMethod("is_by_time")
}

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_time.demog_change_component_df <- function(x) {
    isTRUE("time" %in% demog_change_component_dimensions(x))
    }

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_time.data.frame <- function(x) {
    time_col_name <- get_df_col_names_for_dimensions(dimensions = "time", spans = FALSE)
    isTRUE(time_col_name %in% colnames(x))
    }

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_age <- function(x) {
    UseMethod("is_by_age")
}

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_age.demog_change_component_df <- function(x) {
    isTRUE("age" %in% demog_change_component_dimensions(x))
}

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_age.data.frame <- function(x) {
    age_col_name <- get_df_col_names_for_dimensions(dimensions = "age", spans = FALSE)
    isTRUE(age_col_name %in% colnames(x))
    }

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_sex <- function(x) {
    UseMethod("is_by_sex")
}

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_sex.demog_change_component_df <- function(x) {
    isTRUE("sex" %in% demog_change_component_dimensions(x))
    }

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_sex.data.frame <- function(x) {
    sex_col_name <- get_df_col_names_for_dimensions(dimensions = "sex", spans = FALSE)
    isTRUE(sex_col_name %in% colnames(x) &&
           length(unique(x[[sex_col_name]]) > 1))
    }

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_indicator <- function(x) {
    UseMethod("is_by_indicator")
}

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_indicator.demog_change_component_df <- function(x) {
    isTRUE("indicator" %in% demog_change_component_dimensions(x))
    }

#' @rdname test_demog_change_component_dimensions
#' @export
is_by_indicator.data.frame <- function(x) {
    indicator_col_name <- get_df_col_names_for_dimensions(dimensions = "indicator", spans = FALSE)
    isTRUE(indicator_col_name %in% colnames(x) &&
           length(unique(x[[indicator_col_name]]) > 1))
    }
