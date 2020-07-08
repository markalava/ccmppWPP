

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


#' Extract specific attributes
#'
#' These functions extract specific attributes of
#' \code{demog_change_component_df}s. Methods for other classes are not defined.
#'
#' @param x An object from which to extract attributes.
#' @return The extracted attribute.
#' @author Mark Wheldon
#' @name extract_demog_change_component_attributes
NULL

#' @rdname extract_demog_change_component_attributes
#' @export
age_span <- function(x) {
    UseMethod("age_span")
}

#' @rdname extract_demog_change_component_attributes
#' @export
age_span.demog_change_component_df <- function(x) {
    if (!is_by_age(x))
        stop("'age' is not a dimension of 'x'.")
    attr(x, "age_span")
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
time_span <- function(x) {
    UseMethod("time_span")
}

#' @rdname extract_demog_change_component_attributes
#' @export
time_span.demog_change_component_df <- function(x) {
    if (!is_by_time(x))
        stop("'time' is not a dimension of 'x'.")
    attr(x, "time_span")
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

#' @rdname extract_demog_change_component_attributes
#' @export
non_zero_fert_ages <- function(x) {
    UseMethod("non_zero_fert_ages")
}

#' @rdname extract_demog_change_component_attributes
#' @export
non_zero_fert_ages.fert_rate_input_df <- function(x) {
    attr(x, "non_zero_fert_ages")
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

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_time <- function(x) {
    UseMethod("is_by_time")
}

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_time.demog_change_component_df <- function(x) {
    isTRUE(get_all_allowed_dimensions()["time"] %in% demog_change_component_dimensions(x))
    }

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_time.data.frame <- function(x) {
    time_col_name <- get_df_col_namees_for_dimensions("time")
    isTRUE(time_col_name %in% colnames(x))
    }

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_age <- function(x) {
    UseMethod("is_by_age")
}

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_age.demog_change_component_df <- function(x) {
    isTRUE(get_all_allowed_dimensions()["age"] %in% demog_change_component_dimensions(x))
}

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_age.data.frame <- function(x) {
    age_col_name <- get_df_col_namees_for_dimensions("age")
    isTRUE(age_col_name %in% colnames(x))
    }

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_sex <- function(x) {
    UseMethod("is_by_sex")
}

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_sex.demog_change_component_df <- function(x) {
    isTRUE(get_all_allowed_dimensions()["sex"] %in% demog_change_component_dimensions(x))
    }

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_sex.data.frame <- function(x) {
    sex_col_name <- get_df_col_namees_for_dimensions("sex")
    isTRUE(sex_col_name %in% colnames(x) &&
           length(unique(x[[sex_col_name]]) > 1))
    }

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_indicator <- function(x) {
    UseMethod("is_by_indicator")
}

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_indicator.demog_change_component_df <- function(x) {
    isTRUE(get_all_allowed_dimensions()["indicator"] %in% demog_change_component_dimensions(x))
    }

#' @author Mark Wheldon
#' @rdname test_demog_change_component_dimensions
#' @export
is_by_indicator.data.frame <- function(x) {
    indicator_col_name <- get_df_col_namees_for_dimensions("indicator")
    isTRUE(indicator_col_name %in% colnames(x) &&
           length(unique(x[[indicator_col_name]]) > 1))
    }
