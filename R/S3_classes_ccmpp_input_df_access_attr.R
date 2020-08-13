
#' @rdname extract_demog_change_component_attributes
#' @export
age_span <- function(x) {
    UseMethod("age_span")
}

#' @rdname extract_demog_change_component_attributes
#' @export
age_span.ccmpp_input_df <- function(x) {
    if (!is_by_age(x))
        stop("'age' is not a dimension of 'x'.")
    attr(x, "age_span")
}

#' @rdname extract_demog_change_component_attributes
#' @export
time_span <- function(x) {
    UseMethod("time_span")
}

#' @rdname extract_demog_change_component_attributes
#' @export
time_span.ccmpp_input_df <- function(x) {
    if (!is_by_time(x))
        stop("'time' is not a dimension of 'x'.")
    attr(x, "time_span")
}

#' @rdname extract_demog_change_component_attributes
#' @export
`value_type<-.ccmpp_input_df` <- function(x, value, ...) {
    vtx <- get_value_types_for_ccmpp_input_classes(oldClass(x)[1])
    if (!is.na(vtx))
        stop("'value_type' of 'x' cannot be changed; it must always be '",
             value_type(x),
             "' for 'x' to remain a valid member of class '",
             oldClass(x)[1],
             "'.")
    as_ccmpp_input_df(NextMethod())
}
