
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
