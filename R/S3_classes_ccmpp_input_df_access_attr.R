
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





## Value_Scale

#' @rdname extract_demog_change_component_attributes
#' @export
value_scale.ccmpp_input_df <- function(x) {
    new_demog_change_component_df_value_scale(x = attr(x, "value_scale"),
                                        class_of_df = oldClass(x)[1],
                                        value_type = value_type(x),
                                        class = "ccmpp_input_df_value_scale")
}

#' @rdname extract_demog_change_component_attributes
#' @export
print.ccmpp_input_df_value_scale <- function(x, ...) {
    msg <- c("value_scale: ")
    if (!is.na(x)) {
        pref <- get_value_scale_prefixes_for_value_types(attr(x, "value_type"))
        ann <- get_value_scale_annotations_for_ccmpp_input_classes(attr(x, "class_of_df"))
        msg <- c("value_scale: ")
        if (!is.na(pref)) msg <- paste0(msg, pref, " ")
        msg <- paste0(msg, as.character(x))
        if (!is.na(ann)) msg <- paste0(msg, " (", ann, ")")
    } else {
        msg <- paste0(msg, as.character(x))
    }
    cat(msg, "\n")
    return(invisible(x))
}
