#' Return the \dQuote{span} of a \code{ccmpp_input_df}
#'
#' Returns the common \dQuote{age_span} and \dQuote{time_span} of
#' objects inheriting from \code{ccmpp_input_df}. To be a valid member
#' of this class, these two spans must be equal. This function
#' provides both a convenient check for this equality and a more
#' logical way of accessing the common span. To change the span, see
#' \code{\link{span<-}}.
#'
#' @param x An object inheriting from \code{ccmpp_input_df}.
#' @param ...
#' @return The span of \code{x}.
#' @author Mark Wheldon
#' @seealso \code{\link{extract_demog_change_component_attributes}},
#'     the assignment version for changing the span
#'     \code{\link{span<-}}
#' @name extract_ccmpp_input_df_span
#' @export
span <- function(x, ...) {
    UseMethod("span")
}

#' @rdname extract_ccmpp_input_df_span
#' @export
span.ccmpp_input_df <- function(x, ...) {
    if (!identical(age_span(x), time_span(x)))
        stop("'age_span' and 'time_span' of 'x' are different. This is not a valid 'ccmpp_input_df' object.")
    return(age_span(x))
}



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
    vtx <- get_value_types_for_ccmpp_in_out_classes(oldClass(x)[1])
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
        ann <- get_value_scale_annotations_for_ccmpp_in_out_classes(attr(x, "class_of_df"))
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
