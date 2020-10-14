
###-----------------------------------------------------------------------------
### * Subset

#' @rdname subset_demog_change_component_df
#' @export
subset_indicator.ccmpp_input_df <- function(x, indicators, drop = FALSE) {

    x <- NextMethod()
    return(ccmpp_input_df(x, dimensions = demog_change_component_dims(x),
                          value_type = value_type(x)))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_time.ccmpp_input_df <- function(x, times, drop = FALSE) {

    x <- NextMethod()
    return(ccmpp_input_df(x, dimensions = demog_change_component_dims(x),
                          value_type = value_type(x)))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.ccmpp_input_df <- function(x, ages, drop = FALSE) {

    x <- NextMethod()
    return(ccmpp_input_df(x, dimensions = demog_change_component_dims(x),
                          value_type = value_type(x)))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.ccmpp_input_df <- function(x, sexes, drop = FALSE) {

    x <- NextMethod()
    return(ccmpp_input_df(x, dimensions = demog_change_component_dims(x),
                          value_type = value_type(x)))
}

###-----------------------------------------------------------------------------
### * Transform

###-----------------------------------------------------------------------------
### ** Re-span

#' Change the span of a \code{ccmpp_input_df} object
#'
#' This is a generic function with methods for the various sub-classes
#' of \code{ccmpp_input_df}. Valid objects inheriting from
#' \code{ccmpp_input_df} have a common value for \dQuote{age_span} and
#' \dQuote{time_span}. This function provides a way of coherently
#' chaning this value, ensuring that the result is still a valid
#' member of its class.
#'
#' @param x An object inheriting from \code{\link{ccmpp_input_df}}.
#' @param value A numeric scalar for the new span.
#' @param ... Passed to or from other methods.
#' @return \code{x} with spans set to \code{value}.
#' @name change_ccmpp_input_df_span
#' @author Mark Wheldon
#' @export
`span<-` <- function(x, value, ...) {
    UseMethod("span<-")
}

#' @rdname change_ccmpp_input_df_span
#' @export
`span<-.ccmpp_input_df` <- function(x, value, ...) {
    stop("Not yet implemented")
}
