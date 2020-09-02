
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
