###-----------------------------------------------------------------------------
### * Subset

#' @rdname subset_demog_change_component_df
#' @export
subset_indicator.ccmpp_output_df <- function(x, indicators, include = TRUE) {

    x <- NextMethod()
    return(ccmpp_output_df(x, dimensions = demog_change_component_dims(x),
                          value_type = value_type(x)))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_time.ccmpp_output_df <- function(x, times, include = TRUE) {

    x <- NextMethod()
    return(ccmpp_output_df(x, dimensions = demog_change_component_dims(x),
                          value_type = value_type(x)))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.ccmpp_output_df <- function(x, ages, include = TRUE) {

    x <- NextMethod()
    return(ccmpp_output_df(x, dimensions = demog_change_component_dims(x),
                          value_type = value_type(x)))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.ccmpp_output_df <- function(x, sexes, include = TRUE) {

    x <- NextMethod()
    return(ccmpp_output_df(x, dimensions = demog_change_component_dims(x),
                          value_type = value_type(x)))
}
