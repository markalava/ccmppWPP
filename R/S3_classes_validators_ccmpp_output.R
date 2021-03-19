#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.ccmpp_output_df <- function(x, ...) {

    ## BASE CHECKS:
    ## Run the inherited checks

    x <- NextMethod()

    demog_change_component_dims_x <-
        demog_change_component_dims(x)

    ## ATTRIBUTES:
    ## 1. Extra attributes required

    req_attr <-
        get_req_attr_names_for_ccmpp_in_out_dfs_for_dimensions(demog_change_component_dims_x)
    if (!all(req_attr %in% names(attributes(x))))
        stop(not_a_valid_object_msg("ccmpp_input_df",
                                    "'x' must have attributes '",
             paste(req_attr, collapse = "', '"),
             "'; some are missing."))

    ## SPANS:
    ## 1. Span attributes must be of length 1
    ## 2. Spans must all be equal
    ## 2. Span columns must contain

    attr_w_span_names <- get_all_dimensions_w_spans()
    attr_w_span_names <-
        attr_w_span_names[attr_w_span_names %in% demog_change_component_dims_x]

    span_values <- numeric()

    for (att in attr_w_span_names) {

        ## Create names of the '_span' and '_start' variables for
        ## use later.
        span_name <- paste0(att, "_span")
        start_name <- paste0(att, "_start")

        ## Get the values of the attribute and column from x for
        ## use later.
        span_attr <- attr(x, span_name)
        start_col <- x[[start_name]]

        ## Check length of attribute
        if (!identical(length(span_attr), 1L))
            stop(not_a_valid_object_msg("ccmpp_input_df",
                                        "'", span_name, "' is not of length 1."))

        ## Diffs of unique values
        start_1st_diff <-
            diff(sort(unique(start_col)), differences = 1)
        if (!identical(as.double(sum(start_1st_diff != span_attr)), 0))
            stop(not_a_valid_object_msg("ccmpp_input_df",
                                        "Spacings between each 'x$", start_name,
                 "' do not equal 'attr(x, \"", span_name, "\")'."))

        ## Record span values
        span_values <- c(span_values,
                         c(span_name = span_attr))
    }
    if (!identical(length(unique(span_values)), 1L))
        stop(not_a_valid_object_msg("ccmpp_input_df",
                                    "Spans must all be equal; instead they are '",
             paste0(span_values, collapse = "', '"),
             "'."))

    ## -------* Return

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.pop_count_age_sex <- function(x, ...) {

    ## BASE CHECKS:
    ## Run the inherited checks

    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("pop_count_age_sex",
                                    "'value' column has negative elements."))

    ## value_type
    val_type <- get_value_types_for_ccmpp_in_out_classes("pop_count_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("pop_count_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

    ## Check dimensions
    check_dimensions_for_ccmpp_in_out_df(x)

    return(x)

}
