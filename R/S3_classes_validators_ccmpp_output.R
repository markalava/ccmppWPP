#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.ccmpp_output_df <- function(x, ...) {
    ## Common validation for 'ccmpp_input_df' and 'ccmpp_output_df'
    return(validate_ccmpp_in_out(NextMethod(), obj_class = "ccmpp_output_df", ...))

    ## ## Same as ccmpp_input_df
    ## validate_ccmppWPP_object.ccmpp_input_df(x, ...)

    ## ## BASE CHECKS:
    ## ## Run the inherited checks

    ## x <- NextMethod()

    ## demog_change_component_dims_x <-
    ##     demog_change_component_dims(x)

    ## ## ATTRIBUTES:
    ## ## 1. Extra attributes required

    ## req_attr <-
    ##     get_req_attr_names_for_subclass_dfs_for_dimensions(demog_change_component_dims_x)
    ## if (!all(req_attr %in% names(attributes(x))))
    ##     stop(not_a_valid_object_msg("ccmpp_output_df",
    ##                                 "'x' must have attributes '",
    ##          paste(req_attr, collapse = "', '"),
    ##          "'; some are missing."))

    ## ## SPANS:
    ## ## 1. Span attributes must be of length 1
    ## ## 2. Span columns must contain
    ## ## 3. Spans must all be equal

    ## attr_w_span_names <- get_all_dimensions_w_spans()
    ## attr_w_span_names <-
    ##     attr_w_span_names[attr_w_span_names %in% demog_change_component_dims_x]

    ## span_values <- numeric()

    ## for (att in attr_w_span_names) {

    ##     ## Create names of the '_span' and '_start' variables for
    ##     ## use later.
    ##     span_name <- paste0(att, "_span")
    ##     start_name <- paste0(att, "_start")

    ##     ## Get the values of the attribute and column from x for
    ##     ## use later.
    ##     span_attr <- attr(x, span_name)
    ##     start_col <- x[[start_name]]

    ##     ## Check length of attribute
    ##     if (!identical(length(span_attr), 1L))
        ##         stop(not_a_valid_object_msg("ccmpp_output_df",
    ##                                     "'", span_name, "' is not of length 1."))

    ##     ## Spans must be consistent with the differences between the
    ##     ## '_start' column values.
    ##     by_col_names <- sapply(demog_change_component_dims_x,
    ##                                FUN = "get_df_col_names_for_dimensions", spans = FALSE)
    ##         by_col_names <- by_col_names[!by_col_names %in% start_name]
    ##         if (length(by_col_names)) {
    ##         start_vs_span_diff <-
    ##             lapply(split(x[, c(by_col_names, span_name, start_name)], x[, by_col_names], drop = TRUE),
    ##                    function(z) {
    ##                 sum(head(z[, span_name], -1) - diff(z[, start_name], differences = 1))
    ##             })
    ##         } else {
    ##             start_vs_span_diff <-
    ##                 sum(head(x[, span_name], -1) - diff(x[, start_name], differences = 1))
    ##         }
    ##         if (any(unlist(start_vs_span_diff) != 0))
    ##             stop(not_a_valid_object_msg("ccmpp_output_df",
    ##                                     "Spacings between each 'x$", start_name,
    ##              "' do not equal the corresponding values of 'x$", span_name))

    ##     ## Diffs of '_start' column values must equal the value of span attribute
    ##     start_1st_diff <-
    ##         diff(sort(unique(start_col)), differences = 1)
    ##     if (!identical(as.double(sum(start_1st_diff != span_attr)), 0))
    ##         stop(not_a_valid_object_msg("ccmpp_output_df",
    ##                                     "Spacings between each 'x$", start_name,
    ##              "' do not equal 'attr(x, \"", span_name, "\")'."))

    ##     ## Record span values
    ##     span_values <- c(span_values,
    ##                      c(span_name = span_attr))
    ## }
    ## if (!identical(length(unique(span_values)), 1L))
    ##     stop(not_a_valid_object_msg("ccmpp_output_df",
    ##                                 "Spans must all be equal; instead they are '",
    ##          paste0(span_values, collapse = "', '"),
    ##          "'."))

    ## ## -------* Return

    ## return(x)
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
    val_type <- get_value_types_for_subclass_classes("pop_count_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("pop_count_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

    ## Check dimensions
    check_dimensions_for_subclass_df(x)

    return(x)

}
