
validate_ccmpp_in_out <- function(x, obj_class, ...) {

    ## VALUES:
    ## Cannot be 'NA':
    check_value_type_of_value_in_subclass_df(x$value)

    ## ATTRIBUTES:
    ## Extra attributes required

    req_attr <-
        get_req_attr_names_for_subclass_dfs_for_dimensions(demog_change_component_dims(x))
    if (!all(req_attr %in% names(attributes(x))))
        stop(not_a_valid_object_msg(obj_class,
                                    "'x' must have attributes '",
                                    paste(req_attr, collapse = "', '"),
                                    "'; some are missing."))

    ## Check mode of attributes
    stopifnot(check_mode_of_attributes(x,
        modes_df = get_master_df_of_attr_modes_for_subclass_dfs(special_subset = "extra_only")))

    ## MUST BE SORTED:
    ## If not sorted, at least by age and sex within time, the
    ## single-step ccmpp function will turn out incorrect
    ## results. The class imposes full sorting.

    demog_change_component_dims_x <- demog_change_component_dims(x)

    order_cols <-
        subset_master_df_of_dimensions_colnames_coltypes(dimensions = demog_change_component_dims_x)$colname
    if (!identical(x[, order_cols],
                   sort_demog_change_component_df(x)[, order_cols]))
        stop(not_a_valid_object_msg(obj_class,
                                    "'x' must be sorted by indicator, time, rev(sex), age_start (see ?",
                                    obj_class, "ccmpp_input_df for class definition)."))

    ## AGE:
    ## Must start at age 0 within indicator * time * sex

    if (is_by_age(x)) {
        min_age_start <- get_min_age_in_dims_in_df(x)
        if (!all(na.omit(min_age_start) == 0))
            stop(not_a_valid_object_msg(obj_class,
                                        "'age_start' does not start at '0' for each time * sex combination."))
    }

    ## SPANS:
    ## 1. Check 'has_time_span_zero'
    ## 2. Span attributes must be of length 1
    ## 3. Spans must equal row-wise differences between the '_start' columns
    ## 4. Spans must all be equal (row- and column-wise)

    ## If time_span is meant to be zero check that this is true
    if (has_time_span_zero(x) && !all(x$time_span == 0))
        stop(not_a_valid_object_msg(obj_class,
                                    "Objects of this class must have all 'time_span' values = 0."))

    ## Check spans against differences
    stopifnot(verify_spans_equal_start_differences(x, obj_class))

    ## Check that all spans are equal to a common value
    attr_w_span_names <- get_all_dimensions_w_spans()
    attr_w_span_names <-
        attr_w_span_names[attr_w_span_names %in% demog_change_component_dims_x]

    if (has_time_span_zero(x))
        attr_w_span_names <- attr_w_span_names[!attr_w_span_names %in% "time"]

    if (!identical(length(unique(unlist(x[, paste0(attr_w_span_names, "_span")]))), 1L)) {
        msg <- not_a_valid_object_msg(obj_class,
                                      "All spans must be equal to a *single* common value; instead they are:")
        for (att in attr_w_span_names) {
            span_name <- paste0(att, "_span")
            msg <- c(msg,
                     "\n\t", span_name, ":\n",
                     "\t\t",
                     toString(unique(x[, span_name])))
        }
        stop(msg)
    }

    ## --- END 'SPANS'

    ## SQUARENESS: Must not be imbalance in indicator * sex * time *
    ## age combinations. E.g., if 1951, age 5, 'male' exists, _and_
    ## there are other rows for 'female', 1951, age 5 must exist for
    ## 'female' (same for all times, ages, and indicators if present).
    ##
    ## This could be checked with 'tabulate_lexis_squares()' but that
    ## function is slow and, given the tests above, partly
    ## redundant. All that remains is to check that there is a
    ## complete set of time, age, sex cells as implied by unique values.

    ## x_tbl <- tabulate_lexis_squares(x)
    ## if (!identical(as.double(sum(x_tbl != 1)), 0)) {
    ##     indx <- which(x_tbl != 1, arr.ind = TRUE)
    ##     errs <- apply(indx, 1, function(z) {
    ##         mapply(function(x, y) x[[y]],
    ##                dimnames(x_tbl), as.list(z))
    ##     })
    ##     print(errs)
    ##     stop(not_a_valid_object_msg("demog_change_component_df",
    ##                                 "'x' does not have exactly one 'value' per 'age_start' * 'sex' * 'time_start' * 'indicator' combination. Either there are duplicates or some are missing. The combinations with more or less than 1 row are printed above. See ?demog_change_component_df for class definition."))
    ## }

    if (!check_all_demog_dimension_combinations(x))
        stop(not_a_valid_object_msg(obj_class,
                                    "Some combinations of ",
                                    toString(demog_change_component_dims_x),
                                    " are not present in 'x'."))

    ## -------* Return

    return(x)
}
