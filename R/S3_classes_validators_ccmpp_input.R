#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.ccmpp_input_df <- function(x, ...) {
    ## Common validation for 'ccmpp_input_df' and 'ccmpp_output_df'
    return(validate_ccmpp_in_out(NextMethod(), obj_class = "ccmpp_input_df", ...))
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.fert_rate_age_f <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("fert_rate_age_f",
                                    "'value' column has negative elements."))

    ## value_type
    val_type <- get_value_types_for_subclass_classes("fert_rate_age_f")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("fert_rate_age_f",
                                    "'value_type' must be \"", val_type, "\"."))

    ## non_zero_fert_ages are valid
    if (is_by_age(x)) {
        nzf_ages <- validate_non_zero_fert_ages(x, non_zero_fert_ages(x))
    }

    ## Make sure zero-fert-rate ages actually have zero values
    zero_fert_ages_idx <- !x$age_start %in% nzf_ages
    zero_fert_values <- x[zero_fert_ages_idx, "value"]
    not_zero_idx <-
        round(zero_fert_values, get_non_zero_fert_ages_tolerance_digits()) != 0
    if (sum(not_zero_idx))
        stop(not_a_valid_object_msg("fert_rate_age_f",
             print_non_zero_fert_ages(unique(x$age_start[zero_fert_ages_idx][not_zero_idx])),
             "' have non-zero 'value' for at least some 'time_start's but are not in the reproductive age range '" ,
             print_non_zero_fert_ages(nzf_ages),
             "'."))

    ## Check dimensions
    check_dimensions_for_subclass_df(x)

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.survival_ratio_age_sex <- function(x, check_sex_equality_by_time = FALSE, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all between 0 and 1
    if (any(x$value < 0 | x$value > 1))
        stop(not_a_valid_object_msg("survial_ratio_age_sex",
                                    "'value' column has elements < 0 or > 1."))

    ## value_type
    val_type <- get_value_types_for_subclass_classes("survival_ratio_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("survival_ratio_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

    ## Check dimensions
    check_dimensions_for_subclass_df(x)

    ## Must have 'male' and 'female'
    if (!all(c("male", "female") %in% sexes(x)))
        stop(not_a_valid_object_msg("survival_ratio_age_sex",
                                    "'x' must have data on both 'male' and 'female'."))

    ## Check that male and female are not near-identical
    test <- sexes_unequal(x, x_name = "survival ratios", tol = 1e-4, scale = 1,
                          check_by_time = check_sex_equality_by_time)
    if(!isTRUE(test)) warning(test)

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.mort_rate_age_sex <- function(x, check_sex_equality_by_time = FALSE, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("mort_rate_age_sex",
                                    "'value' column has elements < 0."))

    ## value_type
    val_type <- get_value_types_for_subclass_classes("mort_rate_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("mort_rate_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

    ## Check dimensions
    check_dimensions_for_subclass_df(x)

    ## Must have 'male' and 'female'
    if (!all(c("male", "female") %in% sexes(x)))
        stop(not_a_valid_object_msg("mort_rate_age_sex",
                                    "'x' must have data on both 'male' and 'female'."))

    ## Check that male and female are not near-identical
    test <- sexes_unequal(x, x_name = "mortality rates", tol = 1e-4, scale = 1,
                          check_by_time = check_sex_equality_by_time)
    if(!isTRUE(test)) warning(test)

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.death_probability_age_sex <- function(x, check_sex_equality_by_time = FALSE, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all between 0 and 1
    if (any(x$value < 0 | x$value > 1))
        stop(not_a_valid_object_msg("death_probability_age_sex",
                                    "'value' column has elements < 0 or > 1."))

    ## value_type
    val_type <- get_value_types_for_subclass_classes("death_probability_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("death_probability_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

    ## Check dimensions
    check_dimensions_for_subclass_df(x)

    ## Must have 'male' and 'female'
    if (!all(c("male", "female") %in% sexes(x)))
        stop(not_a_valid_object_msg("death_probability_age_sex",
                                    "'x' must have data on both 'male' and 'female'."))

    ## Check that male and female are not near-identical
    test <- sexes_unequal(x, x_name = "mortality probabilities", tol = 1e-4, scale = 1,
                          check_by_time = check_sex_equality_by_time)
    if(!isTRUE(test)) warning(test)

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.death_count_age_sex <- function(x, check_sex_equality_by_time = FALSE, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("death_count_age_sex",
                                    "'value' column has elements < 0."))

    ## value_type
    val_type <- get_value_types_for_subclass_classes("death_count_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("death_count_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

    ## Check dimensions
    check_dimensions_for_subclass_df(x)

    ## Must have 'male' and 'female'
    if (!all(c("male", "female") %in% sexes(x)))
        stop(not_a_valid_object_msg("death_count_age_sex",
                                    "'x' must have data on both 'male' and 'female'."))

    ## Check that male and female are not near-identical
    test <- sexes_unequal(x, x_name = "death counts", tol = 1e-4, scale = 1,
                          check_by_time = check_sex_equality_by_time)
    if(!isTRUE(test)) warning(test)

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.pop_count_age_sex_base <- function(x, check_sex_equality_by_time = FALSE, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("pop_count_age_sex_base",
                                    "'value' column has negative elements."))

    ## value_type
    val_type <- get_value_types_for_subclass_classes("pop_count_age_sex_base")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("pop_count_age_sex_base",
                                    "'value_type' must be \"", val_type, "\"."))

    ## Check dimensions
    check_dimensions_for_subclass_df(x)

    ## no time dimension
    if (is_by_time(x)) {
    if (!identical(length(unique(x$time_start)), 1L))
        stop(not_a_valid_object_msg("pop_count_age_sex_base",
                                    "'x$time_start' has more than one unique value; 'pop_count_age_sex_base' objects can only refer to a single time period."))
    }

    ## Must have 'male' and 'female'
    if (!all(c("male", "female") %in% sexes(x)))
        stop(not_a_valid_object_msg("pop_count_age_sex_base",
                                    "'x' must have data on both 'male' and 'female'."))

    ## Check that male and female are not near-identical
    test_tol <- 0.5 / 100
    test <- sexes_unequal(x, x_name = "baseline population counts", tolerance = test_tol,
                          check_by_time = check_sex_equality_by_time)
    if(!isTRUE(test)) warning(test)

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.srb <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("srb",
                                    "'value' column has negative elements."))

    ## value_type
    val_type <- get_value_types_for_subclass_classes("srb")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("srb",
                                    "'value_type' must be \"", val_type, "\"."))

    ## Check dimensions
    check_dimensions_for_subclass_df(x)

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.mig_net_rate_age_sex <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_subclass_classes("mig_net_rate_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("mig_net_rate_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

     ## Check dimensions
    check_dimensions_for_subclass_df(x)

    ## Must have 'male' and 'female'
    if (!all(c("male", "female") %in% sexes(x)))
        stop(not_a_valid_object_msg("mig_net_rate_age_sex",
                                    "'x' must have data on both 'male' and 'female'."))

    ## Sanity check magnitudes
    thold <- get_mig_net_rate_value_warning_threshold(x)
    large_vals <- abs(x$value) > thold
    if (sum(large_vals))
        warning("'x' has unusually large values for a net migration rates; absolute values are larger than ",
                thold,
                " in rows:\n\t",
                toString(which(large_vals), width = 40)
                )

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.mig_net_count_age_sex <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_subclass_classes("mig_net_count_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("mig_net_count_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

     ## Check dimensions
    check_dimensions_for_subclass_df(x)

    ## Must have 'male' and 'female'
    if (!all(c("male", "female") %in% sexes(x)))
        stop(not_a_valid_object_msg("mig_net_count_age_sex",
                                    "'x' must have data on both 'male' and 'female'."))

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.mig_net_count_tot_b <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_subclass_classes("mig_net_count_tot_b")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("mig_net_count_tot_b",
                                    "'value_type' must be \"", val_type, "\"."))

     ## Check dimensions
    check_dimensions_for_subclass_df(x)

    ## Check 'sex'
    if (is_by_sex(x)) {
        if (!identical("both", sexes(x)))
            stop(not_a_valid_object_msg("mig_net_count_tot_b",
                                        "'x' must have data only on 'both' sexes."))
    }

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.mig_parameter <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_subclass_classes("mig_parameter")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("mig_parameter",
                                    "'value_type' must be \"", val_type, "\"."))

     ## Check dimensions
    check_dimensions_for_subclass_df(x)

    ## Allowed indicator and value categories
    if (!all(get_required_indicator_categories_mig_parameter() %in% unique(indicators(x))))
        stop(not_a_valid_object_msg("mig_parameter",
                                    "The 'indicator' column must contain all of the following (and no more): '",
             paste(get_required_indicator_categories_mig_parameter(),
                   collapse = ", "),
             "'. 'x' has '",
             toString(unique(x$indicator), 240),
             "'."))
    if (!all(values(x) %in% get_allowed_value_categories_mig_parameter()))
        stop(not_a_valid_object_msg("mig_parameter",
                                    "The only values allowed in the 'value' column are: '",
             paste(get_allowed_value_categories_mig_parameter(),
                   collapse = ", "),
             "'. 'x' has '",
             toString(unique(x$value), 240),
             "'."))

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.life_table_age_sex <- function(x, check_sex_equality_by_time, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("life_table_age_sex",
                                    "'value' column has negative elements."))

    ## value_type
    val_type <- get_value_types_for_subclass_classes("life_table_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("life_table_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

     ## Check dimensions
    check_dimensions_for_subclass_df(x)

    ## Allowed indicator and value categories
    if (!all(get_required_indicator_categories_life_table() %in% unique(indicators(x))))
        stop(not_a_valid_object_msg("life_table_age_sex",
                                    "The 'indicator' column must contain all of the following (and no more): '",
             paste(get_required_indicator_categories_life_table(),
                   collapse = ", "),
             "'. 'x' has '",
             toString(unique(x$indicator), 240),
             "'."))

    ## Must have 'male' and 'female'
    if (!all(c("male", "female") %in% sexes(x)))
        stop(not_a_valid_object_msg("life_table_age_sex",
                                    "'x' must have data on both 'male' and 'female'."))

    ## Check that male and female are not near-identical. Do by type
    ## of indicator (counts, probabilities/proportions, etc)
    ##
    ## 'count' based columns (years, people, person-years)
    test <-
        sexes_unequal(
            subset_indicator.demog_change_component_df(
                x, c("lt_ex", "lt_lx", "lt_ndx", "lt_nLx", "lt_Tx")),
            x_name = "life table quantities", tol = 0.5/100,
                          check_by_time = check_sex_equality_by_time)
    if(!isTRUE(test)) warning(test)
    ##
    ## 'rate/probability' based columns (survival ratios done below)
    test <-
        sexes_unequal(
            subset_indicator.demog_change_component_df(
                x, c("lt_nMx", "lt_nqx")),
            x_name = "life table quantities", tol = 1e-4, scale = 1,
                          check_by_time = check_sex_equality_by_time)
    if(!isTRUE(test)) warning(test)

    ## Check survival ratio sub-table
    check <- survival_ratio_component(x) # will run validation for
                                         # survival ratio via
                                         # 'as_survival_ratio_age_sex'

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.ccmpp_input_list <- function(x, .validate_elements = TRUE, ...) {

    req_el_names <- get_all_required_ccmpp_input_list_element_names()
    req_el_classes <- get_all_required_ccmpp_input_list_element_classes()

    if (!identical(sort(names(x)),
                   sort(req_el_names)))
        stop(not_a_valid_object_msg("ccmpp_input_list",
                                    "'x' must have these elements (no more):\n\t'",
             paste(req_el_names,
                   collapse = "', '"),
             ".\n",
             "Actual names are:\n\t'",
             paste(names(x), collapse = "', '"),
             "'."))

    x_classes <-
        unname(sapply(x, function(z) oldClass(z)[1], USE.NAMES = FALSE))
    if (!identical(sort(x_classes), sort(req_el_classes)))
        stop(not_a_valid_object_msg("ccmpp_input_list",
                                    "Elements of 'x' must have the following classes:\n\t'",
             paste(req_el_classes, collapse = "', '"),
             ".\n",
             "Actual classes are:\n\t'",
             paste(x_classes, collapse = "', '"), "'."))

    ## Check elements
    age_span_attrs <- c()
    age_levels <- list()
    time_span_attrs <- c()
    time_levels <- list()
    value_scale_attrs <- c()

    for (df_nm in req_el_names) {

        ## Validate objects
        if (.validate_elements) {
            test <- tryCatch(validate_ccmppWPP_object(x[[df_nm]]))
            if (identical(class(test), "try-error"))
                stop(not_a_valid_object_msg("ccmpp_input_list",
                                            df_nm, ":\n", strsplit(c(test), " : ")[[1]][2]))
        }

        ## Store 'spans' time and age levels, and value_scales
        if (is_by_age(x[[df_nm]])) {
            age_span_attrs <-
                c(age_span_attrs, setNames(age_span(x[[df_nm]]), df_nm))
            age_levels <- c(age_levels, setNames(list(ages(x[[df_nm]])), df_nm))
        }
        if (is_by_time(x[[df_nm]])) {
            time_span_attrs <-
                c(time_span_attrs, setNames(time_span(x[[df_nm]]), df_nm))
            time_levels <- c(time_levels, setNames(list(times(x[[df_nm]])), df_nm))
        }
        value_scale_attrs <- c(value_scale_attrs, setNames(attr(x[[df_nm]], "value_scale"), df_nm))
                                # use 'attr' because 'value_scale' creates a
                                # special object and don't need that
                                # here.
    }

    ## Check value_scales are conformable
    value_scale_attrs <- value_scale_attrs[!is.na(value_scale_attrs)]
    value_scale_attrs_not_lt <- value_scale_attrs[!grepl("life_table", names(value_scale_attrs))]
    if (!identical(1L, length(unique(value_scale_attrs_not_lt)))) {
        print(value_scale_attrs_not_lt)
        stop(not_a_valid_object_msg("ccmpp_input_list",
                                    "All 'value_scales' must be equal among elements of 'x' (except the life table element). Actual 'value_scale's are printed above."))
    }
    if (!identical(as.double(1e+05),
                   as.double(value_scale_attrs[grepl("life_table", names(value_scale_attrs))])))
        stop(not_a_valid_object_msg("ccmpp_input_list",
                                    "The life table element must have value_scale ",
             1e+05,
             "; actual value_scale is '",
             value_scale_attrs[grepl("life_table", names(value_scale_attrs))],
             "'."))

    ## Check age and time spans are identical and scalar
    if (!identical(length(unique(age_span_attrs)), 1L)) {
        print(age_span_attrs)
        stop(not_a_valid_object_msg("ccmpp_input_list",
                                    "All 'age_span's must be equal among elements of 'x'. Actual 'age_span's are printed above."))
    }
    time_span_attrs_not_pop <-
        time_span_attrs[!grepl("pop_count_age_sex_base", names(time_span_attrs))]
    if (!identical(length(unique(time_span_attrs_not_pop)), 1L)) {
        print(time_span_attrs)
        stop(not_a_valid_object_msg("ccmpp_input_list","All 'time_span's except 'pop_count_age_sex_base must be equal among elements of 'x'. Actual 'time_span's are printed above."))
    }
    if (!identical(unique(time_span_attrs_not_pop), unique(age_span_attrs))) {
        print(age_span_attrs)
        print(time_span_attrs)
        stop(not_a_valid_object_msg("ccmpp_input_list",
                                    "'time_span' must equal 'age_span'. Actual 'time_span' and 'age_span' are printed above."))
    }

    ## Check that all have the same ages
    age_lengths <- sapply(age_levels, "length")
    common_length <- unique(age_lengths)
    if (!identical(length(common_length), 1L)) {
        print(age_lengths)
        stop(not_a_valid_object_msg("ccmpp_input_list",
                                    "All elements that have an \"age\" dimension must have the same number of age groups. Actual number of ages by element are printed above."))
    }
    if (!all(sapply(age_levels[-1], function(z) identical(z, age_levels[[1]])))) {
        print(age_levels)
        stop(not_a_valid_object_msg("ccmpp_input_list",
                                    "All elements that have an \"age\" dimension must have the same age groups ('age_start'). Actual age groups by element are printed above."))
    }

    ## Check that all have the same times
    time_levels_not_pop_count_base <-
        time_levels[-which(names(time_levels) == "pop_count_age_sex_base")]
    time_lengths <-
        sapply(time_levels_not_pop_count_base, "length")
    common_length <- unique(time_lengths)
    if (!identical(length(common_length), 1L)) {
        ptl <- print(time_lengths)
        stop(not_a_valid_object_msg("ccmpp_input_list",
                                    "All elements except \"pop_count_age_sex_base\" that have a \"time\" dimension must have the same number of unique times. Actual number of unique times by element are printed above.", ptl))
    }
    if (!all(sapply(time_levels_not_pop_count_base[-1],
                    function(z) identical(z, time_levels_not_pop_count_base[[1]])))) {
        print(time_levels_not_pop_count_base)
        stop(not_a_valid_object_msg("ccmpp_input_list",
                                    "All elements except \"pop_count_age_sex_base\" that have a \"time\" dimension must have the same unique times ('time_start'). Actual unique times by element are printed above."))
    }

    ## Check that mig_count_age_sex and mig_count_tot_b are consistent with each other
    mig_tot_agg <- collapse_demog_dimension(x$mig_net_count_age_sex, by_dimensions = "time",
                             out_class = "data.frame")
    mig_check <- base::merge(x$mig_net_count_tot_b,
                             mig_tot_agg,
                             by = "time_start",
                             all = FALSE)
    all_eq <- all.equal(mig_check$value.x, mig_check$value.y, tolerance = 1)
    if (!isTRUE(all_eq))
        warning("'mig_net_count_tot_b' is not consistent with totals calculated from 'mig_net_count_tot_b':\n",
                paste(all_eq, collapse = "\n"))

    ## FINISH
    return(x)
}
