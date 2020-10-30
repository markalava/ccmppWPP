## All validation functions are defined here and use inheritance. This
## is to ensure that the correct validation method is called according
## to the class of the object.

not_a_valid_object_msg <- function(class, ...) {
    paste0("Not a valid '", class, "' object:\n", ...)
}

## Error checking: Report on near equality of sexes
#' @noRd
sexes_unequal <- function(x, x_name = "x", tolerance = 1e-6, scale = NULL,
                          check_by_time = FALSE #<-- with 'FALSE'
                                                #almost doubles time
                                                #for validation; with
                                                #'TRUE' quadruples it
                          ) {
    if (is_by_indicator(x)) {
        indicators_x <- indicators(x)
        msg <- lapply(setNames(indicators_x, indicators_x),
                      FUN = function(this_indicator) {
            y <- subset_indicator.demog_change_component_df(x, indicators = this_indicator,
                                                            include = TRUE, drop = TRUE) #'drop' so that loop over time activates
            sexes_unequal(y, x_name = x_name, tolerance = tolerance, scale = scale)
        })
    } else if (is_by_time(x) && check_by_time) {
        times_x <- times(x)
        msg <- lapply(setNames(times_x, times_x),
                      FUN = function(this_time) {
            ## save time..subset manually
            ## y <- subset_time.demog_change_component_df(x, times = this_time,
            ##                  include = TRUE, drop = TRUE)
            y <- x[x$time_start == this_time,
                   colnames(x)[!colnames(x) %in% get_df_col_names_for_dimensions(dimensions = "time")]]
            sexes_unequal(y, x_name = x_name, tolerance = tolerance, scale = scale)
        })
    } else {
        target <- x[x$sex == "female", "value"]
        current <- x[x$sex == "male", "value"]
        test <- all.equal(target = target, current = current, scale = scale, tolerance = tolerance,
                          countEQ = TRUE)
        if (!isTRUE(test)) return(TRUE)
        else {
            ## If equal, want to know what comparison 'all.equal' actually did.
            ## From 'all.equal.numeric'..
            out <- is.na(target)
            out <- out | target == current
            if (all(out))
                return(paste0("Female and male ",
                              x_name,
                              " are identical."))
            else {
                if (!is.null(scale)) what <- "absolute"
                else {
                    N <- length(out)
                    sabst0 <- sum(abs(target[out]))
                    target <- target[!out]
                    current <- current[!out]
                    if (is.integer(target) && is.integer(current))
                        target <- as.double(target)
                    xy <- sum(abs(target - current))/N
                    what <- {
                        xn <- (sabst0 + sum(abs(target)))/N
                        if (is.finite(xn) && xn > tolerance) {
                            xy <- xy/xn
                            "relative"
                        }
                        else "absolute"
                    }
                }
            }
            return(paste0("Female and male ",
                          x_name,
                          " are very similar; mean ",
                          what,
                          " difference less than ",
                          tolerance, "."))
        }
    }
    if (is.list(msg)) {
        if (suppressWarnings(isTRUE(all(unlist(msg))))) return(TRUE)
        else {
            ## Discard the 'TRUE' elements
            msg <- rapply(msg, function(z) if (isTRUE(z)) { NULL } else { z })
            ## Format: summarize duplicates
            list_to_txt <- function(w) {
                w <- w[!is.na(w)]
                fmtd_txt <- character(0)
                for (uw in unique(w)) {
                    fmtd_txt <- paste0(fmtd_txt,
                                       toString(names(w == uw), width = getOption("width")),
                                       "\n",
                                       "    ",
                                       uw)
                }
                return(paste0(fmtd_txt, "\n"))
            }
            return(list_to_txt(msg))
        }
    } else return(msg) #not a list.. not sure what would trigger this but cover the possibility
}

#' Validate objects of class \code{demog_change_component_df}.
#'
#' Checks that an object with \code{class} attribute
#' \code{demog_change_component_df} is a valid object of this type.
#'
#' An additional check for near equality of values for sexes is done
#' for \code{survival_ratio_age_sex}, \code{pop_count_age_sex_base},
#' and \code{life_table_age_sex} objects. This is done by applying
#' \code{\link{all.equal}} to all values (by \dQuote{indicator} for
#' \code{life_table_age_sex} objects). This can be done separately by
#' \dQuote{time} for a more precise check but it is very
#' time-consuming so is switched off by default. Set
#' \code{check_sex_equality_by_time} to \code{TRUE} to turn it on.
#'
#' @seealso \code{\link{demog_change_component_df}} and the other
#'     creator functions (e.g., \code{\link{ccmpp_input_df}},
#'     \code{link{fert_rate_age_f}}, etc.)
#'
#' @param x An object to be validated.
#' @param check_sex_equality_by_time Logical; for
#'     \code{survival_ratio_age_sex}, \code{pop_count_age_sex_base},
#'     and \code{life_table_age_sex} objects only. See
#'     \dQuote{Details}.
#' @return Either an error or the object \code{x}.
#' @author Mark Wheldon
#' @name validate_ccmpp_object
NULL

#' @export
#' @rdname validate_ccmpp_object
validate_ccmpp_object <- function(x, ...) {
    UseMethod("validate_ccmpp_object")
}

#' @export
#' @rdname validate_ccmpp_object
validate_ccmpp_object.default <- function(x, ...) {
    stop("'x' is not an object with a valid CCMPP object class. 'class(x) = ",
         class(x),
         "'. Valid classes are '",
         paste(get_all_demog_change_component_df_class_names(),
               collapse = "', '"),
         "'.")
    }

#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.demog_change_component_df <-
    function(x, ...) {

        if (!inherits(x, "data.frame"))
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'x' does not inherit from 'data.frame'."))

        ## ATTRIBUTES:
        ## 1. The "dimensions" attribute must be formed correctly.
        ## 2. The attributes that go with the dimensions must be present.
        ## 3. The "value_type" attribute must be formed correctly.
        ## 4. The "value_scale" attribute must be formed correctly.

        demog_change_component_dims_x <- demog_change_component_dims(x)
        if (is.na(demog_change_component_dims_x) || !is.character(demog_change_component_dims_x) ||
            length(demog_change_component_dims_x) < 1 ||
            !all(demog_change_component_dims_x %in% get_all_allowed_dimensions()))
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'dimensions' attribute of 'x' is not valid. 'dimensions' must be in '",
                 paste(get_all_allowed_dimensions(), collapse = ", "),
                 "' and cannot be missing or duplicated. See ?demog_change_component_df for class definition."))

        req_attr <- get_req_attr_names_for_dimensions(demog_change_component_dims_x)
        if (!all(req_attr %in% names(attributes(x))))
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'x' must have attributes '",
                 paste(req_attr, collapse = "', '"),
                 "'; some are missing."))

        value_type <- attr(x, "value_type")
        if (!identical(length(value_type), 1L) || !is.character(value_type)) {
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'value_type' must be a single character string."))
        }
        allowed_value_types <- get_all_allowed_value_types()
        if (!(value_type %in% allowed_value_types))
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'value_type' must be one of '",
                 paste(allowed_value_types, collapse = "', '"),
                 "'."))

        value_scale <- attr(x, "value_scale")
        if (!identical(length(value_scale), 1L) ||
            !(is.numeric(value_scale) || is.na(value_scale))
        ) {
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'value_scale' must be a numeric scalar or 'NA'."))
        }
        vt_nonNA_value_scale <- get_value_types_w_non_NA_value_scale()
        if (!is.na(value_scale) && !value_type %in% vt_nonNA_value_scale)
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'value_type' is '", value_type, "' but 'value_scale' is '",
                 value_scale, "'. Only value types ",
                 toString(vt_nonNA_value_scale),
                 " can have non-NA 'value_scale'."))

        ## VALUE COLUMN
        ## 1. The value column must conform to the stated "value_type"

        check_value_type_of_value_in_df(value = x$value, type = value_type)

        ## COLUMN NAMES:
        ## 1. The columns that go with the dimensions must be present.
        ## 2. No superfluous columns are allowed. The dimensions
        ##    entirely determine the permitted columns.

        coln_x <- colnames(x)
        req_cols <-
            get_all_req_col_names_for_dimensions(dimensions = demog_change_component_dims_x)

        if (!all(req_cols %in% coln_x))
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'x' must have columns '",
                 paste0(req_cols, collapse = "', '"),
                 "'; some or all are missing."))

        if (!all(coln_x %in% req_cols)) {
                                # the converse has already been verified so this is a
                                # test for set equality
            superf_cols <- coln_x[!(coln_x %in% req_cols)]
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'x' has superfluous columns. The following are not permitted: '",
                    paste0(superf_cols, collapse = "', '"),
                    "'."))
        }

        ## COLUMN TYPES:
        ## 1. The columns must have the correct data type, 'numeric' or 'character'.
        ## 2. 'span_...' columns must be numeric
        ## 2. 'sex' column must be valid

        req_cols_out_types <-
            get_all_req_col_types_for_dimensions(dimensions = demog_change_component_dims_x)

        num_cols <- which(req_cols_out_types == "numeric")
        char_cols <- which(req_cols_out_types == "character")
        for (j in num_cols) {
            if (!is.numeric(x[, req_cols[j]]))
                stop(not_a_valid_object_msg("demog_change_component_df",
                                            "'", req_cols[j], "' must be numeric."))
        }
        for (j in char_cols) {
            if (!is.character(x[, req_cols[j]]))
                stop(not_a_valid_object_msg("demog_change_component_df",
                                            "'", req_cols[j], "' must be character."))
        }

        ## attr_w_span_names <- get_all_dimensions_w_spans()
        ## attr_w_span_names <-
        ##     attr_w_span_names[attr_w_span_names %in% demog_change_component_dims_x]
        ## for (att in attr_w_span_names) {
        ##     ## Create names of the '_span' and '_start' variables for
        ##     ## use later.
        ##     span_name <- paste0(att, "_span")
        ##     start_name <- paste0(att, "_start")

        ##     ## Get the values of the attribute and column from x for
        ##     ## use later.
        ##     span_attr <- attr(x, span_name)
        ##     start_col <- x[[start_name]]

        ##     ## Do the tests now:
        ##     if (!is.numeric(span_attr))
        ##         stop("'", span_name, "' is not numeric.")

        ##     if (!is.numeric(start_col))
        ##         stop("'x$", start_name, "' is not numeric.")
        ## }

        if (is_by_sex(x)) {
            allowed_sexes <- get_all_allowed_sexes()
            if (!all(x$sex %in% c("female", "male", "both")))
                stop(not_a_valid_object_msg("demog_change_component_df",
                                            "Not all 'x$sex' are in '",
                     paste0(allowed_sexes, collapse = "', '"),
                     "'; values other than these are not supported."))
        }

        ## CHECK SQUARENESS
        ## 1. Must be _one_ value per indicator * time * sex * age combination.

        x_tbl <- tabulate_demog_change_component_df(x)
        if (!identical(as.double(sum(x_tbl != 1)), 0))
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'x' does not have exactly one 'value' per 'age' * 'sex' * 'time' * 'indicator' combination (see ?demog_change_component_df for class definition)."))

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.ccmpp_input_df <- function(x, ...) {

    ## BASE CHECKS:
    ## Run the inherited checks

    x <- NextMethod()

    ## VALUES:
    ## 1. Cannot be 'NA':
    check_value_type_of_value_in_ccmpp_input_df(x$value)

    ## ATTRIBUTES:
    ## 1. Extra attributes required

    req_attr <-
        get_req_attr_names_for_ccmpp_input_dfs_for_dimensions(demog_change_component_dims(x))
    if (!all(req_attr %in% names(attributes(x))))
        stop(not_a_valid_object_msg("ccmpp_input_df",
                                    "'x' must have attributes '",
             paste(req_attr, collapse = "', '"),
             "'; some are missing."))

    ## MUST BE SORTED:
    ## If not sorted, at least by age and sex within time, the
    ## single-step ccmpp function will turn out incorrect
    ## results. The class imposes full sorting.

    demog_change_component_dims_x <- demog_change_component_dims(x)

    order_cols <-
        subset_master_df_of_dimensions_colnames_coltypes(dimensions = demog_change_component_dims_x)$colname
    if (!identical(x[, order_cols],
                   sort_demog_change_component_df(x)[, order_cols]))
        stop(not_a_valid_object_msg("ccmpp_input_df",
                                    "'x' must be sorted by indicator, time, rev(sex), age_start (see ?ccmpp_input_df for class definition)."))

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

    ## AGE:
    ## 1. Must start at age 0 within indicator * time * sex

    if (is_by_age(x)) {
        min_age_start <- get_min_age_in_dims_in_df(x)
        if (!all(min_age_start == 0))
            stop(not_a_valid_object_msg("ccmpp_input_df",
                                        "'age_start' does not start at '0' for each time * sex combination."))
    }

    ## -------* Return

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.fert_rate_age_f <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("fert_rate_age_f",
                                    "'value' column has negative elements."))

    ## value_type
    val_type <- get_value_types_for_ccmpp_input_classes("fert_rate_age_f")
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
    check_dimensions_for_ccmpp_input_df(x)

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.survival_ratio_age_sex <- function(x, check_sex_equality_by_time = FALSE, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all between 0 and 1
    if (any(x$value < 0 | x$value > 1))
        stop(not_a_valid_object_msg("survial_ratio_age_sex",
                                    "'value' column has elements < 0 or > 1."))

    ## value_type
    val_type <- get_value_types_for_ccmpp_input_classes("survival_ratio_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("survival_ratio_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

    ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

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


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.pop_count_age_sex_base <- function(x, check_sex_equality_by_time = FALSE, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("pop_count_age_sex_base",
                                    "'value' column has negative elements."))

    ## value_type
    val_type <- get_value_types_for_ccmpp_input_classes("pop_count_age_sex_base")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("pop_count_age_sex_base",
                                    "'value_type' must be \"", val_type, "\"."))

    ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

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


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.srb <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("srb",
                                    "'value' column has negative elements."))

    ## value_type
    val_type <- get_value_types_for_ccmpp_input_classes("srb")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("srb",
                                    "'value_type' must be \"", val_type, "\"."))

    ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.mig_net_rate_age_sex <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_ccmpp_input_classes("mig_net_rate_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("mig_net_rate_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

     ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

    ## Must have 'male' and 'female'
    if (!all(c("male", "female") %in% sexes(x)))
        stop(not_a_valid_object_msg("mig_net_rate_age_sex",
                                    "'x' must have data on both 'male' and 'female'."))

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.mig_net_count_age_sex <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_ccmpp_input_classes("mig_net_count_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("mig_net_count_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

     ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

    ## Must have 'male' and 'female'
    if (!all(c("male", "female") %in% sexes(x)))
        stop(not_a_valid_object_msg("mig_net_count_age_sex",
                                    "'x' must have data on both 'male' and 'female'."))

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.mig_net_count_tot_b <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_ccmpp_input_classes("mig_net_count_tot_b")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("mig_net_count_tot_b",
                                    "'value_type' must be \"", val_type, "\"."))

     ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

    ## Check 'sex'
    if (is_by_sex(x)) {
        if (!identical("both", sexes(x)))
            stop(not_a_valid_object_msg("mig_net_count_tot_b",
                                        "'x' must have data only on 'both' sexes."))
    }

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.mig_parameter <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_ccmpp_input_classes("mig_parameter")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("mig_parameter",
                                    "'value_type' must be \"", val_type, "\"."))

     ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

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


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.life_table_age_sex <- function(x, check_sex_equality_by_time, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("life_table_age_sex",
                                    "'value' column has negative elements."))

    ## value_type
    val_type <- get_value_types_for_ccmpp_input_classes("life_table_age_sex")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("life_table_age_sex",
                                    "'value_type' must be \"", val_type, "\"."))

     ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

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


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.ccmpp_input_list <- function(x, ...) {

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
        test <- tryCatch(validate_ccmpp_object(x[[df_nm]]))
        if (identical(class(test), "try-error"))
            stop(not_a_valid_object_msg("ccmpp_input_list",
                                        df_nm, ":\n", strsplit(c(test), " : ")[[1]][2]))

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
        print(age_spans_df)
        stop(not_a_valid_object_msg("ccmpp_input_list",
                                    "All 'age_span's must be equal among elements of 'x'. Actual 'age_span's are printed above."))
    }
    if (!identical(length(unique(time_span_attrs)), 1L)) {
        print(time_spans_df)
        stop(not_a_valid_object_msg("ccmpp_input_list","All 'time_span's must be equal among elements of 'x'. Actual 'time_span's are printed above."))
    }
    if (!identical(time_span_attrs[1], age_span_attrs[1])) {
        print(time_spans_df)
        print(age_spans_df)
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
    mig_tot_agg <- aggregate(x$mig_net_count_age_sex, by = "time")
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
