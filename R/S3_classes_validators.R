## All validation functions are defined here and use inheritance. This
## is to ensure that the correct validation method is called according
## to the class of the object.

#' Validate objects of class \code{demog_change_component_df}.
#'
#' @description
#' Checks that an object with \code{class} attribute
#' \code{demog_change_component_df} is a valid object of this type.
#'
#' @seealso \code{\link{demog_change_component_df}} and the other
#'     creator functions (e.g., \code{\link{ccmpp_input_df}},
#'     \code{link{fert_rate_input_df}}, etc.)
#'
#' @param x An object to be validated.
#' @return Either an error or the object \code{x}.
#' @author Mark Wheldon
#' @name validate_ccmpp_object
NULL

#' @export
#' @rdname validate_ccmpp_object
validate_ccmpp_object <- function(x, ...) {
    UseMethod("validate_ccmpp_object")
    }

#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.demog_change_component_df <-
    function(x, ...) {

        if (!inherits(x, "data.frame"))
            stop("'x' does not inherit from 'data.frame'.")

        ## ATTRIBUTES:
        ## 1. The "dimensions" attribute must be formed correctly.
        ## 2. The attributes that go with the dimensions must be present.
        ## 3. The "value_type" attribute must be formed correctly.

        demog_change_component_dims_x <- demog_change_component_dimensions(x)
        if (is.na(demog_change_component_dims_x) || !is.character(demog_change_component_dims_x) ||
            length(demog_change_component_dims_x) < 1 ||
            !all(demog_change_component_dims_x %in% get_all_allowed_dimensions()))
            stop("'dimensions' attribute of 'x' is not valid. 'dimensions' must be in '",
                 paste(get_all_allowed_dimensions(), collapse = ", "),
                 "' and cannot be missing or duplicated. See ?demog_change_component_df for class definition.")

        req_attr <- get_req_attr_names_for_dimensions(demog_change_component_dims_x)
        if (!all(req_attr %in% names(attributes(x))))
            stop("'x' must have attributes '",
                 paste(req_attr, collapse = "', '"),
                 "'; some are missing.")

        value_type <- attr(x, "value_type")
        if (!identical(length(value_type), 1L) || !is.character(value_type)) {
            stop("'value_type' must be a single character string, or 'NULL'.")
        }
        allowed_value_types <- get_all_allowed_value_types()
        if (!(value_type %in% allowed_value_types))
            stop("'value_type' must be one of '",
                 paste(allowed_value_types, collapse = "', '"),
                 "'.")

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
            stop("'x' must have columns '",
                 paste0(req_cols, collapse = "', '"),
                 "'; some or all are missing.")

        if (!all(coln_x %in% req_cols)) {
                                # the converse has already been verified so this is a
                                # test for set equality
            superf_cols <- coln_x[!(coln_x %in% req_cols)]
            stop("'x' has superfluous columns. The following are not permitted: '",
                    paste0(superf_cols, collapse = "', '"),
                    "'.")
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
                stop("'", req_cols[j], "' must be numeric.")
        }
        for (j in char_cols) {
            if (!is.character(x[, req_cols[j]]))
                stop("'", req_cols[j], "' must be character.")
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
                stop("Not all 'x$sex' are in '",
                     paste0(allowed_sexes, collapse = "', '"),
                     "'; values other than these are not supported.")
        }

        ## CHECK SQUARENESS
        ## 1. Must be _one_ value per indicator * time * sex * age combination.

        x_tbl <- tabulate_demog_change_component_df(x)
        if (!identical(as.double(sum(x_tbl != 1)), 0))
            stop("'x' does not have exactly one 'value' per 'age' * 'sex' * 'time' * 'indicator' combination (see ?demog_change_component_df for class definition).")

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.ccmpp_input_df <- function(x, ...) {

    ## BASE CHECKS:
    ## Run the inherited checks

    x <- NextMethod()

    ## ATTRIBUTES:
    ## 1. Extra attributes required

    req_attr <-
        get_req_attr_names_for_ccmpp_input_dfs_for_dimensions(demog_change_component_dimensions(x))
    if (!all(req_attr %in% names(attributes(x))))
        stop("'x' must have attributes '",
             paste(req_attr, collapse = "', '"),
             "'; some are missing.")

    ## MUST BE SORTED:
    ## If not sorted, at least by age and sex within time, the
    ## single-step ccmpp function will turn out incorrect
    ## results. The class imposes full sorting.

    demog_change_component_dims_x <- demog_change_component_dimensions(x)

    order_cols <-
        subset_master_df_of_dimensions_colnames_coltypes(dimensions = demog_change_component_dims_x)$colname
    if (!identical(x[, order_cols],
                   sort_demog_change_component_df(x)[, order_cols]))
        stop("'x' must be sorted by time, rev(sex), age_start (see ?ccmpp_input_df for class definition).")

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
            stop("'", span_name, "' is not of length 1.")

        ## Diffs of unique values
        start_1st_diff <-
            diff(sort(unique(start_col)), differences = 1)
        if (!identical(as.double(sum(start_1st_diff != span_attr)), 0))
            stop("Spacings between each 'x$", start_name,
                 "' do not equal 'attr(x, \"", span_name, "\")'.")

        ## Record span values
        span_values <- c(span_values,
                         c(span_name = span_attr))
    }
    if (!identical(length(unique(span_values)), 1L))
        stop("Spans must all be equal; instead they are '",
             paste0(span_values, collapse = "', '"),
             "'.")

    ## AGE:
    ## 1. Must start at age 0 within indicator * time * sex

    if (is_by_age(x)) {
        min_age_start <- get_min_age_in_dims_in_df(x)
        if (!all(min_age_start == 0))
            stop("'age_start' does not start at '0' for each time * sex combination.")
    }

    ## -------* Return

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.fert_rate_input_df <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_classes("fert_rate_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

    ## non_zero_fert_ages are valid
    if (is_by_age(x)) {
        nzf_ages <- validate_non_zero_fert_ages(x, non_zero_fert_ages(x))
    }

    ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.survival_ratio_input_df <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_classes("survival_ratio_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

    ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.pop_count_base_input_df <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_classes("pop_count_base_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

    ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

    ## no time dimension
    if (is_by_time(x)) {
    if (!identical(length(unique(x$time_start)), 1L))
        stop("'x$time_start' has more than one unique value; 'pop_count_base_input_df' objects can only refer to a single time period.")
    }

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.srb_input_df <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_classes("srb_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

    ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.mig_net_rate_input_df <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_classes("mig_net_rate_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

     ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.mig_net_count_input_df <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_classes("mig_net_count_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

     ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.mig_net_count_tot_input_df <- function(x, ...) {

    ## Base checks
    x <- NextMethod()

    ## value_type
    val_type <- get_value_types_for_classes("mig_net_count_tot_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

     ## Check dimensions
    check_dimensions_for_ccmpp_input_df(x)

    return(x)
}
