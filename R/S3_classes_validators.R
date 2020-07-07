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

        ## -------* Attributes

        demog_change_component_dims_x <- demog_change_component_dimensions(x)
        if (is.na(demog_change_component_dims_x) || !is.character(demog_change_component_dims_x) ||
            length(demog_change_component_dims_x) < 1 ||
            !all(demog_change_component_dims_x %in% get_allowed_dimensions()))
            stop("'dimensions' attribute of 'x' is not valid. 'dimensions' must be in '",
                 paste(get_allowed_dimensions(), collapse = ", "),
                 "' and cannot be missing or duplicated. See ?demog_change_component_df for class definition.")

        req_attr <- get_req_attr_names(demog_change_component_dims_x)
        if (!all(req_attr %in% names(attributes(x))))
            stop("'x' must have attributes '",
                 paste(req_attr, collapse = "', '"),
                 "'; some are missing.")

        ## -------* Colnames

        coln_x <- colnames(x)
        req_cols <-
            get_all_req_col_names(dimensions = demog_change_component_dims_x)

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

        ## -------* Column types

        req_cols_out_types <-
            get_all_req_col_types(dimensions = demog_change_component_dims_x)
        num_cols <- which(req_cols_out_types == "numeric")
        char_cols <- which(req_cols_out_types == "character")
        for (j in num_cols) {
            if (!is.numeric(x[, j]))
                stop("'", req_cols[j], "' must be numeric.")
        }
        for (j in char_cols) {
            if (!is.character(x[, j]))
                stop("'", req_cols[j], "' must be character.")
        }

        ## -------* Spans

        attr_w_span_names <- get_attr_w_span_names()

        for (att in
             attr_w_span_names[attr_w_span_names %in% demog_change_component_dims_x]
             ) {
            ## Create names of the '_span' and '_start' variables for
            ## use later.
            span_name <- paste0(att, "_span")
            start_name <- paste0(att, "_start")

            ## Get the values of the attribute and column from x for
            ## use later.
            span_attr <- attr(x, span_name)
            start_col <- x[[start_name]]

            ## Do the tests now:
            if (!is.numeric(span_attr))
                stop("'", span_name, "' is not numeric.")

            if (!is.numeric(start_col))
                stop("'x$", start_name, "' is not numeric.")
        }

        ## -------* Values

        value_type <- attr(x, "value_type")

        if (!identical(length(value_type), 1L) || !is.character(value_type)) {
            stop("'value_type' must be a single character string, or 'NULL'.")
        }

        allowed_value_types <- get_allowed_value_types()
        if (!(value_type %in% allowed_value_types))
            stop("'value_type' must be one of '",
                 paste(allowed_value_types, collapse = "', '"),
                 "'.")

        check_value_type(value = x$value, type = value_type)

        ## -------* Check squareness

        x_tbl <- tabulate_demog_change_component_df(x)
        if (!identical(as.double(sum(x_tbl != 1)), 0))
            stop("'x' does not have exactly one 'value' per 'age' * 'sex' * 'time' * 'indicator' combination (see ?demog_change_component_df for class definition).")

        ## -------* Sex

        allowed_sexes <- get_allowed_sexes()
        if (!is.null(x$sex) && !all(x$sex %in% c("female", "male", "both")))
            stop("Not all 'x$sex' are in '",
                 paste0(allowed_sexes, collapse = "', '"),
                 "'; values other than these are not supported.")

        ## -------* Indicator

        if (!is.null(x$indicator) && !is.character(x$indicator) && !is.factor(x$indicator))
            stop("'indicator' must be 'character' or 'factor'.")

    return(x)
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.ccmpp_input_df <- function(x) {

    demog_change_component_dims_x <- demog_change_component_dimensions(x)

    ## -------* Must be Sorted

    ## If not sorted, at least by age and sex within time, the
    ## single-step ccmpp function will turn out incorrect
    ## results. The class imposes full sorting.

    order_cols <-
        get_dim_col_info(dimensions = demog_change_component_dims_x)$colname
    if (!identical(x[, order_cols],
                   sort_demog_change_component_df(x)[, order_cols]))
        stop("'x' must be sorted by time, rev(sex), age_start (see ?ccmpp_input_df for class definition).")

    ## -------* Spans

    attr_w_span_names <- get_attr_w_span_names()

    for (att in
         attr_w_span_names[attr_w_span_names %in% demog_change_component_dims_x]
         ) {
        ## Create names of the '_span' and '_start' variables for
        ## use later.
        span_name <- paste0(att, "_span")
        start_name <- paste0(att, "_start")

        ## Get the values of the attribute and column from x for
        ## use later.
        span_attr <- attr(x, span_name)
        start_col <- x[[start_name]]

        ## Diffs of unique values
        start_1st_diff <-
            diff(sort(unique(start_col)), differences = 1)

        ## Do the tests now:
        if (!identical(length(span_attr), 1L))
            stop("'", span_name, "' is not of length 1.")

        if (!identical(as.double(sum(start_1st_diff != span_attr)), 0))
            stop("Spacings between each 'x$", start_name,
                 "' do not equal 'attr(x, \"", span_name, "\")'.")
    }

    ## -------* Age

    if (is_by_age(x)) {
        min_age_start <- get_min_age_in_dims(x)
        if (!all(min_age_start == 0))
            stop("'age_start' does not start at '0' for each time * sex combination.")
    }

    ## -------* Base checks

    return(NextMethod())
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.fert_rate_input_df <- function(x, ...) {

    ## value_type
    val_type <- get_value_type("fert_rate_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

    ## non_zero_fert_ages are valid
    if (is_by_age(x)) {
        nzf_ages <- validate_non_zero_fert_ages(x, non_zero_fert_ages(x))
    }

    ## no sex dimension
    if (is_by_sex(x) || get_attr_col_name("sex") %in% colnames(x)) {
        stop("Either 'is_by_sex(x)' is 'TRUE' or 'x' has a sex dimension column. Fertility rates for CCMPP must not have sex information.")
    }

    ## no indicator dimension
    if (is_by_indicator(x) || get_attr_col_name("indicator") %in% colnames(x)) {
        stop("Either 'is_by_indicator(x)' is 'TRUE' or 'x' has a indicator dimension column. This type of CCMPP input must not have indicator information.")
    }

    return(NextMethod())
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.survival_ratio_input_df <- function(x, ...) {

    ## value_type
    val_type <- get_value_type("survival_ratio_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

    ## ages start at zero
    if (is_by_age(x)) {
        min_age_start <- get_min_age_in_dims(x)
        if (!all(min_age_start == 0))
            stop("'age_start' does not start at '0' for each time * sex combination.")
        }

    ## no indicator dimension
    if (is_by_indicator(x) || get_attr_col_name("indicator") %in% colnames(x)) {
        stop("Either 'is_by_indicator(x)' is 'TRUE' or 'x' has a indicator dimension column. This type of CCMPP input must not have indicator information.")
    }

    return(NextMethod())
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.pop_count_base_input_df <- function(x, ...) {

    ## value_type
    val_type <- get_value_type("pop_count_base_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

    ## ages start at zero
    if (is_by_age(x)) {
        min_age_start <- get_min_age_in_dims(x)
        if (!all(min_age_start == 0))
            stop("'age_start' does not start at '0' for each time * sex combination.")
        }

    ## no time dimension
    if (is_by_time(x)) {
    if (!identical(length(unique(x$time_start)), 1L))
        stop("'x$time_start' has more than one unique value; 'pop_count_base_input_df' objects can only refer to a single time period.")
    }

    ## no indicator dimension
    if (is_by_indicator(x) || get_attr_col_name("indicator") %in% colnames(x)) {
        stop("Either 'is_by_indicator(x)' is 'TRUE' or 'x' has a indicator dimension column. This type of CCMPP input must not have indicator information.")
    }

    return(NextMethod())
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.srb_input_df <- function(x, ...) {

    ## value_type
    val_type <- get_value_type("srb_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

    ## no age dimension
    if (is_by_age(x) || get_attr_col_name("age") %in% colnames(x)) {
        stop("Either 'is_by_age(x)' is 'TRUE' or 'x' has an age dimension column. SRB for CCMPP must not have age information.")
    }

    ## no sex dimension
    if (is_by_sex(x) || get_attr_col_name("sex") %in% colnames(x)) {
        stop("Either 'is_by_sex(x)' is 'TRUE' or 'x' has a sex dimension column. SRB for CCMPP must not have sex information.")
    }

    ## no indicator dimension
    if (is_by_indicator(x) || get_attr_col_name("indicator") %in% colnames(x)) {
        stop("Either 'is_by_indicator(x)' is 'TRUE' or 'x' has a indicator dimension column. This type of CCMPP input must not have indicator information.")
    }

    return(NextMethod())
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.mig_net_rate_input_df <- function(x, ...) {

    ## value_type
    val_type <- get_value_type("mig_net_rate_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

    ## ages start at zero
    if (is_by_age(x)) {
        min_age_start <- get_min_age_in_dims(x)
        if (!all(min_age_start == 0))
            stop("'age_start' does not start at '0' for each time * sex combination.")
        }

    ## no indicator dimension
    if (is_by_indicator(x) || get_attr_col_name("indicator") %in% colnames(x)) {
        stop("Either 'is_by_indicator(x)' is 'TRUE' or 'x' has a indicator dimension column. This type of CCMPP input must not have indicator information.")
    }

    return(NextMethod())
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.mig_net_count_input_df <- function(x, ...) {

    ## value_type
    val_type <- get_value_type("mig_net_count_input_df")
    if (!identical(value_type(x), val_type))
        stop("'value_type' must be \"", val_type, "\".")

    ## ages start at zero
    if (is_by_age(x)) {
        min_age_start <- get_min_age_in_dims(x)
        if (!all(min_age_start == 0))
            stop("'age_start' does not start at '0' for each time * sex combination.")
        }

    ## no indicator dimension
    if (is_by_indicator(x) || get_attr_col_name("indicator") %in% colnames(x)) {
        stop("Either 'is_by_indicator(x)' is 'TRUE' or 'x' has a indicator dimension column. This type of CCMPP input must not have indicator information.")
    }

    return(NextMethod())
}


#' @rdname validate_ccmpp_object
#' @export
validate_ccmpp_object.mig_net_count_tot_input_df <- function(x, ...) {

    ## no age dimension
    if (is_by_age(x) || get_attr_col_name("age") %in% colnames(x)) {
        stop("Either 'is_by_age(x)' is 'TRUE' or 'x' has an age dimension column. 'mig_net_count_tot_input_df' for CCMPP must not have age information.")
    }

    ## no sex dimension
    if (is_by_sex(x) || get_attr_col_name("sex") %in% colnames(x)) {
        stop("Either 'is_by_sex(x)' is 'TRUE' or 'x' has a sex dimension column. 'mig_net_count_tot_input_df' for CCMPP must not have sex information.")
    }

    ## no indicator dimension
    if (is_by_indicator(x) || get_attr_col_name("indicator") %in% colnames(x)) {
        stop("Either 'is_by_indicator(x)' is 'TRUE' or 'x' has a indicator dimension column. This type of CCMPP input must not have indicator information.")
    }

    return(NextMethod())
}
