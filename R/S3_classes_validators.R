## All validation functions are defined here and use inheritance. This
## is to ensure that the correct validation method is called according
## to the class of the object.

#' Validate objects of class \code{demog_change_component_df}.
#'
#' @description
#' Checks that an object with \code{class} attribute
#' \code{demog_change_component_df} is a valid object of this type.
#'
#' @seealso demog_change_component_df
#'
#' @family demog_change_component_df class non-exported functions
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

        if (!inherits(x, "data.frame"))
            stop("'x' does not inherit from 'data.frame'.")

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
            stop("'x' does not have exactly one 'value' per 'age' * 'sex' * 'time' combination (see ?demog_change_component_df for class definition).")

        ## -------* Sex

        allowed_sexes <- get_allowed_sexes()
        if (!all(x$sex %in% c("female", "male", "both")))
            stop("Not all 'x$sex' are in '",
                 paste0(allowed_sexes, collapse = "', '"),
            "'; values other than these are not supported.")

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
    if (!identical(value_type(x), "rate"))
        stop("'value_type' must be \"rate\".")

    ## non_zero_fert_ages are valid
    if (is_by_age(x)) {
        nzf_ages <- validate_non_zero_fert_ages(x, non_zero_fert_ages(x))
    }

    return(NextMethod())
}
