
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
#' @param .validate_elements For \dQuote{\code{_list}} objects, should
#'     the individual elements also be validated? Unless you are
#'     \emph{sure}, always leave \code{TRUE} (default).
#' @return Either an error or the object \code{x}.
#' @author Mark Wheldon
#' @name validate_ccmppWPP_object
NULL

#' @export
#' @rdname validate_ccmppWPP_object
validate_ccmppWPP_object <- function(x, ...) {
    UseMethod("validate_ccmppWPP_object")
}

#' @export
#' @rdname validate_ccmppWPP_object
validate_ccmppWPP_object.default <- function(x, ...) {
    stop("'x' is not an object with a valid CCMPP object class. 'class(x) = ",
         class(x),
         "'. Valid classes are '",
         paste(get_all_demog_change_component_df_class_names(),
               collapse = "', '"),
         "'.")
    }

#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.demog_change_component_df <-
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
        if (any(is.na(demog_change_component_dims_x)) || !is.character(demog_change_component_dims_x) ||
            length(demog_change_component_dims_x) < 1 ||
            !all(demog_change_component_dims_x %in% get_all_allowed_dimensions()))
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'dimensions' attribute of 'x' is not valid. 'dimensions' must be in '",
                 paste(get_all_allowed_dimensions(), collapse = ", "),
                 "' and cannot be missing or duplicated. See ?demog_change_component_df for class definition."))

        if (!identical(demog_change_component_dims_x,
                       ensure_these_dimensions_correctly_ordered(demog_change_component_dims_x)))
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'dimensions' attribute of 'x' is not correctly ordered. Must be ",
                                        toString(ensure_these_dimensions_correctly_ordered(demog_change_component_dims_x)),
                                        "."))

        req_attr <- get_req_attr_names()
        if (!all(req_attr %in% names(attributes(x))))
            stop(not_a_valid_object_msg("demog_change_component_df",
                                        "'x' must have attributes '",
                 paste(req_attr, collapse = "', '"),
                 "'; some are missing."))

        ## Check mode of attributes
        stopifnot(check_mode_of_attributes(x))

        value_type <- attr(x, "value_type")
        if (!identical(length(value_type), 1L)) {
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
        if (!identical(length(value_scale), 1L)) {
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

        ## SPANS:
        ## 1. Spans must be numeric (already tested above)

        ## *If* There is an age_span of 1000, it must be the oldest age group
        if (is_by_age(x) && any(x$age_span == 1000)) {
            by_col_names <- sapply(demog_change_component_dims_x,
                                   FUN = "get_df_col_names_for_dimensions", spans = FALSE)
            by_col_names <- by_col_names[!by_col_names %in% "age_start"]
            if (length(by_col_names)) {
                ## E.g., have to do it by 'sex' or by 'indicator'
                dump <- lapply(split(x, x[, by_col_names], drop = TRUE), function(z) {
                    if (!identical(which(z$age_span == 1000), which(z$age_start == max(z$age_start, na.rm = TRUE))))
                        stop(not_a_valid_object_msg("demog_change_component_df",
                                                    "There are 'age_span's of 1000 associated with 'age_start' values that are not the oldest age."))
                })
            } else {
                if (!identical(which(x$age_span == 1000), which(x$age_start == max(x$age_start, na.rm = TRUE))))
                        stop(not_a_valid_object_msg("demog_change_component_df",
                                                    "There are 'age_span's of 1000 associated with 'age_start' values that are not the oldest age."))
            }
        }

        ## SEX:
        ## 1. Levels must be in allowed list
        if (is_by_sex(x)) {
            allowed_sexes <- get_all_allowed_sexes()
            if (!all(x$sex %in% c("female", "male", "both")))
                stop(not_a_valid_object_msg("demog_change_component_df",
                                            "Not all 'x$sex' are in '",
                     paste0(allowed_sexes, collapse = "', '"),
                     "'; values other than these are not supported."))
        }

    return(x)
}


#' @rdname validate_ccmppWPP_object
#' @export
validate_ccmppWPP_object.pop_count_age_sex_reference <- function(x, check_sex_equality_by_time = FALSE, ...) {

    ## Base checks
    x <- NextMethod()

    ## 'value's all non-negative
    if (any(x$value < 0))
        stop(not_a_valid_object_msg("pop_count_age_sex_reference",
                                    "'value' column has negative elements."))

    ## value_type
    val_type <- get_value_types_for_subclass_classes("pop_count_age_sex_reference")
    if (!identical(value_type(x), val_type))
        stop(not_a_valid_object_msg("pop_count_age_sex_reference",
                                    "'value_type' must be \"", val_type, "\"."))

    ## If time_span is meant to be zero check that this is true
    if (!all(x$time_span == 0))
        stop(not_a_valid_object_msg("pop_count_age_sex_reference",
                                    "Objects of this class must have all 'time_span' values = 0."))

    ## Check that male and female are not near-identical
    if (is_by_sex(x)) {
        test_tol <- 0.5 / 100
        test <- sexes_unequal(x, x_name = "reference population counts", tolerance = test_tol,
                              check_by_time = check_sex_equality_by_time)
        if(!isTRUE(test)) warning(test)
    }

    return(x)
}
