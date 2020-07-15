###-----------------------------------------------------------------------------
### * Helpers

get_all_required_ccmpp_input_list_element_names <- function() {
    c("pop_count_age_sex_base", "life_table_age_sex",
                          "fert_rate_age_f",
                          "srb", "mig_net_count_age_sex",
                          "mig_net_rate_age_sex", "mig_net_count_tot_b",
      "mig_parameter")
}

###-----------------------------------------------------------------------------
### * Class constructors

#' Low-level constructor for class \code{ccmpp_input_list}.
#'
#' @description
#' Creates an object of class \code{ccmpp_input_list}. Minimal
#' checks are done; for interactive use see
#' \code{\link{ccmpp_input_list}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{ccmpp_input_list}}.
#'
#' @seealso ccmpp_input_list
#'
#' @inheritParams new_demog_change_component_df
#' @inheritParams new_fert_rate_input_df
#' @return An object of class \code{ccmpp_input_list}.
#' @author Mark Wheldon


new_ccmpp_input_list <-
    function(x,
             age_span = double(),
             time_span = double(),
             non_zero_fert_ages = double(),
             ..., class = character()) {
        if (missing(x)) x <- list()
        stopifnot(is.list(x))
        stopifnot(is.character(class))
        structure(x,
                  age_span = age_span,
                  time_span = time_span,
                  non_zero_fert_ages = non_zero_fert_ages,
                  ...,
                  class = c(class, "ccmpp_input_list", "list"))
    }


#' Constructor for class \code{ccmpp_input_list}
#'
#' **TBC**
#'
#' @family ccmpp_input_objects
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{ccmpp_input_list}.
#' @author Mark Wheldon
#' @export
ccmpp_input_list <-
    function(pop_count_base_input_df,
             life_table_input_df,
             fert_rate_input_df,
             srb_input_df,
             mig_net_count_input_df,
             mig_net_rate_input_df,
             mig_net_count_tot_input_df,
             mig_parameter_input_df) {

        obj <- list(pop_count_age_sex_base = as_pop_count_base_input_df(pop_count_base_input_df),
                    life_table_age_sex = as_life_table_input_df(life_table_input_df),
                    fert_rate_age_f = as_fert_rate_input_df(fert_rate_input_df),
                    srb = as_srb_input_df(srb_input_df),
                    mig_net_count_age_sex = as_mig_net_count_input_df(mig_net_count_input_df),
                    mig_net_rate_age_sex = as_mig_net_rate_input_df(mig_net_rate_input_df),
                    mig_net_count_tot_b = as_mig_net_count_tot_input_df(mig_net_count_tot_input_df),
                    mig_parameter = as_mig_parameter_input_df(mig_parameter_input_df))

        age_span <- age_span(mig_net_count_input_df)
        time_span <- time_span(mig_net_count_input_df)
        non_zero_fert_ages <- suppressMessages(non_zero_fert_ages(fert_rate_input_df))

        ## Create/Validate
        validate_ccmpp_object(
            new_ccmpp_input_list(obj,
                               age_span = age_span,
                               time_span = time_span,
                               non_zero_fert_ages = non_zero_fert_ages)
        )
    }

###-----------------------------------------------------------------------------
### * Coercion

#' Coerce to a \code{ccmpp_input_list}
#'
#' These functions coerce an object to a
#' \code{ccmpp_input_list} if possible, or check if it is
#' one.
#'
#' @seealso \code{\link{coerce_demog_change_component_df}} for an important note on validation.
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_ccmpp_input_list
#' @export
as_ccmpp_input_list <- function(x, ...) {
    UseMethod("as_ccmpp_input_list")
}

#' @rdname coerce_ccmpp_input_list
#' @export
as_ccmpp_input_list.default <- function(x, ...) {
    if (is_ccmpp_input_list(x)) return(x)
    stop("Cannot coerce 'x' to 'ccmpp_input_list'.")
}

#' @rdname coerce_ccmpp_input_list
#' @export
as_ccmpp_input_list.list <- function(x, ...) {
    ccmpp_input_list(as.list(x))
}

#' @rdname coerce_ccmpp_input_list
#' @export
as_ccmpp_input_list.ccmpp_input_list <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("ccmpp_input_list", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(x)
}

#' @rdname coerce_ccmpp_input_list
#' @export
is_ccmpp_input_list <- function(x) {
    inherits(x, "ccmpp_input_list")
}

###-----------------------------------------------------------------------------
### * Subset

#' @rdname subset_demog_change_component_df
#' @export
subset_indicator.ccmpp_input_list <- function(x, indicator, drop = FALSE) {

    for (df_nm in names(x)) {
        if (is_by_indicator(x[[df_nm]]))
            x[[df_nm]] <- subset_indicator(x[[df_nm]], indicator = indicator, drop = drop)
    }
    return(ccmpp_input_list(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_time.ccmpp_input_list <- function(x, time, drop = FALSE) {

    for (df_nm in names(x)) {
        if (is_by_time(x[[df_nm]]))
            x[[df_nm]] <- subset_time(x[[df_nm]], time = time, drop = drop)
    }
    return(ccmpp_input_list(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.ccmpp_input_list <- function(x, age, drop = FALSE) {

    for (df_nm in names(x)) {
        if (is_by_age(x[[df_nm]]))
            x[[df_nm]] <- subset_age(x[[df_nm]], age = age, drop = drop)
    }
    return(ccmpp_input_list(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.ccmpp_input_list <- function(x, sex, drop = FALSE) {

    for (df_nm in names(x)) {
        if (is_by_sex(x[[df_nm]]))
            x[[df_nm]] <- subset_sex(x[[df_nm]], sex = sex, drop = drop)
    }
    return(ccmpp_input_list(x))
}
