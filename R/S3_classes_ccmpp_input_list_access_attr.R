###-----------------------------------------------------------------------------
### * Attributes

#' Extract attributes of a \code{ccmpp_input_list}
#'
#' These are convenience functions for accessing specific attributes
#' and dimensions of objects inheriting from
#' \code{ccmpp_input_list}.
#'
#' \code{age_span} and \code{time_span} return the attribute, which
#' are scalars. \code{ages}, \code{times}, \code{sexes} return the
#' levels of these dimensions (see also
#' \code{\link{ages.demog_change_component_df}}). \code{indicators}
#' and \code{value_type} return lists, with one element per element of
#' \code{x}. By default, elements that do not have an
#' \dQuote{indicator} dimension are dropped by \code{indicators}.
#'
#' @param x An object inheriting from \code{ccmpp_input_list}
#' @param drop Logical, for \code{indicators}: should \code{NULL} elements be dropped?
#' @return The requested attribute or dimension levels; see \dQuote{Details}.
#' @author Mark Wheldon
#' @name extract_ccmpp_input_list_attributes
#' @family extract_attributes
NULL

#' @rdname extract_ccmpp_input_list_attributes
#' @export
age_span.ccmpp_input_list <- function(x) {
    attr(x, "age_span")
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
time_span.ccmpp_input_list <- function(x) {
    attr(x, "time_span")
}


###-----------------------------------------------------------------------------
### * Dimensions

#' @rdname extract_ccmpp_input_list_attributes
#' @export
ages.ccmpp_input_list <- function(x) {
    ages(mig_net_count_component(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
non_zero_fert_ages.ccmpp_input_list <- function(x) {
    non_zero_fert_ages(fert_rate_component(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
`non_zero_fert_ages<-.ccmpp_input_list` <- function(x, value) {
    f <- fert_rate_age_f(fert_rate_component(x),
                         non_zero_fert_ages = value)
    ccmpp_input_list(pop_count_age_sex_base = pop_count_base_component(x),
             life_table_age_sex = life_table_component(x),
             fert_rate_age_f = f,
             srb = srb_component(x),
             mig_net_count_age_sex = mig_net_count_component(x),
             mig_net_rate_age_sex = mig_net_rate_component(x),
             mig_net_count_tot_b = mig_net_count_tot_component(x),
             mig_parameter = mig_parameter_component(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
times.ccmpp_input_list <- function(x) {
    times(mig_net_count_component(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
sexes.ccmpp_input_list <- function(x) {
    sexes(mig_net_count_component(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
indicators.ccmpp_input_list <- function(x, drop = TRUE) {
    x <- lapply(x, function(z) {
        if (is_by_indicator(z)) indicators(z)
    })
    if (drop) return(x[!sapply(x, "is.null")])
    else return(x)
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
value_type.ccmpp_input_list <- function(x) {
    lapply(x, "value_type")
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
value_scale.ccmpp_input_list <- function(x) {
    lapply(x, "value_scale")
}


###-----------------------------------------------------------------------------
### * Access Elements

#' Access the \code{ccmpp_input_df} components of a list
#'
#' These functions extract the \code{ccmpp_input_df} elements from an object
#' inheriting from \code{list}.
#'
#' @param x An object of class \code{ccmpp_input_list}
#' @return An object inheriting from \code{ccmpp_input_df}.
#' @author Mark Wheldon
#' @name ccmpp_list_access_elements
NULL

#' @rdname ccmpp_list_access_elements
#' @export
pop_count_base_component <- function(x) {
    UseMethod("pop_count_base_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
pop_count_base_component.ccmpp_input_list <- function(x) {
    x[["pop_count_age_sex_base"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
life_table_component <- function(x) {
    UseMethod("life_table_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
life_table_component.ccmpp_input_list <- function(x) {
    x[["life_table_age_sex"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
survival_ratio_component <- function(x) {
    UseMethod("survival_ratio_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
survival_ratio_component.ccmpp_input_list <- function(x) {
    lt <- life_table_component(x)
    lt[lt$indicator == "lt_Sx",
       c("time_start", "time_span", "sex",
         "age_start", "age_span", "value")]
}

#' @rdname ccmpp_list_access_elements
#' @export
fert_rate_component <- function(x) {
    UseMethod("fert_rate_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
fert_rate_component.ccmpp_input_list <- function(x) {
    x[["fert_rate_age_f"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
srb_component <- function(x) {
    UseMethod("srb_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
srb_component.ccmpp_input_list <- function(x) {
    x[["srb"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
mig_net_count_component <- function(x) {
    UseMethod("mig_net_count_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
mig_net_count_component.ccmpp_input_list <- function(x) {
    x[["mig_net_count_age_sex"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
mig_net_rate_component <- function(x) {
    UseMethod("mig_net_rate_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
mig_net_rate_component.ccmpp_input_list <- function(x) {
    x[["mig_net_rate_age_sex"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
mig_net_count_tot_component <- function(x) {
    UseMethod("mig_net_count_tot_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
mig_net_count_tot_component.ccmpp_input_list <- function(x) {
    x[["mig_net_count_tot_b"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
mig_parameter_component <- function(x) {
    UseMethod("mig_parameter_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
mig_parameter_component.ccmpp_input_list <- function(x) {
    x[["mig_parameter"]]
}
