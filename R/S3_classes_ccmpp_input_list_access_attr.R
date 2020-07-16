###-----------------------------------------------------------------------------
### * Attributes

#' Extract attributes of a \code{ccmpp_input_list}
#'
#' These are convenience functions for accessing specific attributes
#' of objects inheriting from \code{ccmpp_input_list}.
#'
#' @param x An object inheriting from \code{ccmpp_input_list}
#' @return The requested attribute
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
pop_count_base_component.list <- function(x) {
    x[["pop_count_age_sex_base"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
life_table_component <- function(x) {
    UseMethod("life_table_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
life_table_component.list <- function(x) {
    x[["life_table_age_sex"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
fert_rate_component <- function(x) {
    UseMethod("fert_rate_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
fert_rate_component.list <- function(x) {
    x[["fert_rate_age_f"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
srb_component <- function(x) {
    UseMethod("srb_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
srb_component.list <- function(x) {
    x[["srb"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
mig_net_count_component <- function(x) {
    UseMethod("mig_net_count_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
mig_net_count_component.list <- function(x) {
    x[["mig_net_count_age_sex"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
mig_net_rate_component <- function(x) {
    UseMethod("mig_net_rate_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
mig_net_rate_component.list <- function(x) {
    x[["mig_net_rate_age_sex"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
mig_net_count_tot_component <- function(x) {
    UseMethod("mig_net_count_tot_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
mig_net_count_tot_component.list <- function(x) {
    x[["mig_net_count_tot_b"]]
}

#' @rdname ccmpp_list_access_elements
#' @export
mig_parameter_component <- function(x) {
    UseMethod("mig_parameter_component")
}
#' @rdname ccmpp_list_access_elements
#' @export
mig_parameter_component.list <- function(x) {
    x[["mig_parameter"]]
}
