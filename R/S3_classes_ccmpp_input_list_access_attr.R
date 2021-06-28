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
#' @param x An object inheriting from \code{ccmpp_input_list}, or a
#'     list that can be coerced to a \code{ccmpp_input_list} via
#'     \code{\link{as_ccmpp_input_list}}.
#' @param drop Logical, for \code{indicators}: should \code{NULL}
#'     elements be dropped?
#' @return The requested attribute or dimension levels; see
#'     \dQuote{Details}.
#' @author Mark Wheldon
#' @name extract_ccmpp_input_list_attributes
#' @family extract_attributes
NULL

#' @rdname extract_ccmpp_input_list_attributes
#' @export
age_span.list <- function(x) {
    age_span(as_ccmpp_input_list(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
age_span.ccmpp_input_list <- function(x) {
    attr(x, "age_span")
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
time_span.list <- function(x) {
    time_span(as_ccmpp_input_list(x))
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
ages.list <- function(x) {
    ages(as_ccmpp_input_list(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
ages.ccmpp_input_list <- function(x) {
    ages(mig_net_count_component(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
non_zero_fert_ages.list <- function(x) {
    non_zero_fert_ages(as_ccmpp_input_list(x))
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
times.list <- function(x) {
    times(as_ccmpp_input_list(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
times.ccmpp_input_list <- function(x) {
    times(mig_net_count_component(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
sexes.list <- function(x) {
    sexes(as_ccmpp_input_list(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
sexes.ccmpp_input_list <- function(x) {
    sexes(mig_net_count_component(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
indicators.list <- function(x, drop = TRUE) {
    indicators(as_ccmpp_input_list(x))
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
value_type.list <- function(x) {
    value_type(as_ccmpp_input_list(x))
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
value_type.ccmpp_input_list <- function(x) {
    lapply(x, "value_type")
}

#' @rdname extract_ccmpp_input_list_attributes
#' @export
value_scale.list <- function(x) {
    value_scale(as_ccmpp_input_list(x))
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
#' These functions extract the \code{ccmpp_input_df} elements from an
#' object inheriting from \code{ccmpp_input_list}. These are \emph{not
#' exported}; use the constructor functions instead. For example, to
#' extract fertility rates from \code{ccmpp_input_list}, \code{x}, use
#' \verb{fert_rate_age_f(x)} instead of \verb{fert_rate_component(x)}.
#'
#' @param x An object of class \code{ccmpp_input_list}
#' @return An object inheriting from \code{ccmpp_input_df}.
#' @author Mark Wheldon
#' @name ccmpp_list_access_elements
NULL

###-----------------------------------------------------------------------------
### ** Pop count base

#' @rdname ccmpp_list_access_elements
pop_count_base_component <- function(x) {
    UseMethod("pop_count_base_component")
}

#' @rdname ccmpp_list_access_elements
pop_count_base_component.list <- function(x) {
    pop_count_base_component(as_ccmpp_input_list(x))
}

#' @rdname ccmpp_list_access_elements
pop_count_base_component.ccmpp_input_list <- function(x) {
    x[["pop_count_age_sex_base"]]
}

#' @rdname ccmpp_list_access_elements
`pop_count_base_component<-` <- function(x, value) {
    UseMethod("pop_count_base_component<-")
}
#' @rdname ccmpp_list_access_elements
`pop_count_base_component<-.ccmpp_input_list` <- function(x, value) {
    x[["pop_count_age_sex_base"]] <- value
    as_ccmpp_input_list(x)
}

###-----------------------------------------------------------------------------
### ** life table

#' @rdname ccmpp_list_access_elements
life_table_component <- function(x) {
    UseMethod("life_table_component")
}

#' @rdname ccmpp_list_access_elements
life_table_component.list <- function(x) {
    life_table_component(as_ccmpp_input_list(x))
}

#' @rdname ccmpp_list_access_elements
life_table_component.ccmpp_input_list <- function(x) {
    x[["life_table_age_sex"]]
}

#' @rdname ccmpp_list_access_elements
`life_table_component<-` <- function(x, value) {
    UseMethod("life_table_component<-")
}
#' @rdname ccmpp_list_access_elements
`life_table_component<-.ccmpp_input_list` <- function(x, value) {
    x[["life_table_age_sex"]] <- value
    as_ccmpp_input_list(x)
}

###-----------------------------------------------------------------------------
### ** Survival ratio

#' @rdname ccmpp_list_access_elements
survival_ratio_component <- function(x) {
    UseMethod("survival_ratio_component")
}

#' @rdname ccmpp_list_access_elements
survival_ratio_component.life_table_age_sex <- function(x) {
    as_survival_ratio_age_sex(x[x$indicator == "lt_Sx",
                                c("time_start", "time_span", "sex",
                                  "age_start", "age_span", "value")])
}

#' @rdname ccmpp_list_access_elements
survival_ratio_component.list <- function(x) {
    survival_ratio_component(as_ccmpp_input_list(x))
}

#' @rdname ccmpp_list_access_elements
survival_ratio_component.ccmpp_input_list <- function(x) {
    survival_ratio_component(life_table_component(x))
}

#' @rdname ccmpp_list_access_elements
`survival_ratio_component<-` <- function(x, value) {
    UseMethod("survival_ratio_component<-")
}
#' @rdname ccmpp_list_access_elements
`survival_ratio_component<-.life_table_age_sex` <- function(x, value) {
    value <- as_survival_ratio_age_sex(value)
    x[x$indicator == "lt_Sx", colnames(value)] <- value
    as_life_table_age_sex(x)
    }
#' @rdname ccmpp_list_access_elements
`survival_ratio_component<-.ccmpp_input_list` <- function(x, value) {
    survival_ratio_component(life_table_component(x)) <- value
    as_ccmpp_input_list(x)
}

###-----------------------------------------------------------------------------
### ** Mortality rate

#' @rdname ccmpp_list_access_elements
mortality_rate_component <- function(x) {
    UseMethod("mortality_rate_component")
}

#' @rdname ccmpp_list_access_elements
mortality_rate_component.life_table_age_sex <- function(x) {
    as_mortality_rate_age_sex(x[x$indicator == "lt_nMx",
                                c("time_start", "time_span", "sex",
                                  "age_start", "age_span", "value")])
}

#' @rdname ccmpp_list_access_elements
mortality_rate_component.list <- function(x) {
    mortality_rate_component(as_ccmpp_input_list(x))
}
#' @rdname ccmpp_list_access_elements
mortality_rate_component.ccmpp_input_list <- function(x) {
    mortality_rate_component(life_table_component(x))
}

#' @rdname ccmpp_list_access_elements
`mortality_rate_component<-` <- function(x, value) {
    UseMethod("mortality_rate_component<-")
}
#' @rdname ccmpp_list_access_elements
`mortality_rate_component<-.life_table_age_sex` <- function(x, value) {
    value <- as_mortality_rate_age_sex(value)
    x[x$indicator == "lt_nMx", colnames(value)] <- value
    as_life_table_age_sex(x)
    }
#' @rdname ccmpp_list_access_elements
`mortality_rate_component<-.ccmpp_input_list` <- function(x, value) {
    mortality_rate_component(life_table_component(x)) <- value
    as_ccmpp_input_list(x)
}

###-----------------------------------------------------------------------------
### ** Death probability

#' @rdname ccmpp_list_access_elements
#' @export
death_probability_component <- function(x) {
    UseMethod("death_probability_component")
}
#' @rdname ccmpp_list_access_elements
death_probability_component.life_table_age_sex <- function(x) {
    as_death_probability_age_sex(x[x$indicator == "lt_nqx",
                                c("time_start", "time_span", "sex",
                                  "age_start", "age_span", "value")])
}

#' @rdname ccmpp_list_access_elements
death_probability_component.list <- function(x) {
    death_probability_component(as_ccmpp_input_list(x))
}
#' @rdname ccmpp_list_access_elements
death_probability_component.ccmpp_input_list <- function(x) {
    death_probability_component(life_table_component(x))
}

#' @rdname ccmpp_list_access_elements
`death_probability_component<-` <- function(x, value) {
    UseMethod("death_probability_component<-")
}
#' @rdname ccmpp_list_access_elements
`death_probability_component<-.life_table_age_sex` <- function(x, value) {
    value <- as_death_probability_age_sex(value)
    x[x$indicator == "lt_nqx", colnames(value)] <- value
    as_life_table_age_sex(x)
    }
#' @rdname ccmpp_list_access_elements
`death_probability_component<-.ccmpp_input_list` <- function(x, value) {
    death_probability_component(life_table_component(x)) <- value
    as_ccmpp_input_list(x)
}

###-----------------------------------------------------------------------------
### ** Death count

#' @rdname ccmpp_list_access_elements
death_count_component <- function(x) {
    UseMethod("death_count_component")
}
#' @rdname ccmpp_list_access_elements
death_count_component.life_table_age_sex <- function(x) {
    as_death_count_age_sex(x[x$indicator == "lt_ndx",
                                c("time_start", "time_span", "sex",
                                  "age_start", "age_span", "value")])
}

#' @rdname ccmpp_list_access_elements
death_count_component.list <- function(x) {
    death_count_component(as_ccmpp_input_list(x))
}
#' @rdname ccmpp_list_access_elements
death_count_component.ccmpp_input_list <- function(x) {
    death_count_component(life_table_component(x))
}

#' @rdname ccmpp_list_access_elements
`death_count_component<-` <- function(x, value) {
    UseMethod("death_count_component<-")
}
#' @rdname ccmpp_list_access_elements
`death_count_component<-.life_table_age_sex` <- function(x, value) {
    value <- as_death_count_age_sex(value)
    x[x$indicator == "lt_ndx", colnames(value)] <- value
    as_life_table_age_sex(x)
    }
#' @rdname ccmpp_list_access_elements
`death_count_component<-.ccmpp_input_list` <- function(x, value) {
    death_count_component(life_table_component(x)) <- value
    as_ccmpp_input_list(x)
}

###-----------------------------------------------------------------------------
### ** fert rate

#' @rdname ccmpp_list_access_elements
fert_rate_component <- function(x) {
    UseMethod("fert_rate_component")
}

#' @rdname ccmpp_list_access_elements
fert_rate_component.list <- function(x) {
    fert_rate_component(as_ccmpp_input_list(x))
}
#' @rdname ccmpp_list_access_elements
fert_rate_component.ccmpp_input_list <- function(x) {
    x[["fert_rate_age_f"]]
}

#' @rdname ccmpp_list_access_elements
`fert_rate_component<-` <- function(x, value) {
    UseMethod("fert_rate_component<-")
}
#' @rdname ccmpp_list_access_elements
`fert_rate_component<-.ccmpp_input_list` <- function(x, value) {
    x[["fert_rate_age_f"]] <- value
    as_ccmpp_input_list(x)
}

###-----------------------------------------------------------------------------
### ** srb

#' @rdname ccmpp_list_access_elements
srb_component <- function(x) {
    UseMethod("srb_component")
}

#' @rdname ccmpp_list_access_elements
srb_component.list <- function(x) {
    srb_component(as_ccmpp_input_list(x))
}
#' @rdname ccmpp_list_access_elements
srb_component.ccmpp_input_list <- function(x) {
    x[["srb"]]
}

#' @rdname ccmpp_list_access_elements
`srb_component<-` <- function(x, value) {
    UseMethod("srb_component<-")
}
#' @rdname ccmpp_list_access_elements
`srb_component<-.ccmpp_input_list` <- function(x, value) {
    x[["srb"]] <- value
    as_ccmpp_input_list(x)
}

###-----------------------------------------------------------------------------
### ** mig net count

#' @rdname ccmpp_list_access_elements
mig_net_count_component <- function(x) {
    UseMethod("mig_net_count_component")
}

#' @rdname ccmpp_list_access_elements
mig_net_count_component.list <- function(x) {
    mig_net_count_component(as_ccmpp_input_list(x))
}
#' @rdname ccmpp_list_access_elements
mig_net_count_component.ccmpp_input_list <- function(x) {
    x[["mig_net_count_age_sex"]]
}

#' @rdname ccmpp_list_access_elements
`mig_net_count_component<-` <- function(x, value) {
    UseMethod("mig_net_count_component<-")
}
#' @rdname ccmpp_list_access_elements
`mig_net_count_component<-.ccmpp_input_list` <- function(x, value) {
    x[["mig_net_count_age_sex"]] <- value
    as_ccmpp_input_list(x)
}

###-----------------------------------------------------------------------------
### ** mig net rate

#' @rdname ccmpp_list_access_elements
mig_net_rate_component <- function(x) {
    UseMethod("mig_net_rate_component")
}

#' @rdname ccmpp_list_access_elements
mig_net_rate_component.list <- function(x) {
    mig_net_rate_component(as_ccmpp_input_list(x))
}
#' @rdname ccmpp_list_access_elements
mig_net_rate_component.ccmpp_input_list <- function(x) {
    x[["mig_net_rate_age_sex"]]
}

#' @rdname ccmpp_list_access_elements
`mig_net_rate_component<-` <- function(x, value) {
    UseMethod("mig_net_rate_component<-")
}
#' @rdname ccmpp_list_access_elements
`mig_net_rate_component<-.ccmpp_input_list` <- function(x, value) {
    x[["mig_net_rate_age_sex"]] <- value
    as_ccmpp_input_list(x)
}

###-----------------------------------------------------------------------------
### ** mig net count total

#' @rdname ccmpp_list_access_elements
mig_net_count_tot_component <- function(x) {
    UseMethod("mig_net_count_tot_component")
}


#' @rdname ccmpp_list_access_elements
mig_net_count_tot_component.list <- function(x) {
    mig_net_count_tot_component(as_ccmpp_input_list(x))
}
#' @rdname ccmpp_list_access_elements
mig_net_count_tot_component.ccmpp_input_list <- function(x) {
    x[["mig_net_count_tot_b"]]
}

#' @rdname ccmpp_list_access_elements
`mig_net_count_tot_component<-` <- function(x, value) {
    UseMethod("mig_net_count_tot_component<-")
}
#' @rdname ccmpp_list_access_elements
`mig_net_count_tot_component<-.ccmpp_input_list` <- function(x, value) {
    x[["mig_net_count_tot_b"]] <- value
    as_ccmpp_input_list(x)
}

###-----------------------------------------------------------------------------
### ** mig parameter

#' @rdname ccmpp_list_access_elements
mig_parameter_component <- function(x) {
    UseMethod("mig_parameter_component")
}

#' @rdname ccmpp_list_access_elements
mig_parameter_component.list <- function(x) {
    mig_parameter_component(as_ccmpp_input_list(x))
}
#' @rdname ccmpp_list_access_elements
mig_parameter_component.ccmpp_input_list <- function(x) {
    x[["mig_parameter"]]
}

#' @rdname ccmpp_list_access_elements
`mig_parameter_component<-` <- function(x, value) {
    UseMethod("mig_parameter_component<-")
}
#' @rdname ccmpp_list_access_elements
`mig_parameter_component<-.ccmpp_input_list` <- function(x, value) {
    x[["mig_parameter"]] <- value
    as_ccmpp_input_list(x)
}


#' @rdname ccmpp_list_access_elements
#' @export
mig_assumption.list <- function(x) {
    mig_assumption(as_ccmpp_input_list(x))
}
#' @rdname mig_assumption_extract_and_set
#' @export
mig_assumption.ccmpp_input_list <- function(x) {
    mig_assumption(mig_parameter_component(x))
}

#' @rdname mig_assumption_extract_and_set
#' @export
`mig_assumption<-.ccmpp_input_list` <- function(x, value) {
    stopifnot(value %in% get_allowed_mig_assumptions_mig_parameter())
    mig_assumption(mig_parameter_component(x)) <- value #runs 'as_ccmpp_input_list'
    return(x)
}
