###-----------------------------------------------------------------------------
### * Helpers

get_allowed_indicator_categories_life_table <- function() {
    c("lt_nMx", "lt_nAx", "lt_nqx", "lt_lx", "lt_ndx", "lt_nLx",
      "lt_Sx", "lt_Tx", "lt_ex")
    }

###-----------------------------------------------------------------------------
### * Class constructors

#' Low-level constructor for class \code{life_table_age_sex}.
#'
#' @description
#' Creates an object of class \code{life_table_age_sex}. Minimal
#' checks are done; for interactive use see
#' \code{\link{life_table_age_sex}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{life_table_age_sex}}.
#'
#' @seealso life_table_age_sex
#'
#' @inheritParams demog_change_component_df
#'
#' @return An object of class \code{life_table_age_sex}.
#' @author Mark Wheldon
new_life_table_age_sex <-
    function(x,
             age_span = double(),
             time_span = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions = get_req_dimensions_for_ccmpp_input_classes("life_table_age_sex"),
                           value_type = get_value_types_for_classes("life_table_age_sex"),
                           ...,
                           class = c(class, "life_table_age_sex"))
    }


#' Constructor for class \code{life_table_age_sex}
#'
#' \code{life_table_age_sex} is a subclass of
#' \code{\link{ccmpp_input_df}}. It has an indicator column that
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{validate_ccmpp_object}} for object validation,
#'     \code{\link{ccmpp_input_df}} for the class from which this one
#'     inherits.
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{life_table_age_sex}.
#' @author Mark Wheldon
#' @export
life_table_age_sex <-
    function(x) {

        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_ccmpp_input_classes("life_table_age_sex"),
                            value_type = get_value_types_for_classes("life_table_age_sex"))

        ## Create/Validate
        validate_ccmpp_object(
            new_life_table_age_sex(li$df,
                               age_span = li$age_span,
                               time_span = li$time_span)
        )
    }


#' Coerce to a \code{life_table_age_sex}
#'
#' These functions coerce an object to a
#' \code{life_table_age_sex} if possible, or check if it is
#' one.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{coerce_demog_change_component_df}}
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_life_table_age_sex
#' @export
as_life_table_age_sex <- function(x, ...) {
    UseMethod("as_life_table_age_sex")
}

#' @rdname coerce_life_table_age_sex
#' @export
as_life_table_age_sex.default <- function(x, ...) {
    if (is_life_table_age_sex(x)) return(x)
    stop("Cannot coerce 'x' to 'life_table_age_sex'.")
}

#' @rdname coerce_life_table_age_sex
#' @export
as_life_table_age_sex.data.frame <- function(x, ...) {
    life_table_age_sex(as.data.frame(x))
}

#' @rdname coerce_life_table_age_sex
#' @export
as_life_table_age_sex.matrix <- function(x, ...) {
    as_life_table_age_sex(as.data.frame(NextMethod()))
}

#' @rdname coerce_life_table_age_sex
#' @export
as_life_table_age_sex.life_table_age_sex <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("life_table_age_sex", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmpp_object(x))
}

#' @rdname coerce_life_table_age_sex
#' @export
is_life_table_age_sex <- function(x) {
    inherits(x, "life_table_age_sex")
}


#' @rdname subset_demog_change_component_df
#' @export
subset_indicator.life_table_age_sex <- function(x, indicators, drop = FALSE) {

    x <- NextMethod()
    return(life_table_age_sex(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_time.life_table_age_sex <- function(x, times, drop = FALSE) {

    x <- NextMethod()
    return(life_table_age_sex(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.life_table_age_sex <- function(x, ages, drop = FALSE) {

    x <- NextMethod()
    return(life_table_age_sex(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.life_table_age_sex <- function(x, sexes, drop = FALSE) {

    x <- NextMethod()
    return(life_table_age_sex(x))
}
