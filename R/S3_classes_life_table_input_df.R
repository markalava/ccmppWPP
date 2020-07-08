###-----------------------------------------------------------------------------
### * Helpers

get_life_table_allowed_indicator_values <- function() {
    c("lt_nMx", "lt_nAx", "lt_nqx", "lt_lx", "lt_ndx", "lt_nLx",
      "lt_Sx", "lt_Tx", "lt_ex")
    }

###-----------------------------------------------------------------------------
### * Class constructors

#' Low-level constructor for class \code{life_table_input_df}.
#'
#' @description
#' Creates an object of class \code{life_table_input_df}. Minimal
#' checks are done; for interactive use see
#' \code{\link{life_table_input_df}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{life_table_input_df}}.
#'
#' @seealso life_table_input_df
#'
#' @family life_table_input_df class non-exported functions
#'
#' @inheritParams demog_change_component_df
#'
#' @return An object of class \code{life_table_input_df}.
#' @author Mark Wheldon
new_life_table_input_df <-
    function(x,
             age_span = double(),
             time_span = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions = get_req_dimensions_for_ccmpp_input_classes("life_table_input_df"),
                           value_type = get_value_types_for_classes("life_table_input_df"),
                           ...,
                           class = c(class, "life_table_input_df"))
    }


#' Constructor for class \code{life_table_input_df}
#'
#' \code{life_table_input_df} is a subclass of
#' \code{\link{ccmpp_input_df}}. It has an indicator column that
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{validate_ccmpp_object}} for object validation,
#'     \code{\link{ccmpp_input_df}} for the class from which this one
#'     inherits.
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{life_table_input_df}.
#' @author Mark Wheldon
#' @export
life_table_input_df <-
    function(x,
             age_span = attr(x, "age_span"),
             time_span = attr(x, "time_span")) {

        x <- ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_ccmpp_input_classes("life_table_input_df"),
                            age_span = age_span,
                            time_span = time_span,
                            value_type = get_value_types_for_classes("life_table_input_df"))

        ## Create/Validate
        validate_ccmpp_object(
            new_life_table_input_df(x,
                               age_span = attr(x, "age_span"),
                               time_span = attr(x, "time_span"))
        )
    }


#' Coerce to a \code{life_table_input_df}
#'
#' These functions coerce an object to a
#' \code{life_table_input_df} if possible, or check if it is
#' one.
#'
#' @seealso \code{\link{coerce_demog_change_component_df}} for an
#'     important note on validation.
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_life_table_input_df
#' @export
as_life_table_input_df <- function(x, ...) {
    UseMethod("as_life_table_input_df")
}

#' @rdname coerce_life_table_input_df
#' @export
as_life_table_input_df.default <- function(x, ...) {
    if (is_life_table_input_df(x)) return(x)
    stop("Cannot coerce 'x' to 'life_table_input_df'.")
}

#' @rdname coerce_life_table_input_df
#' @export
as_life_table_input_df.data.frame <- function(x, ...) {
    life_table_input_df(as.data.frame(x))
}

#' @rdname coerce_life_table_input_df
#' @export
as_life_table_input_df.matrix <- function(x, ...) {
    as_life_table_input_df(as.data.frame(NextMethod()))
}

#' @rdname coerce_life_table_input_df
#' @export
as_life_table_input_df.life_table_input_df <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("life_table_input_df", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(x)
}

#' @rdname coerce_life_table_input_df
#' @export
is_life_table_input_df <- function(x) {
    inherits(x, "life_table_input_df")
}
