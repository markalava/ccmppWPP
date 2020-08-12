###-----------------------------------------------------------------------------
### * Helpers

get_allowed_value_categories_mig_parameter <- function() {
    c("counts", "end")
}

get_required_indicator_categories_mig_parameter <- function() {
    c("mig_type", "mig_assumption")
}

###-----------------------------------------------------------------------------
### * Constructors, etc.

#' Low-level constructor for class \code{mig_parameter}.
#'
#' @description
#' Creates an object of class \code{mig_parameter}. Minimal
#' checks are done; for interactive use see
#' \code{\link{mig_parameter}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{mig_parameter}}.
#'
#' @seealso mig_parameter
#'
#' @inheritParams new_demog_change_component_df
#' @return An object of class \code{mig_parameter}.
#' @author Mark Wheldon
new_mig_parameter <-
    function(x,
             age_span = double(),
             time_span = double(),
             dimensions = get_req_dimensions_for_ccmpp_input_classes("mig_parameter"),
             value_type = get_value_types_for_classes("mig_parameter"),
             value_scale = NA,
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions = dimensions,
                           value_type = value_type,
                           value_scale = value_scale,
                           ...,
                           class = c(class, "mig_parameter"))
    }


#' Constructor for class \code{mig_parameter}
#'
#' \code{mig_parameter} is a subclass of
#' \code{\link{ccmpp_input_df}}. It imposes two additional conditions:
#' \enumerate{
#'   \item{\code{Value_type} attribute equals \dQuote{rate}.}
#'   \item{Within year and sex, age must start at 0.}}
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{validate_ccmpp_object}} for object validation,
#'     \code{\link{ccmpp_input_df}} for the class from which this one
#'     inherits.
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{mig_parameter}.
#' @author Mark Wheldon
#' @export
mig_parameter <-
    function(x,
             time_span = attr(x, "time_span")) {

        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_ccmpp_input_classes("mig_parameter"),
                            value_type = get_value_types_for_classes("mig_parameter"),
                            value_scale = NA)
        ## Create/Validate
        validate_ccmpp_object(
            new_mig_parameter(li$df,
                              time_span = li$time_span,
                              value_scale = NA)
        )
    }


#' Coerce to a \code{mig_parameter}
#'
#' These functions coerce an object to a
#' \code{mig_parameter} if possible, or check if it is
#' one.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{coerce_demog_change_component_df}}
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_mig_parameter
#' @export
as_mig_parameter <- function(x, ...) {
    UseMethod("as_mig_parameter")
}

#' @rdname coerce_mig_parameter
#' @export
as_mig_parameter.default <- function(x, ...) {
    if (is_mig_parameter(x)) return(x)
    stop("Cannot coerce 'x' to 'mig_parameter'.")
}

#' @rdname coerce_mig_parameter
#' @export
as_mig_parameter.data.frame <- function(x, ...) {
    mig_parameter(as.data.frame(x))
}

#' @rdname coerce_mig_parameter
#' @export
as_mig_parameter.matrix <- function(x, ...) {
    as_mig_parameter(as.data.frame(NextMethod()))
}

#' @rdname coerce_mig_parameter
#' @export
as_mig_parameter.mig_parameter <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("mig_parameter", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmpp_object(x))
}

#' @rdname coerce_mig_parameter
#' @export
is_mig_parameter <- function(x) {
    inherits(x, "mig_parameter")
}




#' @rdname subset_demog_change_component_df
#' @export
subset_time.mig_parameter <- function(x, times, drop = FALSE) {

    x <- NextMethod()
    return(mig_parameter(x))
}
