
#' Low-level constructor for class \code{mig_net_count_tot_input_df}.
#'
#' @description
#' Creates an object of class \code{mig_net_count_tot_input_df}. Minimal
#' checks are done; for interactive use see
#' \code{\link{mig_net_count_tot_input_df}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{mig_net_count_tot_input_df}}.
#'
#' @seealso mig_net_count_tot_input_df
#'
#' @inheritParams new_demog_change_component_df
#' @return An object of class \code{mig_net_count_tot_input_df}.
#' @author Mark Wheldon
new_mig_net_count_tot_input_df <-
    function(x,
             time_span = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = double(),
                           time_span = time_span,
                           dimensions = get_req_dimensions_for_ccmpp_input_classes("mig_net_count_tot_input_df"),
                           value_type = get_value_types_for_classes("mig_net_count_tot_input_df"),
                           ...,
                           class = c(class, "mig_net_count_tot_input_df"))
    }


#' Constructor for class \code{mig_net_count_tot_input_df}
#'
#' \code{mig_net_count_tot_input_df} is a subclass of
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
#' @return An object of class \code{mig_net_count_tot_input_df}.
#' @author Mark Wheldon
#' @export
mig_net_count_tot_input_df <-
    function(x,
             time_span = attr(x, "time_span")) {

        x <- ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_ccmpp_input_classes("mig_net_count_tot_input_df"),
                            value_type = get_value_types_for_classes("mig_net_count_tot_input_df"))
        ## Create/Validate
        validate_ccmpp_object(
            new_mig_net_count_tot_input_df(x,
                               time_span = attr(x, "time_span"))
        )
    }


#' Coerce to a \code{mig_net_count_tot_input_df}
#'
#' These functions coerce an object to a
#' \code{mig_net_count_tot_input_df} if possible, or check if it is
#' one.
#'
#' @seealso \code{\link{coerce_demog_change_component_df}} for an important note on validation.
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_mig_net_count_tot_input_df
#' @export
as_mig_net_count_tot_input_df <- function(x, ...) {
    UseMethod("as_mig_net_count_tot_input_df")
}

#' @rdname coerce_mig_net_count_tot_input_df
#' @export
as_mig_net_count_tot_input_df.default <- function(x, ...) {
    if (is_mig_net_count_tot_input_df(x)) return(x)
    stop("Cannot coerce 'x' to 'mig_net_count_tot_input_df'.")
}

#' @rdname coerce_mig_net_count_tot_input_df
#' @export
as_mig_net_count_tot_input_df.data.frame <- function(x, ...) {
    mig_net_count_tot_input_df(as.data.frame(x))
}

#' @rdname coerce_mig_net_count_tot_input_df
#' @export
as_mig_net_count_tot_input_df.matrix <- function(x, ...) {
    as_mig_net_count_tot_input_df(as.data.frame(NextMethod()))
}

#' @rdname coerce_mig_net_count_tot_input_df
#' @export
as_mig_net_count_tot_input_df.mig_net_count_tot_input_df <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("mig_net_count_tot_input_df", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(x)
}

#' @rdname coerce_mig_net_count_tot_input_df
#' @export
is_mig_net_count_tot_input_df <- function(x) {
    inherits(x, "mig_net_count_tot_input_df")
}
