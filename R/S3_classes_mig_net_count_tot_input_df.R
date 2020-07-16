
#' Low-level constructor for class \code{mig_net_count_tot_b}.
#'
#' @description
#' Creates an object of class \code{mig_net_count_tot_b}. Minimal
#' checks are done; for interactive use see
#' \code{\link{mig_net_count_tot_b}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{mig_net_count_tot_b}}.
#'
#' @seealso mig_net_count_tot_b
#'
#' @inheritParams new_demog_change_component_df
#' @return An object of class \code{mig_net_count_tot_b}.
#' @author Mark Wheldon
new_mig_net_count_tot_b <-
    function(x,
             time_span = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = double(),
                           time_span = time_span,
                           dimensions = get_req_dimensions_for_ccmpp_input_classes("mig_net_count_tot_b"),
                           value_type = get_value_types_for_classes("mig_net_count_tot_b"),
                           ...,
                           class = c(class, "mig_net_count_tot_b"))
    }


#' Constructor for class \code{mig_net_count_tot_b}
#'
#' \code{mig_net_count_tot_b} is a subclass of
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
#' @return An object of class \code{mig_net_count_tot_b}.
#' @author Mark Wheldon
#' @export
mig_net_count_tot_b <-
    function(x,
             time_span = attr(x, "time_span")) {

        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_ccmpp_input_classes("mig_net_count_tot_b"),
                            value_type = get_value_types_for_classes("mig_net_count_tot_b"))
        ## Create/Validate
        validate_ccmpp_object(
            new_mig_net_count_tot_b(li$df,
                               time_span = li$time_span)
        )
    }


#' Coerce to a \code{mig_net_count_tot_b}
#'
#' These functions coerce an object to a
#' \code{mig_net_count_tot_b} if possible, or check if it is
#' one.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{coerce_demog_change_component_df}} for an important note on validation.
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_mig_net_count_tot_b
#' @export
as_mig_net_count_tot_b <- function(x, ...) {
    UseMethod("as_mig_net_count_tot_b")
}

#' @rdname coerce_mig_net_count_tot_b
#' @export
as_mig_net_count_tot_b.default <- function(x, ...) {
    if (is_mig_net_count_tot_b(x)) return(x)
    stop("Cannot coerce 'x' to 'mig_net_count_tot_b'.")
}

#' @rdname coerce_mig_net_count_tot_b
#' @export
as_mig_net_count_tot_b.data.frame <- function(x, ...) {
    mig_net_count_tot_b(as.data.frame(x))
}

#' @rdname coerce_mig_net_count_tot_b
#' @export
as_mig_net_count_tot_b.matrix <- function(x, ...) {
    as_mig_net_count_tot_b(as.data.frame(NextMethod()))
}

#' @rdname coerce_mig_net_count_tot_b
#' @export
as_mig_net_count_tot_b.mig_net_count_tot_b <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("mig_net_count_tot_b", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(x)
}

#' @rdname coerce_mig_net_count_tot_b
#' @export
is_mig_net_count_tot_b <- function(x) {
    inherits(x, "mig_net_count_tot_b")
}




#' @rdname subset_demog_change_component_df
#' @export
subset_time.mig_net_count_tot_b <- function(x, times, drop = FALSE) {

    x <- NextMethod()
    return(mig_net_count_tot_b(x))
}
