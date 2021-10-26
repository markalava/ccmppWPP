
#' Low-level constructor for class \code{mig_net_count_age_sex}.
#'
#' @description
#' Creates an object of class \code{mig_net_count_age_sex}. Minimal
#' checks are done; for interactive use see
#' \code{\link{mig_net_count_age_sex}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{mig_net_count_age_sex}}.
#'
#' @seealso mig_net_count_age_sex
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{mig_net_count_age_sex}.
#' @author Mark Wheldon
new_mig_net_count_age_sex <-
    function(x,
             age_span = double(),
             time_span = double(),
             dimensions = get_req_dimensions_for_subclass_classes("mig_net_count_age_sex"),
             value_type = get_value_types_for_subclass_classes("mig_net_count_age_sex"),
             value_scale = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions = dimensions,
                           value_type = value_type,
                           value_scale = value_scale,
                           ...,
                           class = c(class, "mig_net_count_age_sex"))
    }


#' Constructor for class \code{mig_net_count_age_sex}
#'
#' \code{mig_net_count_age_sex} is a subclass of
#' \code{\link{ccmpp_input_df}}. It imposes two additional conditions:
#' \enumerate{
#'   \item{\code{Value_type} attribute equals \dQuote{rate}.}
#'   \item{Within year and sex, age must start at 0.}}
#'
#' Methods are defined for \code{\link{data.frame}}s and
#' \code{\link{ccmpp_input_list}}s, and possibly other objects as
#' well. The \code{data.frame} method \dQuote{constructs} an object
#' from \code{x}. The \code{ccmpp_input_list} method \dQuote{extracts}
#' an object from \code{x}. There is also a replacement function which
#' complements the extraction methods.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{validate_ccmppWPP_object}} for object
#'     validation, \code{\link{ccmpp_input_df}} for the class from
#'     which this one inherits.
#'
#' @param x An object for which a method is defined (see \dQuote{Details}).
#' @param value_scale_pop_count The scale of the \code{value} column
#'     in \code{pop_count_age_sex}. If unspecified, defaults to the
#'     \dQuote{value_scale} attribute of \code{pop_count_age_sex} if
#'     non-\code{NULL}, otherwise \code{1}.
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{mig_net_count_age_sex}.
#' @author Mark Wheldon
#' @name mig_net_count_age_sex
#' @export
mig_net_count_age_sex <- function(x, ...) {
    UseMethod("mig_net_count_age_sex")
}

#' @rdname mig_net_count_age_sex
#' @export
mig_net_count_age_sex.data.frame <-
    function(x,
             value_scale = attr(x, "value_scale"), ...) {

        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_subclass_classes("mig_net_count_age_sex"),
                            value_type = get_value_types_for_subclass_classes("mig_net_count_age_sex"),
                            value_scale = value_scale)

        ## Create/Validate
        validate_ccmppWPP_object(
            new_mig_net_count_age_sex(li$df,
                               age_span = li$age_span,
                               time_span = li$time_span,
                               value_scale = li$value_scale)
        )
    }

#' @rdname mig_net_count_age_sex
#' @export
mig_net_count_age_sex.ccmpp_input_list <- function(x) {
    mig_net_count_component(x)
}

#' @rdname mig_net_count_age_sex
#' @export
`mig_net_count_age_sex<-`  <- function(x, value) {
    `mig_net_count_component<-`(x, value)
}


#' Coerce to a \code{mig_net_count_age_sex}
#'
#' These functions coerce an object to a
#' \code{mig_net_count_age_sex} if possible, or check if it is
#' one.
#'
#' @seealso \code{\link{coerce_demog_change_component_df}}
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_mig_net_count_age_sex
#' @export
as_mig_net_count_age_sex <- function(x, ...) {
    UseMethod("as_mig_net_count_age_sex")
}

#' @rdname coerce_mig_net_count_age_sex
#' @export
as_mig_net_count_age_sex.default <- function(x, ...) {
    if (is_mig_net_count_age_sex(x)) return(x)
    stop("Cannot coerce 'x' to 'mig_net_count_age_sex'.")
}

#' @rdname coerce_mig_net_count_age_sex
#' @export
as_mig_net_count_age_sex.data.frame <- function(x, ...) {
    mig_net_count_age_sex(as.data.frame(x))
}

#' @rdname coerce_mig_net_count_age_sex
#' @export
as_mig_net_count_age_sex.matrix <- function(x, ...) {
    as_mig_net_count_age_sex(as.data.frame(NextMethod()))
}

#' @rdname coerce_mig_net_count_age_sex
#' @export
as_mig_net_count_age_sex.mig_net_count_age_sex <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("mig_net_count_age_sex", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmppWPP_object(x))
}

#' @rdname coerce_mig_net_count_age_sex
#' @export
is_mig_net_count_age_sex <- function(x) {
    inherits(x, "mig_net_count_age_sex")
}




#' @rdname subset_demog_change_component_df
#' @export
subset_time.mig_net_count_age_sex <- function(x, times, include = TRUE) {

    x <- NextMethod()
    return(mig_net_count_age_sex(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.mig_net_count_age_sex <- function(x, ages, include = TRUE) {

    x <- NextMethod()
    return(mig_net_count_age_sex(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.mig_net_count_age_sex <- function(x, sexes, include = TRUE) {

    x <- NextMethod()
    return(mig_net_count_age_sex(x))
}
