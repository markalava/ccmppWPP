#' Low-level constructor for class \code{srb}.
#'
#' @description
#' Creates an object of class \code{srb}. Minimal
#' checks are done; for interactive use see
#' \code{\link{srb}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{srb}}.
#'
#' @seealso srb
#'
#' @inheritParams new_demog_change_component_df
#' @return An object of class \code{srb}.
#' @author Mark Wheldon
new_srb <-
    function(x,
             age_span = double(),
             time_span = double(),
             dimensions =  get_req_dimensions_for_ccmpp_in_out_classes("srb"),
             value_type = get_value_types_for_ccmpp_in_out_classes("srb"),
             value_scale = NA,
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions = dimensions,
                           value_type = value_type,
                           value_scale = value_scale,
                           ...,
                           class = c(class, "srb"))
    }


#' Constructor for class \code{srb}
#'
#' \code{srb} is a subclass of
#' \code{\link{ccmpp_input_df}}. It imposes three additional conditions:
#' \enumerate{
#'   \item{\code{Value_type} attribute equals \dQuote{ratio}.}
#'   \item{There can be no 'age' dimension.}
#'   \item{There can be no 'sex' dimension.}}
#'
#' Methods are defined for \code{\link{data.frame}}s and
#' \code{\link{ccmpp_input_list}}s, and possibly other objects as
#' well. The \code{data.frame} method \dQuote{constructs} an object
#' from \code{x}. The \code{ccmpp_input_list} method \dQuote{extracts}
#' an object from \code{x}. There is also a replacement function which
#' complements the extraction methods.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{validate_ccmppWPP_object}} for object validation,
#'     \code{\link{ccmpp_input_df}} for the class from which this one
#'     inherits.
#'
#' @param x An object for which a method is defined (see \dQuote{Details}).
#' @inheritParams demog_change_component_df
#' @return An object of class \code{srb}.
#' @author Mark Wheldon
#' @export
srb <- function(x, ...) {
    UseMethod("srb")
}

#' @rdname srb
#' @export
srb.data.frame <- function(x,
             time_span = attr(x, "time_span")) {

        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_ccmpp_in_out_classes("srb"),
                            value_type = get_value_types_for_ccmpp_in_out_classes("srb"),
                            value_scale = NA)

        ## Create/Validate
        validate_ccmppWPP_object(
            new_srb(li$df,
                    time_span = li$time_span,
                    value_scale = NA)
        )
    }

#' @rdname srb
#' @export
srb.ccmpp_input_list <- function(x) {
    srb_component(x)
}

#' @rdname srb
#' @export
`srb<-` <- function(x, value) {
    `srb_component<-`(x, value)
}


#' Coerce to a \code{srb}
#'
#' These functions coerce an object to a
#' \code{srb} if possible, or check if it is
#' one.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{coerce_demog_change_component_df}}
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_srb
#' @export
as_srb <- function(x, ...) {
    UseMethod("as_srb")
}

#' @rdname coerce_srb
#' @export
as_srb.default <- function(x, ...) {
    if (is_srb(x)) return(x)
    stop("Cannot coerce 'x' to 'srb'.")
}

#' @rdname coerce_srb
#' @export
as_srb.data.frame <- function(x, ...) {
    srb(as.data.frame(x))
}

#' @rdname coerce_srb
#' @export
as_srb.matrix <- function(x, ...) {
    as_srb(as.data.frame(NextMethod()))
}

#' @rdname coerce_srb
#' @export
as_srb.srb <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("srb", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmppWPP_object(x))
}

#' @rdname coerce_srb
#' @export
is_srb <- function(x) {
    inherits(x, "srb")
}



#' @rdname subset_demog_change_component_df
#' @export
subset_time.srb <- function(x, times, include = TRUE) {

    x <- NextMethod()
    return(srb(x))
}
