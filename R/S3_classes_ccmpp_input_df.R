
###-----------------------------------------------------------------------------
### * Class constructors

#' Low-level constructor for class \code{ccmpp_input_df}.
#'
#' @description
#' Creates an object of class \code{ccmpp_input_df}. Minimal
#' checks are done; for interactive use see
#' \code{\link{ccmpp_input_df}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{ccmpp_input_df}}.
#'
#' @seealso ccmpp_input_df
#'
#' @inheritParams new_demog_change_component_df
#' @return An object of class \code{ccmpp_input_df}.
#' @author Mark Wheldon
new_ccmpp_input_df <-
    function(x, dimensions = character(),
             age_span = double(),
             time_span = double(),
             value_type = character(),
             value_scale = numeric(),
             ..., class = character()) {
        new_demog_change_component_df(x = x,
                                      age_span = age_span,
                                      time_span = time_span,
                                      dimensions = dimensions,
                                      value_type = value_type,
                                      value_scale = value_scale,
                                      ...,
                                      class = c(class, "ccmpp_input_df"))
    }


prepare_df_for_ccmpp_input_df <- function(x,
             dimensions = attr(x, "dimensions"),
             value_type = attr(x, "value_type"),
             value_scale = attr(x, "value_scale")) {

    li <- prepare_df_for_demog_change_component_df(
                x,
                dimensions = dimensions,
        value_type = value_type,
        value_scale = value_scale)

    age_span <- unique(li$df$age_span)
    time_span <- unique(li$df$time_span)

    li$age_span <- (if (is.null(age_span)) double() else age_span)
    li$time_span <- (if (is.null(time_span)) double() else time_span)

    return(li)
    }


#' Constructor for class \code{ccmpp_input_df}
#'
#' **TBC** More strict version of \code{\link{demog_change_component_df}}.
#'
#' @family ccmpp_input_objects
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{ccmpp_input_df}.
#' @author Mark Wheldon
#' @export
ccmpp_input_df <-
    function(x,
             dimensions = attr(x, "dimensions"),
             value_type = attr(x, "value_type"),
             value_scale = attr(x, "value_scale"),
             ...) {

        li <-
            prepare_df_for_ccmpp_input_df(
                x,
                dimensions = dimensions,
                value_type = value_type,
                value_scale = value_scale)

        ## Create/Validate
        validate_ccmppWPP_object(
            new_ccmpp_input_df(li$df,
                               dimensions = li$dimensions,
                               age_span = li$age_span,
                               time_span = li$time_span,
                               value_type = li$value_type,
                               value_scale = li$value_scale,
                               ...)
        )
    }

###-----------------------------------------------------------------------------
### * Coercion

#' Coerce to a \code{ccmpp_input_df}
#'
#' These functions coerce an object to a
#' \code{ccmpp_input_df} if possible, or check if it is
#' one.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{coerce_demog_change_component_df}}
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_ccmpp_input_df
#' @export
as_ccmpp_input_df <- function(x, ...) {
    UseMethod("as_ccmpp_input_df")
}

#' @rdname coerce_ccmpp_input_df
#' @export
as_ccmpp_input_df.default <- function(x, ...) {
    if (is_ccmpp_input_df(x)) return(x)
    stop("Cannot coerce 'x' to 'ccmpp_input_df'.")
}

#' @rdname coerce_ccmpp_input_df
#' @export
as_ccmpp_input_df.data.frame <- function(x, value_type = attr(x, "value_type"), value_scale = attr(x, "value_scale"), ...) {
    ccmpp_input_df(as.data.frame(x), value_type = value_type, value_scale = value_scale)
}

#' @rdname coerce_ccmpp_input_df
#' @export
as_ccmpp_input_df.matrix <- function(x, value_type = attr(x, "value_type"), value_scale = attr(x, "value_scale"), ...) {
    as_ccmpp_input_df(as_demog_change_component_df(x, value_type = value_type, value_scale = value_scale),
                      value_type = value_type, value_scale = value_scale)
}

#' @rdname coerce_ccmpp_input_df
#' @export
as_ccmpp_input_df.ccmpp_input_df <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("ccmpp_input_df", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmppWPP_object(x))
}

#' @rdname coerce_ccmpp_input_df
#' @export
is_ccmpp_input_df <- function(x) {
    inherits(x, "ccmpp_input_df")
}
