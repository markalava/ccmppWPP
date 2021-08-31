###-----------------------------------------------------------------------------
### * Helpers

get_allowed_mig_assumptions_mig_parameter <- function() {
    c("end", "even")
}

get_allowed_mig_types_mig_parameter <- function() {
    c("counts", "rates")
}

get_allowed_value_categories_mig_parameter <- function() {
    c(get_allowed_mig_types_mig_parameter(),
      get_allowed_mig_assumptions_mig_parameter())
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
             time_span = double(),
             dimensions = get_req_dimensions_for_subclass_classes("mig_parameter"),
             value_type = get_value_types_for_subclass_classes("mig_parameter"),
             value_scale = NA,
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
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
#' \code{\link{ccmpp_input_df}}. It contains
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
#' @return An object of class \code{mig_parameter}.
#' @author Mark Wheldon
#' @export
mig_parameter <- function(x, ...) {
    UseMethod("mig_parameter")
}

#' @rdname mig_parameter
#' @export
mig_parameter.data.frame <- function(x) {
        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_subclass_classes("mig_parameter"),
                            value_type = get_value_types_for_subclass_classes("mig_parameter"),
                            value_scale = NA)
        ## Create/Validate
        validate_ccmppWPP_object(
            new_mig_parameter(li$df,
                              time_span = li$time_span,
                              value_scale = NA)
        )
    }

#' @rdname mig_parameter
#' @export
mig_parameter.ccmpp_input_list <- function(x) {
    mig_parameter_component(x)
}

#' @rdname mig_parameter
#' @export
`mig_parameter<-` <- function(x, value) {
    `mig_parameter_component<-`(x, value)
}


###-----------------------------------------------------------------------------
### * Coercion

#' Coerce to a \code{mig_parameter}
#'
#' These functions coerce an object to a
#' \code{mig_parameter} if possible, or check if it is
#' one.
#'
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
    return(validate_ccmppWPP_object(x))
}

#' @rdname coerce_mig_parameter
#' @export
is_mig_parameter <- function(x) {
    inherits(x, "mig_parameter")
}


###-----------------------------------------------------------------------------
### * Subset

#' @rdname subset_demog_change_component_df
#' @export
subset_time.mig_parameter <- function(x, times, include = TRUE) {
    x <- NextMethod()
    return(mig_parameter(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_indicator.mig_parameter <- function(x, times, include = TRUE) {
    x <- NextMethod()
    return(mig_parameter(x))
}


###-----------------------------------------------------------------------------
### * Extraction

#' Extract or set the migration assumption or migration type
#'
#' Returns or sets the migration assumption for a
#' \code{\link{mig_parameter}} object. Migration assumption is stored
#' in the \value{column} in rows where \code{indicator} =
#' \dQuote{mig_assumption}. Migration type is stored
#' in the \value{column} in rows where \code{indicator} =
#' \dQuote{mig_type}.
#'
#' @param x An object inheriting from \code{\link{mig_parameter}}.
#' @return For the extraction function, a character string with the
#'     assumption or type; for the replacement function, \code{x}
#'     (invisibly) with the assumption or type to set to \code{value}.
#' @author Mark Wheldon
#' @name mig_assumption_extract_and_set
#' @export
mig_assumption <- function(x) {
    UseMethod("mig_assumption")
}

#' @rdname mig_assumption_extract_and_set
#' @export
mig_assumption.mig_parameter <- function(x) {
    unique(x[x$indicator == "mig_assumption", "value"])
}

#' @rdname mig_assumption_extract_and_set
#' @export
`mig_assumption<-` <- function(x, value) {
    UseMethod("mig_assumption<-")
}

#' @rdname mig_assumption_extract_and_set
#' @export
`mig_assumption<-.mig_parameter` <- function(x, value) {
    stopifnot(value %in% get_allowed_mig_assumptions_mig_parameter())
    x[x$indicator == "mig_assumption", "value"] <- value
    as_mig_parameter(x)
}

#' @rdname mig_assumption_extract_and_set
#' @export
mig_type <- function(x) {
    UseMethod("mig_type")
}

#' @rdname mig_assumption_extract_and_set
#' @export
mig_type.mig_parameter <- function(x) {
    unique(x[x$indicator == "mig_type", "value"])
}

#' @rdname mig_assumption_extract_and_set
#' @export
`mig_type<-` <- function(x, value) {
    UseMethod("mig_type<-")
}

#' @rdname mig_assumption_extract_and_set
#' @export
`mig_type<-.mig_parameter` <- function(x, value) {
    stopifnot(value %in% get_allowed_mig_types_mig_parameter())
    x[x$indicator == "mig_type", "value"] <- value
    as_mig_parameter(x)
}
