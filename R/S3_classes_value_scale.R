###-----------------------------------------------------------------------------
### demog_change_component_df

### Creator

new_demog_change_component_df_value_scale <-
    function(x, class_of_df = character(),
             value_type = character(),
             ...,
             class = character()) {
        stopifnot(is.na(x) || is.numeric(x))
        if (is.na(x)) x <- as.numeric(NA)
        structure(x, class_of_df = class_of_df,
                  value_type = value_type,
                  ...,
                  class = c(class, "demog_change_component_df_value_scale"))
    }


### Extraction and display

#' Create, extract and set a \dQuote{value_scale}
#'
#' These functions extract and set the \dQuote{value_scale} attributes
#' of objects inheriting from
#' \code{\link{demog_change_component_df}}. This attribute is
#' displayed differently depending on the class and other attributes
#' of its parent object. If you are comparing \dQuote{value_scales}
#' with, e.g., \code{\link{identical}} you should use
#' \code{value_scale_numeric} (see \dQuote{Details}).
#'
#' The object returned by \code{value_scale} is given class
#' \code{\var{[prefix]}_value_scale}. \var{[prefix]} can currently be
#' either \code{demog_change_component_df} or \code{ccmpp_input_df}
#' depending on the class of the input \code{x}. This is currently the
#' only (exported) way of creating objects of these types; the class
#' is used only to control the \code{\print} method. As a consequence,
#' for example, \code{attr(x, "value_scale")} will return the
#' unclassed, numeric, value, not an object of class
#' \code{\var{[prefix]}_value_scale} (but see function
#' \code{value_scale_numeric} below).
#'
#' Function \code{value_scale_numeric} returns a numeric (double) scalar (via
#' \code{\link{as.double}}) with no class attribute. It is suitable
#' for use with \code{\link{identical}} when comparing the
#' \dQuote{value_scale}s of different objects.
#'
#' @param x An object inheriting from
#'     \code{demog_change_component_df}.
#' @return For \code{value_scale}, an object of class
#'     \code{\var{[prefix]}_value_scale}. For
#'     \code{value_scale_numeric} a numeric scalar with no class. See
#'     \dQuote{Details}.
#' @author Mark Wheldon
#' @family extract_attributes
#' @name value_scale
#' @export
value_scale <- function(x) {
    UseMethod("value_scale")
}

#' @rdname value_scale
#' @export
value_scale.demog_change_component_df <- function(x) {
    new_demog_change_component_df_value_scale(x = attr(x, "value_scale"),
                                        class_of_df = oldClass(x)[1],
                                     value_type = value_type(x))
}

#' @rdname value_scale
#' @export
value_scale_numeric <- function(x) {
    UseMethod("value_scale_numeric")
}

#' @rdname value_scale
#' @export
value_scale_numeric.demog_change_component_df <- function(x) {
    as.double(attr(x, "value_scale"))
}

#' @rdname value_scale
#' @export
print.demog_change_component_df_value_scale <- function(x, ...) {
    msg <- c("value_scale: ")
    if (!is.na(x)) {
        pref <- get_value_scale_prefixes_for_value_types(attr(x, "value_type"))
        msg <- c("value_scale: ")
        if (!is.na(pref)) msg <- paste0(msg, pref, " ")
        msg <- paste0(msg, as.character(x))
    } else {
        msg <- paste0(msg, as.character(x))
    }
    cat(msg, "\n")
    return(invisible(x))
}


### Replacement

#' @rdname value_scale
#' @export
`value_scale<-` <- function(x, value, ...) {
    UseMethod("value_scale<-")
}

#' @rdname value_scale
#' @export
`value_scale<-.demog_change_component_df` <- function(x, value, ...) {
    vtx <- value_type(x)
    if (length(vtx) > 1) {
        stop("'x' has value_type '",
                vtx,
                "' which has length > 1. Cannot change the value_scale.")
    }
    if (!vtx %in% get_value_types_w_non_NA_value_scale()) {
        stop("Cannot change the value_scale of an object with value_type '",
                vtx,
                "'.")
    }
    attr(x, "value_scale") <- value
    S3_class_warning("Changing the 'value_scale' attribute does not automatically re-scale the 'value' column in the data; you must do that yourself to ensure consistency, or see the 'rescale_value' function for an alternative approach (NOT YET IMPLEMENTED).")
    validate_ccmpp_object(x)
    }


###-----------------------------------------------------------------------------
### ccmpp_input_df

#' @rdname value_scale
#' @export
value_scale.ccmpp_input_df <- function(x) {
    new_demog_change_component_df_value_scale(x = attr(x, "value_scale"),
                                        class_of_df = oldClass(x)[1],
                                        value_type = value_type(x),
                                        class = "ccmpp_input_df_value_scale")
}
