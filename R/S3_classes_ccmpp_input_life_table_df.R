###-----------------------------------------------------------------------------
### * Helpers

get_required_indicator_categories_life_table <- function() {
    c("lt_nMx", "lt_nAx", "lt_nqx", "lt_lx", "lt_ndx", "lt_nLx",
      "lt_Sx", "lt_Tx", "lt_ex")
}

get_life_table_radix_from_li <- function(li) {
        lx_age_0 <- li$df[li$df$indicator == "lt_lx" &
                          li$df$age_start == 0, ][1, "value"]
        if (length(lx_age_0) && is.finite(lx_age_0))
            return(lx_age_0)
        else return(NA)
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
             dimensions = get_req_dimensions_for_ccmpp_in_out_classes("life_table_age_sex"),
             value_type = get_value_types_for_ccmpp_in_out_classes("life_table_age_sex"),
             value_scale = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions = dimensions,
                           value_type = value_type,
                           value_scale = value_scale,
                           ...,
                           class = c(class, "life_table_age_sex"))
    }


#' Constructor for class \code{life_table_age_sex}
#'
#' \code{life_table_age_sex} is a subclass of
#' \code{\link{ccmpp_input_df}}. It has an indicator column that **TO BE COMPLETED**
#'
#' @section Note:
#' The \dQuote{value_scale} attribute for objects of class
#' \code{life_table_age_sex} is the \emph{radix} of the life table.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{validate_ccmppWPP_object}} for object validation,
#'     \code{\link{ccmpp_input_df}} for the class from which this one
#'     inherits.
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{life_table_age_sex}.
#' @author Mark Wheldon
#' @export
life_table_age_sex <-
    function(x,
             value_scale = attr(x, "value_scale")) {

        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_ccmpp_in_out_classes("life_table_age_sex"),
                            value_type = get_value_types_for_ccmpp_in_out_classes("life_table_age_sex"),
                            value_scale = value_scale)

        ## Set 'value_scale' to the radix
        if (is.null(attr(x, "value_scale"))) li$value_scale <- get_life_table_radix_from_li(li)

        ## Create/Validate
        validate_ccmppWPP_object(
            new_life_table_age_sex(li$df,
                               age_span = li$age_span,
                               time_span = li$time_span,
                               value_scale = li$value_scale)
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
    return(validate_ccmppWPP_object(x))
}

#' @rdname coerce_life_table_age_sex
#' @export
is_life_table_age_sex <- function(x) {
    inherits(x, "life_table_age_sex")
}


###-----------------------------------------------------------------------------
### * Subset

#' @rdname subset_demog_change_component_df
#' @export
subset_indicator.life_table_age_sex <- function(x, indicators, include = TRUE) {
    vsx <- attr(x, "value_scale")
    x <- NextMethod()
    return(life_table_age_sex(x, value_scale = vsx))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_time.life_table_age_sex <- function(x, times, include = TRUE) {
    vsx <- attr(x, "value_scale")
    x <- NextMethod()
    return(life_table_age_sex(x, value_scale = vsx))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.life_table_age_sex <- function(x, ages, include = TRUE) {
    vsx <- attr(x, "value_scale")
    x <- NextMethod()
    return(life_table_age_sex(x, value_scale = vsx))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.life_table_age_sex <- function(x, sexes, include = TRUE) {
    vsx <- attr(x, "value_scale")
    x <- NextMethod()
    return(life_table_age_sex(x, value_scale = vsx))
}


## ###-----------------------------------------------------------------------------
## ### * Abridge

## #' @rdname abridge
## #' @export
## abridge.life_table_age_sex <- function(x, span_abridged = NULL, time_span_abridged = NULL,
##                                    age_start_abridged = NULL, time_start_abridged = NULL,
##                                    out_class = class(x)[1], ...) {

##     ## Abridge lt, ndx, nLx

##     abr_1 <- subset_indicator(as_demog_change_component_df(x),
##                               c("lt_ndx" ,"lt_lx", "lt_nLx"))
##     abr_1 <- abridge(abr_1, age_span_abridged = age_span_abridged, time_span_abridged = time_span_abridged,
##                     age_start_abridged = age_start_abridged, time_start_abridged = time_start_abridged,
##                     out_class = out_class, ...)



##     abr_1 <- demog_change_component_df(x[x$indicator %in% c("lt_ndx" ,"lt_lx", "lt_nLx"), ])
##     abr_1 <- abridgetapply(abr_1, list(indicator = abr_1$indicator), FUN = "abridge",
##                     age_span_abridged = age_span_abridged, time_span_abridged = time_span_abridged,
##                     age_start_abridged = age_start_abridged, time_start_abridged = time_start_abridged,
##                     out_class = out_class, ...)



##     death_count_abridged <-
##         abridge(subset_indicator(as_ccmpp_input_df(x), "lt_ndx"),
##                 age_span_abridged = age_span_abridged, time_span_abridged = time_span_abridged,
##                 age_start_abridged = age_start_abridged, time_start_abridged = time_start_abridged,
##                 out_class = out_class, ...)
##     lx_count_abridged <-
##         abridge(subset_indicator(x, "lt_lx"),
##                 age_span_abridged = age_span_abridged, time_span_abridged = time_span_abridged,
##                 age_start_abridged = age_start_abridged, time_start_abridged = time_start_abridged,
##                 out_class = out_class, ...)

##     ## Person-years lived
##     nLx_abridged <-
##         abridge(subset_indicator(x, "lt_nLx"),
##                 age_span_abridged = age_span_abridged, time_span_abridged = time_span_abridged,
##                 age_start_abridged = age_start_abridged, time_start_abridged = time_start_abridged,
##                 out_class = out_class, ...)



