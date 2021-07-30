
#' Low-level constructor for class \code{pop_count_age_sex_reference}.
#'
#' @description
#' Creates an object of class \code{pop_count_age_sex_reference}. Minimal
#' checks are done; for interactive use see
#' \code{\link{pop_count_age_sex_reference}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{pop_count_age_sex_reference}}.
#'
#' @seealso pop_count_age_sex_reference
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{pop_count_age_sex_reference}.
#' @author Mark Wheldon
new_pop_count_age_sex_reference <-
    function(x,
             dimensions = character(),
             value_type = get_value_types_for_subclass_classes("pop_count_age_sex_reference"),
             value_scale = double(),
             ..., class = character()) {
        new_demog_change_component_df(x = x,
                           dimensions = dimensions,
                           value_type = value_type,
                           value_scale = value_scale,
                           ...,
                           class = c(class, "pop_count_age_sex_reference"))
    }


#' Constructor for class \code{pop_count_age_sex_reference}
#'
#' \code{pop_count_age_sex_reference} is a subclass of
#' \code{\link{demog_change_component_df}}. It imposes two additional conditions:
#' \enumerate{
#'   \item{\code{time_span} attribute equals 0.}
#'   \item{\code{value_type} attribute equals \dQuote{count}.}}
#'
#' Methods are defined for \code{\link{data.frame}}s and possibly
#' other objects as well. The \code{data.frame} method
#' \dQuote{constructs} an object from \code{x}.
#'
#' @seealso \code{\link{validate_ccmppWPP_object}} for object validation,
#'     \code{\link{demog_change_component_df}} for the class from which this one
#'     inherits.
#'
#' @param x An object for which a method is defined (see \dQuote{Details}).
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{pop_count_age_sex_reference}.
#' @author Mark Wheldon
#' @name pop_count_age_sex_reference
#' @export
pop_count_age_sex_reference <- function(x,
             value_scale = attr(x, "value_scale")) {

        if (is.null(value_scale)) value_scale <- 1
        else if (!is.numeric(value_scale)) stop("'value_scale' must be numeric.")

        li <-
            prepare_df_for_demog_change_component_df(
                x,
                dimensions = attr(x, "dimensions"),
                value_type = get_value_types_for_subclass_classes("pop_count_age_sex_reference"),
                value_scale = value_scale)

        ## Create/Validate
        validate_ccmppWPP_object(
            new_pop_count_age_sex_reference(li$df,
                                            dimensions = li$dimensions,
                                          value_type = li$value_type,
                                            value_scale = li$value_scale)
        )
    }


#' Coerce to a \code{pop_count_age_sex_reference}
#'
#' These functions coerce an object to a
#' \code{pop_count_age_sex_reference} if possible, or check if it is
#' one.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{coerce_demog_change_component_df}}
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_pop_count_age_sex_reference
#' @export
as_pop_count_age_sex_reference <- function(x, ...) {
    UseMethod("as_pop_count_age_sex_reference")
}

#' @rdname coerce_pop_count_age_sex_reference
#' @export
as_pop_count_age_sex_reference.default <- function(x, ...) {
    if (is_pop_count_age_sex_reference(x)) return(x)
    stop("Cannot coerce 'x' to 'pop_count_age_sex_reference'.")
}

#' @rdname coerce_pop_count_age_sex_reference
#' @export
as_pop_count_age_sex_reference.data.frame <- function(x, ...) {
    pop_count_age_sex_reference(as.data.frame(x))
}

#' @rdname coerce_pop_count_age_sex_reference
#' @export
as_pop_count_age_sex_reference.matrix <- function(x, ...) {
    as_pop_count_age_sex_reference(as.data.frame(NextMethod()))
}

#' @rdname coerce_pop_count_age_sex_reference
#' @export
as_pop_count_age_sex_reference.pop_count_age_sex_reference <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("pop_count_age_sex_reference", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmppWPP_object(x))
}

#' @rdname coerce_pop_count_age_sex_reference
#' @export
is_pop_count_age_sex_reference <- function(x) {
    inherits(x, "pop_count_age_sex_reference")
}




#' @rdname subset_demog_change_component_df
#' @export
subset_time.pop_count_age_sex_reference <- function(x, times, include = TRUE) {
    x <- NextMethod()
    return(pop_count_age_sex_reference(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.pop_count_age_sex_reference <- function(x, ages, include = TRUE) {
    x <- NextMethod()
    return(pop_count_age_sex_reference(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.pop_count_age_sex_reference <- function(x, sexes, include = TRUE) {
    x <- NextMethod()
    return(pop_count_age_sex_reference(x))
}
