
#' Low-level constructor for class \code{pop_count_age_sex_base}.
#'
#' @description
#' Creates an object of class \code{pop_count_age_sex_base}. Minimal
#' checks are done; for interactive use see
#' \code{\link{pop_count_age_sex_base}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{pop_count_age_sex_base}}.
#'
#' @seealso pop_count_age_sex_base
#'
#' @family pop_count_age_sex_base class non-exported functions
#'
#' @param age_span Scalar indicating the span of the age groups.
#' @param time_span Scalar indicating the span of the time periods.
#' @param value_scale \emph{Numeric} scalar indicating the value_scale of the
#'     counts, e.g., \code{1}, \code{1000}, \code{1e6}, etc.
#' @param dimensions Character vector listing the dimensions such as
#'     \dQuote{age}, \dQuote{sex}.
#' @param value_type Scalar indicating the type of the \dQuote{value}
#'     column (e.g., \dQuote{count}, \dQuote{rate}, etc.).
#' @return An object of class \code{pop_count_age_sex_base}.
#' @author Mark Wheldon
new_pop_count_age_sex_base <-
    function(x,
             age_span = double(),
             time_span = double(),
             dimensions =  get_req_dimensions_for_ccmpp_input_classes("pop_count_age_sex_base"),
             value_type = get_value_types_for_ccmpp_input_classes("pop_count_age_sex_base"),
             value_scale = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions = dimensions,
                           value_type = value_type,
                           value_scale = value_scale,
                           ...,
                           class = c(class, "pop_count_age_sex_base"))
    }


#' Constructor for class \code{pop_count_age_sex_base}
#'
#' \code{pop_count_age_sex_base} is a subclass of
#' \code{\link{ccmpp_input_df}}. It imposes three additional conditions:
#' \enumerate{
#'   \item{\code{Value_type} attribute equals \dQuote{count}.}
#'   \item{Within year and sex, age must start at 0.}
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{validate_ccmpp_object}} for object validation,
#'     \code{\link{ccmpp_input_df}} for the class from which this one
#'     inherits.
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{pop_count_age_sex_base}.
#' @author Mark Wheldon
#' @export
pop_count_age_sex_base <-
    function(x,
             age_span = attr(x, "age_span"),
             time_span = attr(x, "time_span"),
             value_scale = attr(x, "value_scale")) {

        if (is.null(value_scale)) value_scale <- 1
        else if (!is.numeric(value_scale)) stop("'value_scale' must be numeric.")

        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions =  get_req_dimensions_for_ccmpp_input_classes("pop_count_age_sex_base"),
                            value_type = get_value_types_for_ccmpp_input_classes("pop_count_age_sex_base"),
                            value_scale = value_scale)

        ## Create/Validate
        validate_ccmpp_object(
            new_pop_count_age_sex_base(li$df,
                               age_span = li$age_span,
                               time_span = li$time_span,
                               value_scale = li$value_scale)
        )
    }


#' Coerce to a \code{pop_count_age_sex_base}
#'
#' These functions coerce an object to a
#' \code{pop_count_age_sex_base} if possible, or check if it is
#' one.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{coerce_demog_change_component_df}}
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_pop_count_age_sex_base
#' @export
as_pop_count_age_sex_base <- function(x, ...) {
    UseMethod("as_pop_count_age_sex_base")
}

#' @rdname coerce_mig_net_rate_age_sex
#' @export
as_pop_count_age_sex_base.default <- function(x, ...) {
    if (is_pop_count_age_sex_base(x)) return(x)
    stop("Cannot coerce 'x' to 'pop_count_age_sex_base'.")
}

#' @rdname coerce_pop_count_age_sex_base
#' @export
as_pop_count_age_sex_base.data.frame <- function(x, ...) {
    pop_count_age_sex_base(as.data.frame(x))
}

#' @rdname coerce_pop_count_age_sex_base
#' @export
as_pop_count_age_sex_base.matrix <- function(x, ...) {
    as_pop_count_age_sex_base(as.data.frame(NextMethod()))
}

#' @rdname coerce_mig_net_rate_age_sex
#' @export
as_pop_count_age_sex_base.pop_count_age_sex_base <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("pop_count_age_sex_base", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmpp_object(x))
}

#' @rdname coerce_pop_count_age_sex_base
#' @export
is_pop_count_age_sex_base <- function(x) {
    inherits(x, "pop_count_age_sex_base")
}



#' @rdname subset_demog_change_component_df
#' @export
subset_time.pop_count_age_sex_base <- function(x, times, drop = FALSE) {

    x <- NextMethod()
    return(pop_count_age_sex_base(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.pop_count_age_sex_base <- function(x, ages, drop = FALSE) {

    x <- NextMethod()
    return(pop_count_age_sex_base(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.pop_count_age_sex_base <- function(x, sexes, drop = FALSE) {

    x <- NextMethod()
    return(pop_count_age_sex_base(x))
}
