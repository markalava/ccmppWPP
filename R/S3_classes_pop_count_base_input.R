
#' Low-level constructor for class \code{pop_count_base_input_df}.
#'
#' @description
#' Creates an object of class \code{pop_count_base_input_df}. Minimal
#' checks are done; for interactive use see
#' \code{\link{pop_count_base_input_df}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{pop_count_base_input_df}}.
#'
#' @seealso pop_count_base_input_df
#'
#' @family pop_count_base_input_df class non-exported functions
#'
#' @param age_span Scalar indicating the span of the age groups.
#' @param time_span Scalar indicating the span of the time periods.
#' @param dimensions Character vector listing the dimensions such as
#'     \dQuote{age}, \dQuote{sex}.
#' @param value_type Scalar indicating the type of the \dQuote{value}
#'     column (e.g., \dQuote{count}, \dQuote{rate}, etc.).
#' @return An object of class \code{pop_count_base_input_df}.
#' @author Mark Wheldon
new_pop_count_base_input_df <-
    function(x,
             age_span = double(),
             time_span = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions =  get_req_dimensions_for_ccmpp_input_classes("pop_count_base_input_df"),
                           value_type = get_value_types_for_classes("pop_count_base_input_df"),
                           ...,
                           class = c(class, "pop_count_base_input_df"))
    }


#' Constructor for class \code{pop_count_base_input_df}
#'
#' \code{pop_count_base_input_df} is a subclass of
#' \code{\link{ccmpp_input_df}}. It imposes three additional conditions:
#' \enumerate{
#'   \item{\code{Value_type} attribute equals \dQuote{count}.}
#'   \item{Within year and sex, age must start at 0.}
#'   \item{There can be no 'time' dimension since, by definition, this
#'   object contains values for one time-period only.}}
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{validate_ccmpp_object}} for object validation,
#'     \code{\link{ccmpp_input_df}} for the class from which this one
#'     inherits.
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{pop_count_base_input_df}.
#' @author Mark Wheldon
#' @export
pop_count_base_input_df <-
    function(x,
             age_span = attr(x, "age_span"),
             time_span = attr(x, "time_span")) {



        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions =  get_req_dimensions_for_ccmpp_input_classes("mig_net_rate_input_df"),
                            value_type = get_value_types_for_classes("pop_count_base_input_df"))

        ## Create/Validate
        validate_ccmpp_object(
            new_pop_count_base_input_df(li$df,
                               age_span = li$age_span,
                               time_span = li$time_span)
        )
    }


#' Coerce to a \code{pop_count_base_input_df}
#'
#' These functions coerce an object to a
#' \code{pop_count_base_input_df} if possible, or check if it is
#' one.
#'
#' @seealso \code{\link{coerce_demog_change_component_df}} for an important note on validation.
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_pop_count_base_input_df
#' @export
as_pop_count_base_input_df <- function(x, ...) {
    UseMethod("as_pop_count_base_input_df")
}

#' @rdname coerce_mig_net_rate_input_df
#' @export
as_pop_count_base_input_df.default <- function(x, ...) {
    if (is_pop_count_base_input_df(x)) return(x)
    stop("Cannot coerce 'x' to 'pop_count_base_input_df'.")
}

#' @rdname coerce_pop_count_base_input_df
#' @export
as_pop_count_base_input_df.data.frame <- function(x, ...) {
    pop_count_base_input_df(as.data.frame(x))
}

#' @rdname coerce_pop_count_base_input_df
#' @export
as_pop_count_base_input_df.matrix <- function(x, ...) {
    as_pop_count_base_input_df(as.data.frame(NextMethod()))
}

#' @rdname coerce_mig_net_rate_input_df
#' @export
as_pop_count_base_input_df.pop_count_base_input_df <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("pop_count_base_input_df", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(x)
}

#' @rdname coerce_pop_count_base_input_df
#' @export
is_pop_count_base_input_df <- function(x) {
    inherits(x, "pop_count_base_input_df")
}
