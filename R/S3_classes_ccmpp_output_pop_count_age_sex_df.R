
#' Low-level constructor for class \code{pop_count_age_sex}.
#'
#' @description
#' Creates an object of class \code{pop_count_age_sex}. Minimal
#' checks are done; for interactive use see
#' \code{\link{pop_count_age_sex}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{pop_count_age_sex}}.
#'
#' @section Note:
#' There is no \code{age_span} argument. Population counts
#'     have time span 0.
#'
#' @seealso pop_count_age_sex
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{pop_count_age_sex}.
#' @author Mark Wheldon
new_pop_count_age_sex <-
    function(x,
             age_span = double(),
             dimensions = get_req_dimensions_for_ccmpp_in_out_classes("pop_count_age_sex"),
             value_type = get_value_types_for_ccmpp_in_out_classes("pop_count_age_sex"),
             value_scale = double(),
             ..., class = character()) {
        new_ccmpp_output_df(x = x,
                           age_span = age_span,
                           time_span = 0,
                           dimensions = dimensions,
                           value_type = value_type,
                           value_scale = value_scale,
                           ...,
                           class = c(class, "pop_count_age_sex"))
    }


#' Constructor for class \code{pop_count_age_sex}
#'
#' \code{pop_count_age_sex} is a subclass of
#' \code{\link{ccmpp_output_df}}. It imposes two additional
#' conditions: \enumerate{
#' \item{\code{Value_type} attribute equals \dQuote{count}}
#' \item{Within year and sex, age must start at 0.}}
#' It is intended as a class that can be used for the population
#' counts produced by the CCMPP.
#'
#' Methods for the creator function exist for
#' \code{\link{base::data.frame}}s and
#' \code{link{ccmpp_input_list}}s. The latter is a wrapper for
#' \code{\link{project_ccmpp_loop_over_time}}; it implements cohort
#' component projection on the input list and returns the projected
#' counts as a \code{pop_count_age_sex}
#' object. \code{get_projected_pop_counts} is a more descriptively
#' name wrapper for \code{pop_count_age_sex.ccmpp_input_list(x,
#' keep_baseline = FALSE)}. \code{ccmpp_input_list}s determine
#' completely the population counts in subsequent times. As such, the
#' \code{ccmpp_input_list} method can be viewed as a way of
#' \dQuote{extracting} these counts from the input.
#'
#' \code{pop_count_age_sex.ccmpp_input_list} and
#' \code{get_projected_pop_counts} return projected
#' counts as an object of class \code{pop_count_age_sex}. For an
#' alternative list output, see the underlying function
#' \code{\link{project_ccmpp_loop_over_time}}.
#'
#' @family ccmpp_output_objects
#' @seealso \code{\link{validate_ccmppWPP_object}} for object validation,
#'     \code{\link{ccmpp_output_df}} for the class from which this one
#'     inherits.
#'
#' @param x A \code{\link{base::data.frame}} or
#'     \code{link{ccmpp_input_list}} object.
#' @param keep_baseline Logical, \code{link{ccmpp_input_list}} method:
#'     should the baseline population counts be included in the
#'     output?
#' @inheritParams demog_change_component_df
#' @return An object of class \code{pop_count_age_sex}.
#' @author Mark Wheldon
#' @name pop_count_age_sex
#' @export
pop_count_age_sex <- function(x, ...) {
    UseMethod("pop_count_age_sex")
}

#' @rdname pop_count_age_sex
#' @export
pop_count_age_sex.data.frame <-
    function(x,
             value_scale = attr(x, "value_scale"), ...) {

        li <- prepare_df_for_ccmpp_output_df(x,
                            dimensions = get_req_dimensions_for_ccmpp_in_out_classes("pop_count_age_sex"),
                            value_type = get_value_types_for_ccmpp_in_out_classes("pop_count_age_sex"),
                            value_scale = value_scale)

        ## Create/Validate
        validate_ccmppWPP_object(
            new_pop_count_age_sex(li$df,
                               age_span = li$age_span,
                               value_scale = li$value_scale)
        )
    }

#' @rdname pop_count_age_sex
#' @export
pop_count_age_sex.ccmpp_input_list <-
    function(x, keep_baseline = TRUE, ...) {

        val_scale_x <-
            as.numeric(value_scale(pop_count_base_component(x)))

        pop_out <- data_reshape_ccmpp_output(
            project_ccmpp_loop_over_time(indata = x))$pop_count_age_sex
        pop_out <- pop_out[pop_out$sex %in% c("male", "female"), ]
        if (keep_baseline)
            pop_out <- rbind(x$pop_count_age_sex_base, pop_out)

        pop_out <- prepare_df_for_ccmpp_output_df(pop_out,
                            dimensions = get_req_dimensions_for_ccmpp_in_out_classes("pop_count_age_sex"),
                            value_type = get_value_types_for_ccmpp_in_out_classes("pop_count_age_sex"),
                            value_scale = val_scale_x)

        ## Create/Validate
        validate_ccmppWPP_object(
            new_pop_count_age_sex(pop_out$df,
                               age_span = pop_out$age_span,
                               value_scale = pop_out$value_scale)
        )
    }

#' @rdname pop_count_age_sex
#' @aliases get_projected_pop_counts
#' @export
get_projected_pop_counts <- function(x, ...) {
    UseMethod("get_projected_pop_counts")
}

#' @rdname pop_count_age_sex
#' @aliases get_projected_pop_counts
#' @export
get_projected_pop_counts.ccmpp_input_list <- function(x) {
    pop_count_age_sex.ccmpp_input_list(x, keep_baseline = FALSE)
}


#' Coerce to a \code{pop_count_age_sex}
#'
#' These functions coerce an object to a
#' \code{pop_count_age_sex} if possible, or check if it is
#' one.
#'
#' @family ccmpp_output_objects
#' @seealso \code{\link{coerce_demog_change_component_df}}
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_pop_count_age_sex
#' @export
as_pop_count_age_sex <- function(x, ...) {
    UseMethod("as_pop_count_age_sex")
}

#' @rdname coerce_pop_count_age_sex
#' @export
as_pop_count_age_sex.default <- function(x, ...) {
    if (is_pop_count_age_sex(x)) return(x)
    stop("Cannot coerce 'x' to 'pop_count_age_sex'.")
}

#' @rdname coerce_pop_count_age_sex
#' @export
as_pop_count_age_sex.data.frame <- function(x, ...) {
    pop_count_age_sex(as.data.frame(x))
}

#' @rdname coerce_pop_count_age_sex
#' @export
as_pop_count_age_sex.matrix <- function(x, ...) {
    as_pop_count_age_sex(as.data.frame(NextMethod()))
}

#' @rdname coerce_pop_count_age_sex
#' @export
as_pop_count_age_sex.pop_count_age_sex <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("pop_count_age_sex", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmppWPP_object(x))
}

#' @rdname coerce_pop_count_age_sex
#' @export
is_pop_count_age_sex <- function(x) {
    inherits(x, "pop_count_age_sex")
}




#' @rdname subset_demog_change_component_df
#' @export
subset_time.pop_count_age_sex <- function(x, times, include = TRUE) {

    x <- NextMethod()
    return(pop_count_age_sex(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.pop_count_age_sex <- function(x, ages, include = TRUE) {

    x <- NextMethod()
    return(pop_count_age_sex(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.pop_count_age_sex <- function(x, sexes, include = TRUE) {

    x <- NextMethod()
    return(pop_count_age_sex(x))
}
