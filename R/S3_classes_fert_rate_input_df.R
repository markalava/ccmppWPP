
## Not for export
validate_non_zero_fert_ages <- function(x, ages, age_span = NULL) {
    if (is.null(age_span)) age_span <- attr(x, "age_span")
    if (is.null(age_span)) stop("Need 'age_span' to verify 'non_zero_fert_rate_ages' but 'age_span = NULL' or 'attr(x, \"age_span\") is 'NULL'.")
    ages <- sort(unique(ages))
    diffs <- diff(ages, differences = 1)
    if (all(diffs == age_span)) return(ages)
    else return(stop("'non_zero_fert_ages' are not valid. See '?fert_rate_input_df' for requirements."))
}

## Not for export; returns 'FALSE' if fails, rather than an error.
guess_non_zero_fert_ages <- function(x, digits = 9, age_span = attr(x, "age_span")) {
    ages <- x$age_start[!(round(x$value, digits = digits) == 0)]
    ages <- try(validate_non_zero_fert_ages(x, ages, age_span = age_span), silent = TRUE)
    if (inherits(ages, "try-error")) return(FALSE)
    else return(ages)
}


#' Low-level constructor for class \code{fert_rate_input_df}.
#'
#' @description
#' Creates an object of class \code{fert_rate_input_df}. Minimal
#' checks are done; for interactive use see
#' \code{\link{fert_rate_input_df}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{fert_rate_input_df}}.
#'
#' @seealso fert_rate_input_df
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{fert_rate_input_df}.
#' @author Mark Wheldon
new_fert_rate_input_df <-
    function(x,
             age_span = double(),
             time_span = double(),
             non_zero_fert_ages = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions = get_req_dimensions_for_ccmpp_input_classes("fert_rate_input_df"),
                           value_type = get_value_types_for_classes("fert_rate_input_df"),
                           non_zero_fert_ages = non_zero_fert_ages,
                           ...,
                           class = c(class, "fert_rate_input_df"))
    }


#' Constructor for class \code{fert_rate_input_df}
#'
#' \code{fert_rate_input_df} is a subclass of
#' \code{\link{ccmpp_input_df}}. It imposes three additional conditions:
#' \enumerate{
#'   \item{\code{Value_type} attribute equals \dQuote{rate}.}
#'   \item{There can be no 'sex' dimension; all fertility rate inputs
#'   must be female fertility rates.}
#'   \item{The additional attribute \code{non_zero_fert_ages}
#'   represents the reproductive age range as a vector of
#'   \code{age_start} values.}}
#' An attempt will be made to guess \code{non_zero_fert_ages} if not supplied.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{validate_ccmpp_object}} for object validation,
#'     \code{\link{ccmpp_input_df}} for the class from which this one
#'     inherits.
#'
#' @section Non-zero fert ages:
#' \code{non_zero_fert_ages} must be a subset of \code{ages(x)} and
#' equally spaced, with spacing equal to \code{age_span(x)}.
#'
#' @inheritParams demog_change_component_df
#' @param non_zero_fert_ages Numeric vector of unique ages indicating
#'     the reproductive age range.
#' @return An object of class \code{fert_rate_input_df}.
#' @author Mark Wheldon
#' @export
fert_rate_input_df <-
    function(x,
             non_zero_fert_ages = attr(x, "non_zero_fert_ages")) {

        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_ccmpp_input_classes("fert_rate_input_df"),
                            value_type = get_value_types_for_classes("fert_rate_input_df"))

        if (is.null(non_zero_fert_ages)) {
            non_zero_fert_ages <- guess_non_zero_fert_ages(li$df, age_span = li$age_span)
        }
        if (is.logical(non_zero_fert_ages)) {
            if (!non_zero_fert_ages)  {
                message("'non_zero_fert_ages' not supplied and guessing failed; setting to 'sort(unique(x$age_start))'.")
                non_zero_fert_ages <- sort(unique(li$df$age_start))
            }
        } else {
            message("'non_zero_fert_ages' set to '",
                    toString(non_zero_fert_ages, width = 20))
        }

        ## Create/Validate
        validate_ccmpp_object(
            new_fert_rate_input_df(li$df,
                               age_span = li$age_span,
                               time_span = li$time_span,
                               non_zero_fert_ages = non_zero_fert_ages)
        )
    }


#' Coerce to a \code{fert_rate_input_df}
#'
#' These functions coerce an object to a
#' \code{fert_rate_input_df} if possible, or check if it is
#' one.
#'
#' @seealso \code{\link{coerce_demog_change_component_df}} for an important note on validation.
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_fert_rate_input_df
#' @export
as_fert_rate_input_df <- function(x, ...) {
    UseMethod("as_fert_rate_input_df")
}

#' @rdname coerce_fert_rate_input_df
#' @export
as_fert_rate_input_df.default <- function(x, ...) {
    if (is_fert_rate_input_df(x)) return(x)
    stop("Cannot coerce 'x' to 'fert_rate_input_df'.")
}

#' @rdname coerce_fert_rate_input_df
#' @export
as_fert_rate_input_df.data.frame <- function(x, ...) {
    fert_rate_input_df(as.data.frame(x))
}

#' @rdname coerce_fert_rate_input_df
#' @export
as_fert_rate_input_df.matrix <- function(x, ...) {
    as_fert_rate_input_df(as.data.frame(NextMethod()))
}

#' @rdname coerce_fert_rate_input_df
#' @export
as_fert_rate_input_df.fert_rate_input_df <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("fert_rate_input_df", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(x)
}

#' @rdname coerce_fert_rate_input_df
#' @export
is_fert_rate_input_df <- function(x) {
    inherits(x, "fert_rate_input_df")
}



#' @rdname subset_demog_change_component_df
#' @export
subset_time.fert_rate_input_df <- function(x, time, drop = FALSE) {

    x <- NextMethod()
    return(fert_rate_input_df(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.fert_rate_input_df <- function(x, age, drop = FALSE) {

    x <- NextMethod()
    return(fert_rate_input_df(x))
}
