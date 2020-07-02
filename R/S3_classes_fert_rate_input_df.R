
## Not for export
validate_non_zero_fert_ages <- function(x, ages) {
    ages <- sort(unique(ages))
    diffs <- diff(ages, differences = 1)
    if (all(diffs == age_span(x))) return(ages)
    else return(stop("'non_zero_fert_ages' are not valid. See '?fert_rate_input_df' for requirements."))
}

## Not for export; returns 'FALSE' if fails, rather than an error.
guess_non_zero_fert_ages <- function(x, digits = 9) {
    ages <- x$age_start[!(round(x$value, digits = digits) == 0)]
    ages <- try(validate_non_zero_fert_ages(x, ages), silent = TRUE)
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
#' @family fert_rate_input_df class non-exported functions
#'
#' @param age_span Scalar indicating the span of the age groups.
#' @param time_span Scalar indicating the span of the time periods.
#' @param dimensions Character vector listing the dimensions such as
#'     \dQuote{time}, \dQuote{age}, \dQuote{sex}.
#' @param value_type Scalar indicating the type of the \dQuote{value}
#'     column (e.g., \dQuote{count}, \dQuote{rate}, etc.).
#' @return An object of class \code{fert_rate_input_df}.
#' @author Mark Wheldon
new_fert_rate_input_df <-
    function(x, dimensions = character(),
             age_span = double(),
             time_span = double(),
             non_zero_fert_ages = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions = dimensions,
                           value_type = "rate",
                           non_zero_fert_ages = non_zero_fert_ages,
                           ...,
                           class = c(class, "fert_rate_input_df"))
    }


#' Constructor for class \code{fert_rate_input_df}
#'
#' \code{fert_rate_input_df} is a subclass of
#' \code{\link{ccmpp_input_df}}. It imposes two additional conditions:
#' \enumerate{
#'   \item{\code{Value_type} attribute equals \dQuote{rate}.}
#'   \item{The additional attribute \code{non_zero_fert_ages}
#'   represents the reproductive age range as a vector of
#'   \code{age_start} values.}}
#' An attempt will be made to guess \code{non_zero_fert_ages} if not supplied.
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
             dimensions = attr(x, "dimensions"),
             age_span = attr(x, "age_span"),
             time_span = attr(x, "time_span"),
             non_zero_fert_ages = attr(x, "non_zero_fert_ages"),
             ...) {

        x <- ccmpp_input_df(x,
                            dimensions = dimensions,
                            age_span = age_span,
                            time_span = time_span,
                            value_type = "rate",
                            ...)

        if (is_by_age(x)) {
            if (is.null(non_zero_fert_ages)) {
                non_zero_fert_ages <- guess_non_zero_fert_ages(x)
            }
            if (is.logical(non_zero_fert_ages)) {
                if (!non_zero_fert_ages)  {
                    message("'non_zero_fert_ages' not supplied and guessing failed; setting to 'sort(unique(x$age_start))'.")
                    non_zero_fert_ages <- sort(unique(x$age_start))
                }
            } else {
                message("'non_zero_fert_ages' set to '",
                        toString(non_zero_fert_ages, width = 20))
            }
        }

        ## Create/Validate
        validate_ccmpp_object(
            new_fert_rate_input_df(x,
                               dimensions = attr(x, "dimensions"),
                               age_span = attr(x, "age_span"),
                               time_span = attr(x, "time_span"),
                               non_zero_fert_ages = non_zero_fert_ages,
                               ...
                               )
        )
    }
