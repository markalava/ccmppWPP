
#' Low-level constructor for class \code{ccmpp_input_df}.
#'
#' @description
#' Creates an object of class \code{ccmpp_input_df}. Minimal
#' checks are done; for interactive use see
#' \code{\link{validate_ccmpp_input_df}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{ccmpp_input_df}}.
#'
#' @seealso ccmpp_input_df
#'
#' @family ccmpp_input_df class non-exported functions
#'
#' @param age_span Scalar indicating the span of the age groups.
#' @param time_span Scalar indicating the span of the time periods.
#' @param dimensions Character vector listing the dimensions such as
#'     \dQuote{time}, \dQuote{age}, \dQuote{sex}.
#' @param value_type Scalar indicating the type of the \dQuote{value}
#'     column (e.g., \dQuote{count}, \dQuote{rate}, etc.).
#' @return An object of class \code{ccmpp_input_df}.
#' @author Mark Wheldon
new_ccmpp_input_df <-
    function(x, dimensions = character(),
             age_span = double(),
             time_span = double(),
             value_type = character(),
             ..., class = character()) {
        new_demog_change_component_df(x = x,
                                      age_span = age_span,
                                      time_span = time_span,
                                      dimensions = dimensions,
                                      value_type = value_type,
                                      ...,
                                      class = "ccmpp_input_df")
    }


#' Constructor for class \code{ccmpp_input_df}
#'
#' **TBC** More strict version of \code{\link{demog_change_component_df}}.
#'
#' @param ... Passed to the low-level constructor.
#' @return An object of class \code{ccmpp_input_df}.
#' @author Mark Wheldon
#'
#' @name construct_ccmpp_input_df
NULL

#' @rdname construct_ccmpp_input_df
#' @export
ccmpp_input_df <-
    function(x,
             dimensions = attr(x, "dimensions"),
             age_span = attr(x, "age_span"),
             time_span = attr(x, "time_span"),
             value_type = attr(x, "value_type"),
             ...) {

        ## Create/Validate
        validate_demog_change_component_df(
            new_ccmpp_input_df(x,
                              dimensions = dimensions,
                              age_span = age_span,
                              time_span = time_span,
                              value_type = value_type,
                              ...,
                              class = "ccmpp_input_df"
                              )
        )
    }


#' @export
validate_demog_change_component.ccmpp_input_df <- function(x) {
    x <- NextMethod()
}
