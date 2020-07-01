
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
#' #' @export
ccmpp_input_df <-
    function(x,
             dimensions = attr(x, "dimensions"),
             age_span = attr(x, "age_span"),
             time_span = attr(x, "time_span"),
             value_type = attr(x, "value_type"),
             ...) {

        x <- demog_change_component_df(x,
                                       dimensions = dimensions,
                                       age_span = age_span,
                                       time_span = time_span,
                                       value_type = value_type,
                                       ...)

        ## Create/Validate
        validate_demog_change_component_df(
            new_ccmpp_input_df(x,
                               dimensions = attr(x, "dimensions"),
                               age_span = attr(x, "age_span"),
                               time_span = attr(x, "time_span"),
                               value_type = attr(x, "value_type"),
                               ...,
                               class = "ccmpp_input_df"
                               )
        )
    }


#' @export
validate_demog_change_component_df.ccmpp_input_df <- function(x) {

    demog_change_component_dims_x <- demog_change_component_dimensions(x)

    ## -------* Must be Sorted

    ## If not sorted, at least by age and sex within time, the
    ## single-step ccmpp function will turn out incorrect
    ## results. The class imposes full sorting.

    order_cols <-
        get_dim_col_info(dimensions = demog_change_component_dims_x)$colname
    if (!identical(x[, order_cols],
                   sort_demog_change_component_df(x)[, order_cols]))
        stop("'x' must be sorted by time, rev(sex), age_start (see ?ccmpp_input_df for class definition).")

    ## -------* Spans

    attr_w_span_names <- get_attr_w_span_names()

    for (att in
         attr_w_span_names[attr_w_span_names %in% demog_change_component_dims_x]
         ) {
        ## Create names of the '_span' and '_start' variables for
        ## use later.
        span_name <- paste0(att, "_span")
        start_name <- paste0(att, "_start")

        ## Get the values of the attribute and column from x for
        ## use later.
        span_attr <- attr(x, span_name)
        start_col <- x[[start_name]]

        ## Diffs of unique values
        start_1st_diff <-
            diff(sort(unique(start_col)), differences = 1)

        ## Do the tests now:
        if (!identical(length(span_attr), 1L))
            stop("'", span_name, "' is not of length 1.")

        if (!identical(as.double(sum(start_1st_diff != span_attr)), 0))
            stop("Spacings between each 'x$", start_name,
                 "' do not equal 'attr(x, \"", span_name, "\")'.")
    }

    ## -------* Age

    if (is_by_age(x)) {
        min_age_start <- get_min_age_in_dims(x)
        if (!all(min_age_start == 0))
            stop("'age_start' does not start at '0' for each time * sex combination.")
    }

    ## -------* Base checks

    x <- NextMethod()
}
