###-----------------------------------------------------------------------------
### * Helpers

get_dimensions_info_for_ccmpp_input_classes <-
    function(classes = get_all_demog_change_component_df_class_names()) {
        db <- list(pop_count_base_input_df =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   fert_rate_input_df =
                       ensure_these_dimensions_correctly_ordered(c("time", "age")),
                   survival_ratio_input_df =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   srb_input_df =
                       ensure_these_dimensions_correctly_ordered(c("time")),
                   mig_net_count_input_df =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   mig_net_rate_input_df =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   mig_net_count_tot_input_df =
                       ensure_these_dimensions_correctly_ordered(c("time")),
                   life_table_input_df =
                       ensure_these_dimensions_correctly_ordered(
                           c("indicator", "time", "sex", "age")
                       ))
        if (identical(length(classes), 1L))
            return(db[[classes]])
        else return(db[names(db) %in% classes])
}

get_req_dimensions_for_ccmpp_input_classes <- function(classes) {
    get_dimensions_info_for_ccmpp_input_classes(classes)
}

###-----------------------------------------------------------------------------
### * Class constructors

#' Low-level constructor for class \code{ccmpp_input_df}.
#'
#' @description
#' Creates an object of class \code{ccmpp_input_df}. Minimal
#' checks are done; for interactive use see
#' \code{\link{ccmpp_input_df}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{ccmpp_input_df}}.
#'
#' @seealso ccmpp_input_df
#'
#' @family ccmpp_input_df class non-exported functions
#'
#' @inheritParams new_demog_change_component_df
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
                                      class = c(class, "ccmpp_input_df"))
    }


#' Constructor for class \code{ccmpp_input_df}
#'
#' **TBC** More strict version of \code{\link{demog_change_component_df}}.
#'
#' @family ccmpp_input_objects
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{ccmpp_input_df}.
#' @author Mark Wheldon
#' @export
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
        validate_ccmpp_object(
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


#' Coerce to a \code{ccmpp_input_df}
#'
#' These functions coerce an object to a
#' \code{ccmpp_input_df} if possible, or check if it is
#' one.
#'
#' @seealso \code{\link{coerce_demog_change_component_df}} for an important note on validation.
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_ccmpp_input_df
#' @export
as_ccmpp_input_df <- function(x, ...) {
    UseMethod("as_ccmpp_input_df")
}

#' @rdname coerce_ccmpp_input_df
#' @export
as_ccmpp_input_df.default <- function(x, ...) {
    if (is_ccmpp_input_df(x)) return(x)
    stop("Cannot coerce 'x' to 'ccmpp_input_df'.")
}

#' @rdname coerce_ccmpp_input_df
#' @export
as_ccmpp_input_df.data.frame <- function(x, ...) {
    ccmpp_input_df(as.data.frame(x))
}

#' @rdname coerce_ccmpp_input_df
#' @export
as_ccmpp_input_df.matrix <- function(x, ...) {
    as_ccmpp_input_df(as.data.frame(NextMethod()))
}

#' @rdname coerce_ccmpp_input_df
#' @export
as_ccmpp_input_df.ccmpp_input_df <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("ccmpp_input_df", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(x)
}

#' @rdname coerce_ccmpp_input_df
#' @export
is_ccmpp_input_df <- function(x) {
    inherits(x, "ccmpp_input_df")
}
