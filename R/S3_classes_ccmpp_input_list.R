###-----------------------------------------------------------------------------
### * Helpers

## Names of all classes
get_all_ccmpp_list_class_names <- function() {
    c("ccmpp_input_list")
}

## Manage 'class' attribute
strip_ccmpp_input_list_classes_attribute <- function(class_att) {
    class_att[!(class_att %in% get_all_ccmpp_list_class_names())]
}

#' List the names of required elements of \code{ccmpp_input_list} objects
#'
#' The function simply returns a character vector naming all the
#' elements required for an object to be a valid
#' \code{ccmpp_input_list}.
#'
#' @return Character vector naming required elements.
#' @author Mark Wheldon
#' @export
get_all_required_ccmpp_input_list_element_names <- function() {
    c("pop_count_age_sex_base", "life_table_age_sex",
      "fert_rate_age_f",
      "srb", "mig_net_count_age_sex",
      "mig_net_rate_age_sex", "mig_net_count_tot_b",
      "mig_parameter")
}

## Required element classes
get_all_required_ccmpp_input_list_element_classes <- function() {
    get_all_required_ccmpp_input_list_element_names()
}

###-----------------------------------------------------------------------------
### * Class constructors

#' Low-level constructor for class \code{ccmpp_input_list}.
#'
#' @description
#' Creates an object of class \code{ccmpp_input_list}. Minimal
#' checks are done; for interactive use see
#' \code{\link{ccmpp_input_list}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{ccmpp_input_list}}.
#'
#' @seealso ccmpp_input_list
#'
#' @inheritParams new_ccmpp_input_df
#' @inheritParams new_fert_rate_age_f
#' @return An object of class \code{ccmpp_input_list}.
#' @author Mark Wheldon
new_ccmpp_input_list <-
    function(x,
             age_span = double(),
             time_span = double(),
             non_zero_fert_ages = double(),
             ..., class = character()) {
        if (missing(x)) x <- list()
        stopifnot(is.list(x))
        stopifnot(is.character(class))
        structure(x,
                  age_span = age_span,
                  time_span = time_span,
                  non_zero_fert_ages = non_zero_fert_ages,
                  ...,
                  class = c(class, "ccmpp_input_list", "list"))
    }


#' Constructor for class \code{ccmpp_input_list}
#'
#' This function creates objects of class \code{ccmpp_input_list}. It
#' takes as arguments a set of objects inheriting from
#' \code{\link{ccmpp_input_df}}.
#'
#' @seealso \code{\link{as_ccmpp_input_list.list}} for creating
#'     \code{ccmpp_input_list}s lists.
#'
#' @family ccmpp_input_objects
#'
#' @param pop_count_age_sex_base An object of class \code{\link{pop_count_age_sex_base}}
#' @param life_table_age_sex An object of class \code{\link{life_table_age_sex}}
#' @param fert_rate_age_f An object of class \code{\link{fert_rate_age_f}}
#' @param srb An object of class \code{\link{srb}}
#' @param mig_net_count_age_sex An object of class \code{\link{mig_net_count_age_sex}}
#' @param mig_net_rate_age_sex An object of class \code{\link{mig_net_rate_age_sex}}
#' @param mig_net_count_tot_b An object of class \code{\link{mig_net_count_tot_b}}
#' @param mig_parameter An object of class \code{\link{mig_parameter}}
#' @return An object of class \code{ccmpp_input_list}.
#' @author Mark Wheldon
#' @export
ccmpp_input_list <-
    function(pop_count_age_sex_base,
             life_table_age_sex,
             fert_rate_age_f,
             srb,
             mig_net_count_age_sex,
             mig_net_rate_age_sex,
             mig_net_count_tot_b,
             mig_parameter) {

        obj <- list(pop_count_age_sex_base = as_pop_count_age_sex_base(pop_count_age_sex_base),
                    life_table_age_sex = as_life_table_age_sex(life_table_age_sex),
                    fert_rate_age_f = as_fert_rate_age_f(fert_rate_age_f),
                    srb = as_srb(srb),
                    mig_net_count_age_sex = as_mig_net_count_age_sex(mig_net_count_age_sex),
                    mig_net_rate_age_sex = as_mig_net_rate_age_sex(mig_net_rate_age_sex),
                    mig_net_count_tot_b = as_mig_net_count_tot_b(mig_net_count_tot_b),
                    mig_parameter = as_mig_parameter(mig_parameter))

        age_span <- age_span(obj$mig_net_count_age_sex)
        time_span <- time_span(obj$mig_net_count_age_sex)
        non_zero_fert_ages <- suppressMessages(non_zero_fert_ages(obj$fert_rate_age_f))

        ## Create/Validate

        ## Individual elements are all validated by
        ## 'as_...' functions so don't redo that part.
        validate_ccmppWPP_object(
            new_ccmpp_input_list(obj,
                               age_span = age_span,
                               time_span = time_span,
                               non_zero_fert_ages = non_zero_fert_ages),
            .validate_elements = FALSE
        )
    }

###-----------------------------------------------------------------------------
### * Coercion

#' Coerce to a \code{ccmpp_input_list}
#'
#' These functions coerce an object to a \code{ccmpp_input_list} if
#' possible, or check if it is one. The list method requires that the
#' \code{names} of the single argument match the arguments to
#' \code{link{ccmpp_input_list}}.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{coerce_demog_change_component_df}} for an important note on validation.
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_ccmpp_input_list
#' @export
as_ccmpp_input_list <- function(x, ...) {
    UseMethod("as_ccmpp_input_list")
}

#' @rdname coerce_ccmpp_input_list
#' @export
as_ccmpp_input_list.default <- function(x, ...) {
    if (is_ccmpp_input_list(x)) return(x)
    stop("Cannot coerce 'x' to 'ccmpp_input_list'.")
}

#' @rdname coerce_ccmpp_input_list
#' @export
as_ccmpp_input_list.list <- function(x, ...) {

    req_el_names <- get_all_required_ccmpp_input_list_element_names()
    if (!identical(sort(names(x)),
                   sort(req_el_names)))
        stop("'x' must have these elements (no more): ",
             paste(req_el_names,
                   collapse = ", "))

        ccmpp_input_list(pop_count_age_sex_base = x[["pop_count_age_sex_base"]],
                         life_table_age_sex = x[["life_table_age_sex"]],
                         fert_rate_age_f = x[["fert_rate_age_f"]],
                         srb = x[["srb"]],
                         mig_net_count_age_sex = x[["mig_net_count_age_sex"]],
                         mig_net_rate_age_sex = x[["mig_net_rate_age_sex"]],
                         mig_net_count_tot_b = x[["mig_net_count_tot_b"]],
                         mig_parameter = x[["mig_parameter"]]
                         )
}

#' @rdname coerce_ccmpp_input_list
#' @export
as_ccmpp_input_list.ccmpp_input_list <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("ccmpp_input_list", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmppWPP_object(x))
}

#' @rdname coerce_ccmpp_input_list
#' @export
is_ccmpp_input_list <- function(x) {
    inherits(x, "ccmpp_input_list")
}


#' Convert Demo Data extract to a CCMPP input list
#'
#' Takes the output of \code{\link{DDextract_ccmppWPPinputs_tier1}}
#' and returns an object inheriting from
#' \code{\link{ccmpp_input_list}}. Note that this will discard some
#' components of the input (e.g., the reference population counts).
#'
#' This function extracts the CCMPP input list component from \code{x}
#' and checks that the components are consistent with each
#' other. Currenlty, this involves selecting rows for times that all
#' components have in common.
#'
#' @param x A list of the form returned by \code{\link{DDextract_ccmppWPPinputs_tier1}}.
#' @return An object inheriting from \code{\link{ccmpp_input_list}}.
#' @author Mark Wheldon
#' @export
DDextract_to_ccmpp_input_list <- function(x) {
    op <- getOption("ccmppWPP.suppress_S3_class_messages")
    options(ccmppWPP.suppress_S3_class_messages = TRUE)
    on.exit(options(ccmppWPP.suppress_S3_class_messages = op))

    ## Check
    stopifnot("ccmppWPP_inputs" %in% names(x))
    req_names <- c("pop_count_age_sex_base",
                   "life_table_age_sex", "fert_rate_age_f",
                   "srb", "mig_net_count_age_sex",
                   "mig_net_rate_age_sex", "mig_net_count_tot_b",
                   "mig_parameter")
    missing_nms <- req_names[!req_names %in% names(x$ccmppWPP_inputs)]
    if (length(missing_nms))
        stop("'x$ccmppWPP_inputs' must have all of the following elements: ",
             toString(req_names),
             ". ",
             toString(missing_nms),
             " is/are missing.")

    ## Make all ccmpp inputs have common times. Do this 'manually',
    ## i.e,. not using ccmppWPP functions. To use 'ccmppWPP::times'
    ## all elements have to be coerced to ccmpp_input_df objects first
    ## which is time conusming and wasteful because this will be done
    ## by 'as_ccmpp_input_list' anyway. A downside is that the name of
    ## the time columne ('time_start') is hardcoded.
    all_but_baseline_names <- req_names[!req_names %in% "pop_count_age_sex_base"]
    common_times <-
        Reduce("intersect",
               lapply(x$ccmppWPP_inputs[all_but_baseline_names], function(z) {
                   unique(z$time_start)
               }))
    x <- lapply(x$ccmppWPP_inputs[req_names], function(z, common_times) {
        z[z$time_start %in% common_times, ]
    }, common_times = common_times)

    return(as_ccmpp_input_list(x))
}


