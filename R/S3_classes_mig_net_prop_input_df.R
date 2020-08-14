###-----------------------------------------------------------------------------
### * Utilities

## make_mig_net_prop <- function(mig_counts, pop_counts,
##                               by_vars_names = NULL, value_var_name = "value") {
##     mig_counts <- as.data.frame(mig_counts)
##     pop_counts <- as.data.frame(pop_counts)
##     if (is.null(by_vars_names)) {
##         by_vars_names <- intersect(names(mig_counts), names(pop_counts))
##         by_vars_names <- by_vars_names[!by_vars_names == value_var_name]
##     }
##     base::merge(mig_counts, pop_counts)

###-----------------------------------------------------------------------------
### * Object class

#' Low-level constructor for class \code{mig_net_prop_age_sex}.
#'
#' @description
#' Creates an object of class \code{mig_net_prop_age_sex}. Minimal
#' checks are done; for interactive use see
#' \code{\link{mig_net_prop_age_sex}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{mig_net_prop_age_sex}}.
#'
#' @seealso mig_net_prop_age_sex
#'
#' @inheritParams new_demog_change_component_df
#' @return An object of class \code{mig_net_prop_age_sex}.
#' @author Mark Wheldon
new_mig_net_prop_age_sex <-
    function(x,
             age_span = double(),
             time_span = double(),
             dimensions = get_req_dimensions_for_ccmpp_input_classes("mig_net_prop_age_sex"),
             value_type = get_value_types_for_ccmpp_input_classes("mig_net_prop_age_sex"),
             value_scale = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions = dimensions,
                           value_type = value_type,
                           value_scale = value_scale,
                           ...,
                           class = c(class, "mig_net_prop_age_sex"))
    }


#' Constructor for class \code{mig_net_prop_age_sex}
#'
#' \code{mig_net_prop_age_sex} is a subclass of
#' \code{\link{ccmpp_input_df}}. It imposes two additional conditions:
#' \enumerate{
#'   \item{\code{Value_type} attribute equals \dQuote{prop}.}
#'   \item{Within year and sex, age must start at 0.}}
#' \code{mig_net_prop_age_sex} objects are not a required element of
#' \code{\link{ccmpp_input_list}}.
#'
#' This function is generic with methods for \code{data.frame},
#' \code{ccmpp_input_list} and \code{mig_net_count_age_sex}. The
#' latter two methods will compute the proportions from counts of
#' migration and a baseline population by simple division of migration
#' counts by population counts, matched by age, sex, and time.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{validate_ccmpp_object}} for object validation,
#'     \code{\link{ccmpp_input_df}} for the class from which this one
#'     inherits.
#'
#' @param x Depending on the method
#' \describe{
#'   \item{\code{data.frame}}{A \code{\link{base::data.frame}} object}
#'   \item{\code{ccmpp_input_list}}{A \code{\link{ccmpp_input_list}} object}
#'   \item{\code{mig_net_count_age_sex}}{A \code{\link{mig_net_count_age_sex}} object}}
#'
#' @param pop_count_age_sex For the \code{mig_net_count_age_sex}
#'     method, an object of class \code{\link{ccmpp_input_df}} holding
#'     population counts from which to calculate the migration
#'     proportions. The \code{value_type} must be \dQuote{count}. If
#'     necessary, values will be rescaled (via
#'     \code{\link{rescale_value}}) so that they match the
#'     \code{value_scale(x)}.
#'
#' @param strict Logical. If \code{TRUE} (default) \code{x} and
#'     \code{pop_count_age_sex} must have the same time, age, and sex
#'     values, i.e., an \dQuote{inner} join on these variables must be
#'     equivalent to an \dQuote{outer} join. If this is not the case
#'     an error will be signalled. If \code{strict = FALSE} then an
#'     \dQuote{inner} join will be performed and the result will have
#'     only those age, time, sex combinations in both sets of
#'     counts. This may result in an object of class
#'     \code{demog_change_component_df} being returned; see
#'     \dQuote{Details}.
#'
#' @inheritParams demog_change_component_df
#'
#' @return An object of class \code{mig_net_prop_age_sex} with the
#'     same \code{\link{value_scale}} as \code{x}.
#' @author Mark Wheldon
#' @name mig_net_prop_age_sex
#' @export
mig_net_prop_age_sex <- function(x, ...) {
    UseMethod("mig_net_prop_age_sex")
}

#' @rdname mig_net_prop_age_sex
#' @export
mig_net_prop_age_sex.data.frame <-
    function(x,
             value_scale = attr(x, "value_scale"), ...) {

        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_ccmpp_input_classes("mig_net_prop_age_sex"),
                            value_type = get_value_types_for_ccmpp_input_classes("mig_net_prop_age_sex"),
                            value_scale = value_scale)

        ## Create/Validate
        validate_ccmpp_object(
            new_mig_net_prop_age_sex(li$df,
                               age_span = li$age_span,
                               time_span = li$time_span,
                               value_scale = li$value_scale)
        )
    }

#' @rdname mig_net_prop_age_sex
#' @export
mig_net_prop_age_sex.ccmpp_input_list <-
    function(x, strict = TRUE, ...) {
        pop_count_age_sex <-
            rbind(x$pop_count_age_sex_base,
                  data_reshape_ccmpp_output(
                      project_ccmpp_loop_over_time(indata = x))$pop_count_age_sex)
        pop_count_age_sex <-
            pop_count_age_sex[pop_count_age_sex$sex %in% c("male", "female"),]
        mig_net_prop_age_sex(x = mig_net_count_component(x),
                             pop_count_age_sex = pop_count_component(x),
                             strict = strict, ...)
    }

#' @rdname mig_net_prop_age_sex
#' @export
mig_net_prop_age_sex.mig_net_count_age_sex <-
    function(x, pop_count_age_sex,
             value_scale = attr(mig_net_count_age_sex, "value_scale"),
             strict = TRUE, ...) {
        pop_count_age_sex <- as_ccmpp_input_df(pop_count_age_sex)
        stopifnot(identical(sort(as.numeric(x$age_start)),
                            sort(as.numeric(pop_count_age_sex$age_start))))
        stopifnot(identical(sort(as.numeric(x$sex)),
                            sort(as.numeric(pop_count_age_sex$sex))))
        stopifnot(identical(sort(as.numeric(x$time_start)),
                            sort(as.numeric(pop_count_age_sex$time_start))))
        x <- merge(x = x, y = pop_count_age_sex,
                         by = c("age_start", "sex", "time_start"))

    }


#' Coerce to a \code{mig_net_prop_age_sex}
#'
#' These functions coerce an object to a
#' \code{mig_net_prop_age_sex} if possible, or check if it is
#' one.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{coerce_demog_change_component_df}}
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_mig_net_prop_age_sex
#' @export
as_mig_net_prop_age_sex <- function(x, ...) {
    UseMethod("as_mig_net_prop_age_sex")
}

#' @rdname coerce_mig_net_prop_age_sex
#' @export
as_mig_net_prop_age_sex.default <- function(x, ...) {
    if (is_mig_net_prop_age_sex(x)) return(x)
    stop("Cannot coerce 'x' to 'mig_net_prop_age_sex'.")
}

#' @rdname coerce_mig_net_prop_age_sex
#' @export
as_mig_net_prop_age_sex.data.frame <- function(x, ...) {
    mig_net_prop_age_sex(as.data.frame(x))
}

#' @rdname coerce_mig_net_prop_age_sex
#' @export
as_mig_net_prop_age_sex.matrix <- function(x, ...) {
    as_mig_net_prop_age_sex(as.data.frame(NextMethod()))
}

#' @rdname coerce_mig_net_prop_age_sex
#' @export
as_mig_net_prop_age_sex.mig_net_prop_age_sex <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("mig_net_prop_age_sex", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmpp_object(x))
}

#' @rdname coerce_mig_net_prop_age_sex
#' @export
is_mig_net_prop_age_sex <- function(x) {
    inherits(x, "mig_net_prop_age_sex")
}



#' @rdname subset_demog_change_component_df
#' @export
subset_time.mig_net_prop_age_sex <- function(x, times, drop = FALSE) {

    x <- NextMethod()
    return(mig_net_prop_age_sex(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.mig_net_prop_age_sex <- function(x, ages, drop = FALSE) {

    x <- NextMethod()
    return(mig_net_prop_age_sex(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_sex.mig_net_prop_age_sex <- function(x, sexes, drop = FALSE) {

    x <- NextMethod()
    return(mig_net_prop_age_sex(x))
}
