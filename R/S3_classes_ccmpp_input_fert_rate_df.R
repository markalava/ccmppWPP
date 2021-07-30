###-----------------------------------------------------------------------------
### * Helpers

get_non_zero_fert_ages_tolerance_digits <- function() return(9)

## Not for export
validate_non_zero_fert_ages <- function(x, ages, age_span = NULL) {
    if (is.null(age_span)) age_span <- attr(x, "age_span")
    if (is.null(age_span)) stop("Need 'age_span' to verify 'non_zero_fert_rate_ages' but 'age_span = NULL' or 'attr(x, \"age_span\") is 'NULL'.")
    ages <- sort(unique(ages))
    diffs <- diff(ages, differences = 1)
    if (all(diffs == age_span)) return(ages)
    else return(stop("'non_zero_fert_ages' are not valid. See '?fert_rate_age_f' for requirements."))
}

## Not for export; returns 'FALSE' if fails, rather than an error.
guess_non_zero_fert_ages <- function(x, digits = get_non_zero_fert_ages_tolerance_digits(),
                                     age_span = attr(x, "age_span")) {
    ages <- x$age_start[!(round(x$value, digits = digits) == 0)]
    ages <- try(validate_non_zero_fert_ages(x, ages, age_span = age_span), silent = TRUE)
    if (inherits(ages, "try-error")) return(FALSE)
    else return(ages)
}

print_non_zero_fert_ages <- function(nzfa, width = 30) {
    nzfa_paste <- paste(nzfa, collapse = ", ")
    if (nchar(nzfa_paste) <= width)
        return(nzfa_paste)

    nzfa_hlf_len <- floor(length(nzfa) * 0.5)
    avail_width <- width - 5 #for ' ... ' in middle
    avail_width_hlf_len <- floor(avail_width * 0.5)

    return(paste0(substr(nzfa_paste, 1, avail_width_hlf_len),
          " ... ",
          substr(nzfa_paste, nchar(nzfa_paste) - avail_width_hlf_len + 1,
                 nchar(nzfa_paste))))
}

## Not for export; overwrite elements of 'value' column with exact zeros.
set_zero_fert_ages_to_zero <- function(x, non_zero_fert_ages) {
    zero_fert_ages_idx <- !(x$age_start %in% non_zero_fert_ages)
    if (sum(zero_fert_ages_idx) > 0)
        x[zero_fert_ages_idx,]$value <- as.double(0)
    return(x)
    }


###-----------------------------------------------------------------------------
### * Constructors, etc.

#' Low-level constructor for class \code{fert_rate_age_f}.
#'
#' @description
#' Creates an object of class \code{fert_rate_age_f}. Minimal
#' checks are done; for interactive use see
#' \code{\link{fert_rate_age_f}}.
#'
#' This function is not exported. The user-level constructor is
#' \code{\link{fert_rate_age_f}}.
#'
#' @seealso fert_rate_age_f
#'
#' @inheritParams demog_change_component_df
#' @return An object of class \code{fert_rate_age_f}.
#' @author Mark Wheldon
new_fert_rate_age_f <-
    function(x,
             age_span = double(),
             time_span = double(),
             dimensions = get_req_dimensions_for_subclass_classes("fert_rate_age_f"),
             value_type = get_value_types_for_subclass_classes("fert_rate_age_f"),
             value_scale = double(),
             non_zero_fert_ages = double(),
             ..., class = character()) {
        new_ccmpp_input_df(x = x,
                           age_span = age_span,
                           time_span = time_span,
                           dimensions = dimensions,
                           value_type = value_type,
                           value_scale = value_scale,
                           non_zero_fert_ages = non_zero_fert_ages,
                           ...,
                           class = c(class, "fert_rate_age_f"))
    }


#' Constructor for class \code{fert_rate_age_f}
#'
#' \code{fert_rate_age_f} is a subclass of
#' \code{\link{ccmpp_input_df}}. It imposes four additional conditions:
#' \enumerate{
#'   \item{The \code{value_type} attribute is \dQuote{rate}.}
#'   \item{All elements of the \dQuote{value} column must be non-negative.}
#'   \item{There can be no 'sex' dimension; all fertility rate inputs
#'   must be female fertility rates.}
#'   \item{An additional attribute \code{non_zero_fert_ages}, which
#'   specifies the reproductive age range as a vector of
#'   \code{age_start} values (see \dQuote{Details}).}}
#'
#' Methods are defined for \code{\link{data.frame}}s and
#' \code{\link{ccmpp_input_list}}s, and possibly other objects as
#' well. The \code{data.frame} method \dQuote{constructs} an object
#' from \code{x}. The \code{ccmpp_input_list} method \dQuote{extracts}
#' an object from \code{x}. There is also a replacement function which
#' complements the extraction methods.
#'
#' An attempt will be made to guess \code{non_zero_fert_ages} if they
#' are not explicitly specified using the \code{non_zero_fert_ages}
#' argument.
#'
#' If \code{non_zero_fert_ages} are specified, they must be a subset
#' of \code{ages(x)} and be equally spaced with spacing equal to
#' \code{age_span(x)}.
#'
#' Elements of the \code{value} column corresponding to ages not
#' listed in \code{non_zero_fert_ages} will be overwritten with zeros
#' (specifically \code{as.double(0)}s).
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{validate_ccmppWPP_object}} for object validation,
#'     \code{\link{ccmpp_input_df}} for the class from which this one
#'     inherits.
#'
#' @param x An object for which a method is defined (see \dQuote{Details}).
#' @inheritParams demog_change_component_df
#' @param non_zero_fert_ages Numeric vector of unique ages indicating
#'     the reproductive age range. See the \dQuote{Details} section.
#' @return An object of class \code{fert_rate_age_f}.
#' @author Mark Wheldon
#' @export
fert_rate_age_f <- function(x, ...) {
    UseMethod("fert_rate_age_f")
}

#' @rdname fert_rate_age_f
#' @export
fert_rate_age_f.data.frame <-
    function(x,
             non_zero_fert_ages = attr(x, "non_zero_fert_ages"),
             value_scale = attr(x, "value_scale")) {

        li <- prepare_df_for_ccmpp_input_df(x,
                            dimensions = get_req_dimensions_for_subclass_classes("fert_rate_age_f"),
                            value_type = get_value_types_for_subclass_classes("fert_rate_age_f"),
                            value_scale = value_scale)

        if (is.null(non_zero_fert_ages)) {
            non_zero_fert_ages <- guess_non_zero_fert_ages(li$df, age_span = li$age_span)
        }
        if (is.logical(non_zero_fert_ages)) {
            if (!non_zero_fert_ages)  {
                S3_class_message("'non_zero_fert_ages' not supplied and guessing failed; setting to 'sort(unique(x$age_start))'.")
                non_zero_fert_ages <- sort(unique(li$df$age_start))
            }
        } else {
            S3_class_message("'non_zero_fert_ages' set to '",
                    print_non_zero_fert_ages(non_zero_fert_ages, width = 30))
        }

        li$df <- set_zero_fert_ages_to_zero(li$df, non_zero_fert_ages)


        ## Create/Validate
        validate_ccmppWPP_object(
            new_fert_rate_age_f(li$df,
                               age_span = li$age_span,
                               time_span = li$time_span,
                               value_scale = li$value_scale,
                               non_zero_fert_ages = non_zero_fert_ages)
        )
    }

#' @rdname fert_rate_age_f
#' @export
fert_rate_age_f.ccmpp_input_list <- function(x) {
    fert_rate_component(x)
}

#' @rdname fert_rate_age_f
#' @export
`fert_rate_age_f<-` <- function(x, value) {
    `fert_rate_component<-`(x, value)
}


###-----------------------------------------------------------------------------
### * Coercion

#' Coerce to a \code{fert_rate_age_f}
#'
#' These functions coerce an object to a
#' \code{fert_rate_age_f} if possible, or check if it is
#' one.
#'
#' @family ccmpp_input_objects
#' @seealso \code{\link{coerce_demog_change_component_df}}
#'
#' @inheritParams coerce_demog_change_component_df
#' @return A coerced object in the case of the \code{as_...}
#'     functions; a logical for the \code{is_...} functions.
#' @author Mark Wheldon
#' @name coerce_fert_rate_age_f
#' @export
as_fert_rate_age_f <- function(x, ...) {
    UseMethod("as_fert_rate_age_f")
}

#' @rdname coerce_fert_rate_age_f
#' @export
as_fert_rate_age_f.default <- function(x, ...) {
    if (is_fert_rate_age_f(x)) return(x)
    stop("Cannot coerce 'x' to 'fert_rate_age_f'.")
}

#' @rdname coerce_fert_rate_age_f
#' @export
as_fert_rate_age_f.data.frame <- function(x, ...) {
    fert_rate_age_f(as.data.frame(x))
}

#' @rdname coerce_fert_rate_age_f
#' @export
as_fert_rate_age_f.matrix <- function(x, ...) {
    as_fert_rate_age_f(as.data.frame(NextMethod()))
}

#' @rdname coerce_fert_rate_age_f
#' @export
as_fert_rate_age_f.fert_rate_age_f <- function(x, ...) {
    ## copied from  'as.data.frame'
    cl <- oldClass(x)
    i <- match("fert_rate_age_f", cl)
    if (i > 1L)
        class(x) <- cl[-(1L:(i - 1L))]
    return(validate_ccmppWPP_object(x))
}

#' @rdname coerce_fert_rate_age_f
#' @export
is_fert_rate_age_f <- function(x) {
    inherits(x, "fert_rate_age_f")
}


###-----------------------------------------------------------------------------
### * Subset

#' @rdname subset_demog_change_component_df
#' @export
subset_time.fert_rate_age_f <- function(x, times, include = TRUE) {
    x <- NextMethod()
    return(fert_rate_age_f(x))
}

#' @rdname subset_demog_change_component_df
#' @export
subset_age.fert_rate_age_f <- function(x, ages, include = TRUE) {
    x <- NextMethod()
    return(fert_rate_age_f(x))
}


###-----------------------------------------------------------------------------
### * Attributes

#' @rdname extract_demog_change_component_attributes
#' @export
non_zero_fert_ages <- function(x) {
    UseMethod("non_zero_fert_ages")
}

#' @rdname extract_demog_change_component_attributes
#' @export
non_zero_fert_ages.fert_rate_age_f <- function(x) {
    attr(x, "non_zero_fert_ages")
}

#' @rdname extract_demog_change_component_attributes
#' @export
`non_zero_fert_ages<-` <- function(x, value, ...) {
    UseMethod("non_zero_fert_ages<-")
}

#' @rdname extract_demog_change_component_attributes
#' @export
`non_zero_fert_ages<-.fert_rate_age_f` <- function(x, value, ...) {
    value <- validate_non_zero_fert_ages(x, value)
    age_diff <- setdiff(value, non_zero_fert_ages(x))
    if (length(age_diff))
        S3_class_warning("The non-zero fertility age range is being expanded beyond the previous range. Ages ",
                         print_non_zero_fert_ages(age_diff),
                         " will have fertility rates of zero.")
    fert_rate_age_f(x, non_zero_fert_ages = value)
}


###-----------------------------------------------------------------------------
### * Transformations

#' Drop zero fertility rate ages
#'
#' Drops zero fertility rate ages from a \code{fert_rate_age_df}
#' object. Note that the result is \emph{not valid} as a member of
#' this class so a \code{\link{demog_change_component_df}} will be
#' returned.
#'
#' @param x An object inheriting from \code{fert_rate_age_df}
#' @param ... Not implemented
#' @return A \code{demog_change_component_df} with the only the rows
#'     corresponding to \code{non_zero_fert_ages(x)}.
#' @author Mark Wheldon
#' @export
drop_zero_fert_ages <- function(x, ...) {
    UseMethod("drop_zero_fert_ages")
}

#' @rdname drop_zero_fert_ages
#' @export
drop_zero_fert_ages.fert_rate_age_f <- function(x) {
    subset_age(as_demog_change_component_df(x), non_zero_fert_ages(x))
}


#' Calculate total fertility rates
#'
#' This generic function returns total fertility rates from an object
#' with information on fertility.
#'
#' \code{tfr} is an alias for this function.
#'
#' @param x An object with information on fertility; typically
#'     inheriting from \code{\link{fert_rate_age_f}} or
#'     \code{\link{ccmpp_input_list}}.
#' @param ... Passed to specific methods
#' @return An object of class \code{\link{demog_change_component_df}}
#' @author Mark Wheldon
#' @name fert_rate_tot_f
#' @export
fert_rate_tot_f <- function(x, ...) {
    UseMethod("fert_rate_tot_f")
}

#' @rdname fert_rate_tot_f
#' @export
tfr <- fert_rate_tot_f

#' @rdname fert_rate_tot_f
#' @export
fert_rate_tot_f.fert_rate_age_f <- function(x) {
    wtd_value <-
        data.frame(value = x$age_start %in% non_zero_fert_ages(x) *
                       x$value * x$age_span)
    out <- stats::aggregate(wtd_value, by = list(time_start = x$time_start),
                     FUN = "sum")
    return(demog_change_component_df(out))
}

