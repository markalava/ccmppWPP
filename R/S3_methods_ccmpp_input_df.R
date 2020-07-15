###-----------------------------------------------------------------------------
### * Print, Summary, etc.

#' Print Values of a \code{ccmpp_input_df}
#'
#' This is a method for the generic \code{\link{base::print}} function. Only
#' the first \code{n} rows are printed for convenience (by default). If all rows are desired use
#' \code{as.data.frame(x)} or see the definition of argument \code{n}.
#'
#' Entries in columns \code{age_span} and \code{time_span} are
#' rendered as \dQuote{\verb{.}} to emphasize that these values are
#' equal over rows and columns. \emph{Note:} This is merely for
#' printing; the columns are still present with their actual numerical
#' values, i.e., \code{unique(x$time_span) == time_span(x)} is
#' \code{TRUE} and similarly for age span.
#'
#' @inheritParams base::print.data.frame
#' @inheritParams print.demog_change_component_df
#'
#' @seealso print.demog_change_component_df
#'
#' @author Mark Wheldon
#' @export
print.ccmpp_input_df <-
    function(x, ..., n = min(6L, nrow(x)), digits = NULL,
             quote = FALSE, right = TRUE, row.names = FALSE, max = NULL,
             print_what = c("info", "table")) {

        print_what <- match.arg(print_what, several.ok = TRUE)

        if ("info" %in% print_what) {
            NextMethod(generic = "print", print_what = "info")
        }

        if ("table" %in% print_what) {
            y <- as.matrix(format.data.frame(x[seq_len(n),]))
            if (is_by_age(x))
                y[-1, "age_span"] <- NA
            if (is_by_time(x))
                y[-1, "time_span"] <- NA
            if (!row.names) dimnames(y)[[1]] <- rep("", nrow(y))
        print.table(y,
                    digits = digits, quote = quote, na.print = ".",
                    right = right,
                    ...)
            cat("# ... etc.\n")
        }
        return(invisible(x))
    }

###-----------------------------------------------------------------------------
### * Subset

#' @rdname subset_demog_change_component_df
#' @export
subset_time.ccmpp_input_df <- function(x, time, drop = FALSE) {

    x <- NextMethod()
    return(ccmpp_input_df(x, dimensions = demog_change_component_dimensions(x),
                          value_type = value_type(x)))
}
