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
#' @inheritParams base::print.table
#' @inheritParams base::print.default
#'
#' @seealso print.demog_change_component_df
#'
#' @author Mark Wheldon
#' @export
print.ccmpp_input_df <-
    function(x, ..., n = min(6L, nrow(x)), digits = NULL,
             quote = FALSE, right = TRUE, row.names = FALSE,
             na.print = ".",
             print_what = c("info", "table")) {

        print_what <- match.arg(print_what, several.ok = TRUE)

        if ("info" %in% print_what) {
            NextMethod(generic = "print", print_what = "info")
        }

        if ("table" %in% print_what) {
            y <- x[seq_len(n),]
            if (is_by_age(x))
                y[-1, "age_span"] <- na.print
            if (is_by_time(x))
                y[-1, "time_span"] <- na.print
            print(y,
                  digits = digits, quote = quote, row.names = row.names,
                  na.print = na.print,
                  right = right,
                  ...)
            cat("# ... etc.\n")
        }
        return(invisible(x))
    }
