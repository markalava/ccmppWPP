###-----------------------------------------------------------------------------
### * Coercion

#' @rdname generic_coerce_demog_change_component_df
#' @export
as.data.frame.fert_rate_age_f <- function(x, restore_columns = FALSE, ...) {
    if (restore_columns) {
        nzfa <- non_zero_fert_ages(x)
        x <- NextMethod()
        x[["non_zero_fert_age"]] <-
            x[[get_df_col_names_for_dimensions(dimensions = "age", spans = FALSE)]] %in% nzfa
        return(x)
    } else return(NextMethod())
}

###-----------------------------------------------------------------------------
### * Print, Summary, etc.


#' Print Values of a \code{fert_rate_age_f}
#'
#' This is a method for the generic \code{\link{base::print}} function. Only
#' \code{n} rows are printed for convenience. If all rows are desired use
#' \code{as.data.frame(x)} or see the definition of argument \code{n}.
#'
#' As well as inheriting the behaviour of
#' \code{\link{print.ccmpp_input_df}}, entries in the \code{value}
#' column corresponding to values of \code{age_start} \emph{not} in
#' \code{non_zero_fert_ages(x)} are rendered as
#' \dQuote{\verb{[zero]}}. At least some rows with non-zero fertility
#' rate ages will be shown if possible.
#'
#' @inheritParams base::print.data.frame
#' @inheritParams print.demog_change_component_df
#'
#' @seealso print.demog_change_component_df print.ccmpp_input_df fert_rate_age_f
#'
#' @author Mark Wheldon
#' @export
print.fert_rate_age_f <-
    function(x, ..., n = min(6L, nrow(x)), digits = NULL,
             quote = FALSE, right = TRUE, row.names = FALSE,
             na.print = ".",
             print_what = c("info", "table")) {

        print_what = c("info", "table")

        if ("info" %in% print_what) {
            NextMethod(generic = "print", print_what = "info")
        }

        if ("table" %in% print_what) {
            nzfa <- non_zero_fert_ages(x)
            if (is_by_age(x))
                x[-1, "age_span"] <- NA
            if (is_by_time(x))
                x[-1, "time_span"] <- NA

            if (is_by_age(x) && !is.null(nzfa)) {
                nzf_rows <- x$age_start %in% nzfa
                x[!nzf_rows, "value"] <- "[zero]"
                if (sum(nzf_rows) < nrow(x) && n < nrow(x) && sum((!nzf_rows[n])) > 0 && n > 5) {
                    n1 <- floor(n/2)
                    n2 <- n - n1
                    a <- match(TRUE, nzf_rows)
                    b <- a + n2 - 1
                    rows_to_print <- unique(c(1:(n1 + 1), a:b))
                    if (length(rows_to_print) < n) {
                        rows_to_print <- seq_len(n)
                    } else {
                        x[n1 + 1, ] <- NA
                        rownames(x)[n1 + 1] <- ""
                    }
                } else {
                    rows_to_print <- seq_len(n)
                }
                y <- x[rows_to_print, ]
                y[is.na(y)] <- na.print
                print(y,
                      digits = digits, quote = quote, na.print = ".",
                      right = right, row.names = row.names,
                      ...)
                cat("# ... etc.\n")
            }
        }
        return(invisible(x))
    }

#' @rdname summary_demog_change_component_df
#' @export
summary.fert_rate_age_f <-
    function(object, maxsum = 7,
             digits = max(3, getOption("digits") - 3), vsep, ...) {
        out <- NextMethod()
        out$non_zero_fert_ages <- non_zero_fert_ages(object)

        return(structure(c(out),
                         class = c("summary_fert_rate_age_f",
                                   "summary_demog_change_component_df", "list")))
    }

#' @rdname demog_change_component_df
#' @export
print.summary_fert_rate_age_f <-
    function(x, vsep, ..., print_what = c("info", "table")) {
        print_what <- match.arg(print_what, several.ok = TRUE)
        if (missing(vsep))
            vsep <- strrep("-", 0.75 * getOption("width"))
        if ("info" %in% print_what) {
            NextMethod(print_what = "info")
            cat(paste0("non_zero_fert_ages:\t",
                       print_non_zero_fert_ages(x$non_zero_fert_ages, 30),
                       "\n"),
                vsep, "\n",
                sep = "")
        }
        if ("table" %in% print_what)
            NextMethod(print_what = "table")
    }
