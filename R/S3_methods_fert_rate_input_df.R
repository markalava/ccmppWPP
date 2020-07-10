###-----------------------------------------------------------------------------
### * Coercion

#' @rdname generic_coerce_demog_change_component_df
#' @export
as.data.frame.fert_rate_input_df <- function(x, restore_columns = FALSE, ...) {
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

#' @rdname summary_demog_change_component_df
#' @export
summary.fert_rate_input_df <-
    function(object, maxsum = 7,
             digits = max(3, getOption("digits") - 3), vsep, ...) {
        out <- NextMethod()
        out$non_zero_fert_ages <- non_zero_fert_ages(object)

        return(structure(c(out),
                         class = c("summary_fert_rate_input_df",
                                   "summary_demog_change_component_df", "list")))
    }

#' @rdname demog_change_component_df
#' @export
print.summary_fert_rate_input_df <-
    function(x, vsep, ...) {
        if (missing(vsep))
            vsep <- strrep("-", 0.75 * getOption("width"))
        NextMethod(print_what = "info")
        cat(paste0("non_zero_fert_ages:\t",
               toString(x$non_zero_fert_ages, 30),
               "\n"),
            vsep, "\n",
            sep = "")
        NextMethod(print_what = "table")
    }
