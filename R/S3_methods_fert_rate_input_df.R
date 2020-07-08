###-----------------------------------------------------------------------------
### * Coercion

#' @rdname generic_coerce_demog_change_component_df
#' @export
as.data.frame.fert_rate_input_df <- function(x, restore_columns = TRUE, ...) {
    if (restore_columns) {
        nzfa <- non_zero_fert_ages(x)
        x <- NextMethod()
        x[["non_zero_fert_age"]] <-
            x[[get_df_col_namees_for_dimensions("age")]] %in% nzfa
        return(x)
    } else return(NextMethod())
}
