###-----------------------------------------------------------------------------
### * dimensions

get_req_attr_names_for_subclass_dfs_for_dimensions <- function(dimensions) {
    out <- get_req_attr_names()
    for (dim_w_span in get_all_dimensions_w_spans()) {
        if (dim_w_span %in% dimensions)
            out <- c(out, paste0(dim_w_span, "_span"))
    }
    return(out)
}

get_master_df_of_attr_modes_for_subclass_dfs <- function(special_subset = c("all", "extra_only")) {
    special_subset <- match.arg(special_subset, several.ok = FALSE)
    extra <- data.frame(name = c("value_scale", "age_span", "time_span",
                              "non_zero_fert_ages"),
                     mode = c("numeric", "numeric", "numeric", "numeric"),
                     NA_OK = c(TRUE, rep(FALSE, 3)))
    if (identical(special_subset, "extra_only")) return(extra)
    else return(rbind(get_master_df_of_attr_modes(), extra))
}

get_dimensions_info_for_subclass_classes <-
    function(classes = get_all_demog_change_component_df_class_names()) {
    ## NOTE: Make sure anything added here is also added to 'get_all_demog_change_component_df_class_names()'
        db <- list(pop_count_age_sex_base =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   fert_rate_age_f =
                       ensure_these_dimensions_correctly_ordered(c("time", "age")),
                   survival_ratio_age_sex =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   mort_rate_age_sex =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   death_probability_age_sex =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   death_count_age_sex =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   srb =
                       ensure_these_dimensions_correctly_ordered(c("time")),
                   mig_net_count_age_sex =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   mig_net_rate_age_sex =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   mig_net_prop_age_sex =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age")),
                   mig_net_count_tot_b =
                       ensure_these_dimensions_correctly_ordered(c("time")),
                   mig_parameter =
                       ensure_these_dimensions_correctly_ordered(c("indicator", "time")),
                   life_table_age_sex =
                       ensure_these_dimensions_correctly_ordered(
                           c("indicator", "time", "sex", "age")),
                   pop_count_age_sex =
                       ensure_these_dimensions_correctly_ordered(c("time", "sex", "age"))
                   )
        if (identical(length(classes), 1L))
            return(db[[classes]])
        else return(db[names(db) %in% classes])
}

get_req_dimensions_for_subclass_classes <- function(classes) {
    get_dimensions_info_for_subclass_classes(classes)
}

check_dimensions_for_subclass_class <- function(class, dimensions) {
    req_dims <- get_req_dimensions_for_subclass_classes(class)
    if (!setequal(dimensions, req_dims))
        stop("'", class, "' objects must have dimensions 'c(\"",
             paste(req_dims, collapse = "\", \""), "\")'. This object has dimensions 'c(\"",
             paste(dimensions, collapse = "\", \""), "\")'.")
    else
        return(invisible(dimensions))
}

check_dimensions_for_subclass_df <- function(x) {
    class_x <- oldClass(x)[1]
    dims_x <-
        check_dimensions_for_subclass_class(class = class_x,
                                           dimensions =
                                               demog_change_component_dims(x))
    req_dims <- get_req_dimensions_for_subclass_classes(class_x)
    dims_from_cols <- guess_dimensions_from_df_cols(x)
    if (!setequal(dims_x, dims_from_cols)) {
        offending_col_names <-
            get_df_col_names_for_dimensions(dimensions = dims_from_cols, spans = FALSE)
        stop("'", class_x, "' objects must have dimensions 'c(\"",
             paste(req_dims, collapse = "\", \""), "\")'. This object has columns 'c(\"",
             paste(offending_col_names, collapse = "\", \""), "\")' that correspond to dimensions 'c(\"",
             paste(dims_from_cols, collapse = "\", \""), "\")'.")
    }
    else
        return(invisible(x))
}

###-----------------------------------------------------------------------------
### * Values

## Check value type
check_value_type_of_value_in_subclass_df <- function(value) {
    if (any(is.na(value)))
        stop("'value' column has missing entries; these are not permitted in 'ccmpp_input_df' objects.")
    return(invisible())
}

###-----------------------------------------------------------------------------
### * 'value_type' attribute

get_value_type_info_for_subclass_classes <- function(class = get_all_demog_change_component_df_class_names()) {
    ## NOTE: Make sure anything added here is also added to 'get_all_demog_change_component_df_class_names()'
    db <- data.frame(rbind(c(class = "fert_rate_age_f",
                       value_type = "rate"),
                     c(class = "survival_ratio_age_sex",
                       value_type = "proportion"), #proportion!
                     c(class = "mort_rate_age_sex",
                       value_type = "rate"),
                     c(class = "death_probability_age_sex",
                       value_type = "proportion"),  #proportion!
                     c(class = "death_count_age_sex",
                       value_type = "count"),
                     c(class = "pop_count_age_sex_base",
                       value_type = "count"),
                     c(class = "srb",
                       value_type = "ratio"),
                     c(class = "mig_net_rate_age_sex",
                       value_type = "rate"),
                     c(class = "mig_net_count_age_sex",
                       value_type = "count"),
                     c(class = "mig_net_count_tot_b",
                       value_type = "count"),
                     c(class = "mig_net_prop_age_sex",
                       value_type = "ratio"), #ratio! can be negative or > 1
                     c(class = "mig_parameter",
                       value_type = "categorical"),
                     c(class = "life_table_age_sex",
                       value_type = "real"),
                     c(class = "pop_count_age_sex",
                       value_type = "count")
                     ), stringsAsFactors = FALSE)
    return(db[db$class %in% class,])
}

get_value_types_for_subclass_classes <- function(classes) {
    tb <- get_value_type_info_for_subclass_classes()
    out <- tb[tb$class %in% classes, "value_type"]
    if (!length(out)) out <- NA
    return(out)
}

###-----------------------------------------------------------------------------
### * 'value_scale' attribute

get_value_scale_annotations_info_for_subclass_classes <- function(class = get_all_demog_change_component_df_class_names()) {
    db <- data.frame(rbind(c(class = "ccmpp_input_df",
                             annotation = NA),
                           c(class = "fert_rate_age_f",
                       annotation = NA),
                     c(class = "survival_ratio_age_sex",
                       annotation = NA),
                     c(class = "mort_rate_age_sex",
                       annotation = NA),
                     c(class = "death_probability_age_sex",
                       annotation = NA),
                     c(class = "death_count_age_sex",
                       annotation = NA),
                     c(class = "pop_count_age_sex_base",
                       annotation = NA),
                     c(class = "srb",
                       annotation = NA),
                     c(class = "mig_net_rate_age_sex",
                       annotation = NA),
                     c(class = "mig_net_count_age_sex",
                       annotation = NA),
                     c(class = "mig_net_count_tot_b",
                       annotation = NA),
                     c(class = "mig_parameter",
                       annotation = NA),
                     c(class = "life_table_age_sex",
                       annotation = "radix"),
                     c(class = "pop_count_age_sex",
                       annotation = NA)
        ), stringsAsFactors = FALSE)
    return(db[db$class %in% class,])
}

get_value_scale_annotations_for_subclass_classes <- function(classes) {
    tb <- get_value_scale_annotations_info_for_subclass_classes()
    out <- tb[tb$class %in% classes, "annotation"]
    if (!length(out)) out <- NA
    return(out)
}
