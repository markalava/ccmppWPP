################################################################################
###
### Internal functions for constructing classes and methods
###
################################################################################

###-----------------------------------------------------------------------------
### * Master Lists

## Names of all classes
get_all_demog_change_component_df_class_names <- function() {
    c("life_table_input_df",
      "mig_net_count_tot_input_df", "mig_net_count_input_df",
      "mig_net_rate_input_df", "srb_input_df", "pop_count_base_input_df",
      "survival_ratio_input_df", "fert_rate_input_df",
      "ccmpp_input_df", "demog_change_component_df")
}

get_all_allowed_attributes <- function() {
    c("dimensions", "value_type", "age_span", "time_span",
      "non_zero_fert_ages")
    }

## Define allowed dimensions
get_all_allowed_dimensions <- function() {
    ## The name component is a label for internal indexing. The data
    ## component is the actual name of the dimension.
    ## !! NAME AND ELEMENT MUST BE THE SAME !!
    ## !! ORDER MATTERS !! Determines the order of sorting!
    c("indicator", "time", "sex", "age")
}

## Attributes with corresponding '_span' !!! should be 'dimensions'
get_all_dimensions_w_spans <- function() {
    c("time", "age")
}

get_master_df_of_dimensions_colnames_coltypes <- function() {
    dims <- get_all_allowed_dimensions()
    dims_w_spans <- get_all_dimensions_w_spans()
    data.frame(dimension = c(dims,
                             dims_w_spans),
               colname = c("indicator", "time_start", "sex", "age_start",
                           paste0(dims_w_spans, "_span")),
               type = c("character", "numeric", "character", "numeric",
                        "numeric", "numeric"),
               span = c(rep(FALSE, length(dims)),
                        rep(TRUE, length(dims_w_spans)))
                        )
                                # rownames will have the dimension
                                # 'names' from
                                # 'get_all_allowed_dimensions()'
}

subset_master_df_of_dimensions_colnames_coltypes <-
    function(dimensions, colnames, types, spans) {
        master_df <- get_master_df_of_dimensions_colnames_coltypes()
        idx <- rep(TRUE, nrow(master_df))
        if (!missing(dimensions)) idx <- idx & master_df[["dimension"]] %in% dimensions
        if (!missing(colnames)) idx <- idx & master_df[["colname"]] %in% colnames
        if (!missing(types)) idx <- idx & master_df[["type"]] %in% types
        if (!missing(spans)) idx <- idx & master_df[["span"]] %in% spans
        return(master_df[idx,])
    }

get_master_df_dimensions_w_span_colnames_coltypes <- function() {
    dim_names <- get_all_dimensions_w_spans()
    data.frame(dimension = get_all_dimensions_w_spans(),
               colname = paste0(dim_names, "_span"),
               type = c("numeric"))
}

###-----------------------------------------------------------------------------
### * Attributes

## Define required attributes
get_req_attr_names_for_dimensions <- function(dimensions) {
    c(names(attributes(data.frame())), "dimensions", "value_type")
}

## Manage 'class' attribute
strip_demog_change_component_df_classes_attribute <- function(class_att) {
    class_att[!(class_att %in% get_all_demog_change_component_df_class_names())]
}

###-----------------------------------------------------------------------------
### * Dimensions

## Select some dimensions but keep order correct
ensure_these_dimensions_correctly_ordered <- function(dimensions) {
    all_dims <- get_all_allowed_dimensions()
    all_dims[all_dims %in% dimensions]
}

## All required columns
get_all_req_col_names_for_dimensions <- function(dimensions) {
    c(subset_master_df_of_dimensions_colnames_coltypes(dimensions = dimensions)$colname, "value")
}

## All required columns
get_all_req_col_names_excl_spans_for_dimensions <- function(dimensions) {
    c(subset_master_df_of_dimensions_colnames_coltypes(dimensions = dimensions,
                                                       spans = FALSE)$colname, "value")
}

## All column types
get_all_req_col_types_for_dimensions <- function(dimensions) {
    c(subset_master_df_of_dimensions_colnames_coltypes(dimensions = dimensions)$type, "numeric")
}

## Get the column name in a data frame corresponding to the given
## dimensions (as in 'get_df_col_info_for_dimensions()')
get_df_col_names_for_dimensions <- function(...) {
    subset_master_df_of_dimensions_colnames_coltypes(...)$colname
}

###-----------------------------------------------------------------------------
### * Sexes

get_all_allowed_sexes <- function() {
    c("female", "male", "both")
    }

###-----------------------------------------------------------------------------
### * 'value_type' Attribute

## Define allowed 'value_type'
get_all_allowed_value_types <- function() {
    c("count", "rate", "ratio", "proportion", "percentage", "real", "categorical")
}

get_value_type_info_for_classes <- function(class = get_all_demog_change_component_df_class_names()) {
    db <- data.frame(rbind(c(class = "fert_rate_input_df",
                       value_type = "rate"),
                     c(class = "survival_ratio_input_df",
                       value_type = "proportion"),
                     c(class = "pop_count_base_input_df",
                       value_type = "count"),
                     c(class = "srb_input_df",
                       value_type = "ratio"),
                     c(class = "mig_net_rate_input_df",
                       value_type = "rate"),
                     c(class = "mig_net_count_input_df",
                       value_type = "count"),
                     c(class = "mig_net_count_tot_input_df",
                       value_type = "count"),
                     c(class = "life_table_input_df",
                       value_type = "real")
                     ), stringsAsFactors = FALSE)
    return(db[db$class %in% class,])
}

get_value_types_for_classes <- function(classes) {
    tb <- get_value_type_info_for_classes()
    tb[tb$class %in% classes, "value_type"]
    }


###-----------------------------------------------------------------------------
### * Data frame checking

## Guess dimensions from data frame columns
guess_dimensions_from_df_cols <- function(x) {
    ## Attempt to guess dimensions
    col_info <-
        subset_master_df_of_dimensions_colnames_coltypes(spans = FALSE)
    dimensions <- col_info$dimension[col_info$colname %in% colnames(x)]
    return(dimensions)
}

## Definine the proper sort order of the class
sort_demog_change_component_df <- function(x) {
    coln_x <- colnames(x)
    coln_info_x <- subset_master_df_of_dimensions_colnames_coltypes(spans = FALSE)
    coln_info_x <- coln_info_x[coln_info_x$colname %in% coln_x, ]
    dims_names_x <- coln_info_x$dimension

    get_x_col <- function(dimension) {
        x[[coln_info_x[coln_info_x$dimension == dimension, "colname"]]]
    }

    dims_names_not_age <- dims_names_x[!dims_names_x == "age"]

    sort_factors <-
        unname(as.data.frame(lapply(dims_names_x, "get_x_col")))

    sex_col <- which(dims_names_x == "sex")
    if (length(sex_col) > 0) {
        sort_factors[, sex_col] <- rev(sort_factors[, sex_col])
    }

    return(x[do.call("order", sort_factors), ])
}

## Tabulate to check squareness
tabulate_demog_change_component_df <- function(x) {
    coln_x <- colnames(x)
    coln_info_x <- subset_master_df_of_dimensions_colnames_coltypes(spans = FALSE)
    coln_info_x <- coln_info_x[coln_info_x$colname %in% coln_x, ]
    dims_names_x <- coln_info_x$dimension

    get_x_col <- function(dimension) {
        x[[coln_info_x[coln_info_x$dimension == dimension, "colname"]]]
    }

    dims_names_not_age <- dims_names_x[!dims_names_x == "age"]

    tab_factors <- lapply(dims_names_x, "get_x_col")
    return(table(tab_factors))
}

## Get min age within each dimension
get_min_age_in_dims_in_df <- function(x) {
    stopifnot(is_by_age(x))

    coln_x <- colnames(x)
    coln_info_x <- subset_master_df_of_dimensions_colnames_coltypes(spans = FALSE)
    coln_info_x <- coln_info_x[coln_info_x$colname %in% coln_x, ]
    dims_names_x <- coln_info_x$dimension

    get_x_col <- function(dimension) {
        x[[coln_info_x[coln_info_x$dimension == dimension, "colname"]]]
    }

    dims_names_not_age <- dims_names_x[!dims_names_x == "age"]

    if (length(dims_names_not_age) > 1) {

        tab_factors <-
            lapply(dims_names_not_age, "get_x_col")

        return(tapply(get_x_col("age"), INDEX = tab_factors,
                      FUN = "min"))

    } else if ("age" %in% dims_names_x)
        return(min(get_x_col("age")))
}

check_value_type_of_value_in_df <- function(value, type) {

    stop_msg <- function(suff) {
        paste0("'value_type' is '", type, "' but ", suff)
    }

    ## Character types
    if (identical(type, "categorical")) {
        if (!is.character(value))
            stop(stop_msg("values are not character."))
    } else {
        ## Numeric types
        if (!all(is.finite(value)))
            stop("Not all 'value's are finite and non-missing.")

        if (type %in% c("rate", "ratio", "real", "count"))
            return(invisible(value))

        if (identical(type, "proportion")) {
            if (any(value < 0 | value > 1))
                stop(stop_msg("values less than 0 or greater than 1 are present."))
        } else if (identical(type, "percentage")) {
            if (any(value < 0 | value > 100))
                stop(stop_msg("values less than 0 or greater than 100 are present."))
        } else {
            return(invisible(value))
        }
    }
}
