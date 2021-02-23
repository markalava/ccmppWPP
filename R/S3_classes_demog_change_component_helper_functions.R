################################################################################
###
### Internal functions for constructing classes and methods
###
################################################################################

###-----------------------------------------------------------------------------
### * Master Lists

## Names of all classes
get_all_demog_change_component_df_class_names <- function() {
    c("life_table_age_sex", "mig_parameter",
      "mig_net_count_tot_b", "mig_net_count_age_sex",
      "mig_net_rate_age_sex", "mig_net_prop_age_sex",
      "srb", "pop_count_age_sex_base",
      "survival_ratio_age_sex",
      "mortality_rate_age_sex",
      "death_probability_age_sex",
      "death_count_age_sex",
      "fert_rate_age_f",
      "ccmpp_input_df",
      "pop_count_age_sex",
      "ccmpp_output_df",
      "demog_change_component_df")
}

get_all_allowed_attributes <- function() {
    c("dimensions", "value_type", "age_span", "time_span",
      "non_zero_fert_ages", "value_scale")
    }

## Define allowed dimensions
get_all_allowed_dimensions <- function() {
    ## !! ORDER MATTERS !! Determines the order of sorting!
    c("indicator", "time", "sex", "age")
}

## Attributes with corresponding '_span' !!! should be 'dimensions'
get_all_dimensions_w_spans <- function() {
    c("time", "age")
}

## NOTE: See subsequent functions for convenient subsets of this
## master list.
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

get_as_function_for_class <- function(class) {
    if (class %in% get_all_demog_change_component_df_class_names())
        return(paste("as", class, sep = "_"))
    else if (identical(class, "data.frame"))
        return("as.data.frame")
    else stop("Don't know what the 'as' function is for class '", class, "'.")
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

## All required columns (including 'value'). Don't want 'value'? See 'get_df_col_names_for_dimensions()' below.
get_all_req_col_names_for_dimensions <- function(dimensions) {
    c(subset_master_df_of_dimensions_colnames_coltypes(dimensions = dimensions)$colname, "value")
}

## All required columns except spans (including 'value'). Don't want 'value'? See 'get_df_col_names_for_dimensions()' below.
get_all_req_col_names_excl_spans_for_dimensions <- function(dimensions) {
    c(subset_master_df_of_dimensions_colnames_coltypes(dimensions = dimensions,
                                                       spans = FALSE)$colname, "value")
}

## All column types
get_all_req_col_types_for_dimensions <- function(dimensions) {
    c(subset_master_df_of_dimensions_colnames_coltypes(dimensions = dimensions)$type, "any")
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

## List value types that can be aggregated or abridged
get_all_aggregatable_value_types <- function() {
    c("count", "real")
    }

###-----------------------------------------------------------------------------
### * 'value_scale' Attribute

## Value types that can have non-NA value_scales
get_value_types_w_non_NA_value_scale <- function() {
    c("rate", "real", "count")
}

get_value_scale_prefixes_info_for_value_types <-
    function(value_type = get_value_types_w_non_NA_value_scale()) {
    db <- data.frame(rbind(c(value_type = "count",
                       prefix = NA),
                     c(value_type = "rate",
                       prefix = "per"),
                     c(value_type = "real",
                       prefix = NA)
                     ), stringsAsFactors = FALSE)
    return(db[db$value_type %in% value_type,])
}

get_value_scale_prefixes_for_value_types <- function(value_types) {
    tb <- get_value_scale_prefixes_info_for_value_types()
    out <- tb[tb$value_type %in% value_types, "prefix"]
    if (!length(out)) out <- NA
    return(out)
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

    sort_factors <-
        unname(as.data.frame(lapply(dims_names_x, "get_x_col")))

    sex_col <- which(dims_names_x == "sex")
    if (length(sex_col)) {
        sort_factors[, sex_col] <-
            factor(sort_factors[, sex_col],
                   levels = c("male", "female"),
                   ordered = TRUE)
    }

    return(x[do.call("order", sort_factors), ])
}


## Tabulate Lexis squares.
tabulate_lexis_squares <- function(x) {
    colnames_x <- colnames(x)
    coln_info_x <- subset_master_df_of_dimensions_colnames_coltypes(spans = FALSE)
    coln_info_x <- coln_info_x[coln_info_x$colname %in% colnames_x, , drop = FALSE]
    dims_names_x <- coln_info_x$dimension
    colnames_x_no_spans <- coln_info_x$colname
    colnames_x_no_span_dims <-
        coln_info_x[!coln_info_x$dimension %in% get_all_dimensions_w_spans(),
                    "colname"]
    dim_names_x_span_dims_only <-
            dims_names_x[dims_names_x %in% get_all_dimensions_w_spans()]

    ## If no age or time then just tabulate sex * indicator
    if (!length(dim_names_x_span_dims_only)) {
        return(table(x[, colnames_x_no_spans]))
    } else {

        ## Expand using '_spans' ----

        span_col_names <- paste0(dim_names_x_span_dims_only, "_span")
        start_col_names <- paste0(dim_names_x_span_dims_only, "_start")

        if (!identical(length(span_col_names), length(start_col_names)))
            stop("Must have '_span' cols for all '_start' cols and vice versa.")

browser()

        min_span <- min(x[, span_col_names])

        if ("age" %in% dims_names_x) {
            x$age_span[x$age_span == 1000] <- 1
                                # ^ TEMP as along as '1000' used to mark
                                # open ended age group
            x$age_span <- x$age_span / min_span # scale in case min span != 1
        }
        if ("time" %in% dims_names_x) {
            x$time_span <- x$time_span / min_span
        }

        non_min_span <-
            sort(which(x[, span_col_names] != min_span, arr.ind = TRUE)[, 1])

        x_min_span <- x[-non_min_span, ]

        ## test <- by(x[non_min_span, ],
        ##            x[non_min_span, colnames_x_no_spans],
        ##            function(z) z){
        ##     grid_list <- list()
        ##     if ("age_start" %in% colnames_x_no_spans)
        ##         grid_list <- c(grid_list,
        ##                        list(age_start = seq(from = z[,"age_start"],
        ##                                             length.out = z[,"age_span"],
        ##                                             by = min_span)))
        ##     if ("time_start" %in% colnames_x_no_spans)
        ##         grid_list <- c(grid_list,
        ##                        list(time_start = seq(from = z[,"time_start"],
        ##                                              length.out = z[,"time_span"],
        ##                                              by = min_span)))
        ##     if ("sex" %in% colnames_x_no_spans)
        ##         grid_list <- c(grid_list, list(sex = unique(z[, "sex"])))
        ##     if ("indicator" %in% colnames_x_no_spans)
        ##         grid_list <- c(grid_list, list(indicator = unique(z[, "indicator"])))
        ##     expand.grid(grid_list)
        ## })




        ## grid_list <-
        ##     lapply(setNames(seq_along(start_col_names), start_col_names),
        ##            function(i) {
        ##         l <- Map(function(a, b) seq(from = a, length.out = b, by = min_span),
        ##                  a = x[, start_col_names[i]], b = x[, span_col_names[i]])
        ##         return(unlist(l))
        ##     })

        ## non_span_dims <- setdiff(dims_names_x, dim_names_x_span_dims_only)
        ## if (length(non_span_dims)) {
        ##     non_span_dims_colnames <- get_df_col_names_for_dimensions(non_span_dims)
        ##     grid_list <- c(grid_list,
        ##                    lapply(setNames(non_span_dims_colnames, non_span_dims_colnames),
        ##                           function(z) unique(x[, z])))


## y <- x[x$age_span != min_span || x$time_span != min_span, ]

##     y <- plyr::ddply(x, coln_x_no_spans, "transform",
##                      function(z) {




        x <- split(x, x[, colnames_x_no_spans])
        x <- lapply(x, function(z) {
            if (all(z[, span_col_names] == min_span)) return(z[, colnames_x_no_spans])
            else {
                grid_list <- list()
                ## 'age', 'time'
                for (j in seq_along(span_col_names)) {
                    for (i in seq_len(nrow(z))) {
                        grid_list <- c(grid_list,
                                       setNames(list(seq(from = z[i, start_col_names[j]],
                                                         length.out = z[i, span_col_names[j]],
                                                         by = min_span)),
                                                start_col_names[j]))
                    }
                }
                ## if ("age_start" %in% colnames_x_no_spans)
                ##     if (
                ##     grid_list <- c(grid_list,
                ##                    list(age_start = seq(from = z[,"age_start"],
                ##                                         length.out = z[,"age_span"],
                ##                                         by = min_span)))
                ## if ("time_start" %in% colnames_x_no_spans)
                ##     grid_list <- c(grid_list,
                ##                    list(time_start = seq(from = z[,"time_start"],
                ##                                         length.out = z[,"time_span"],
                ##                                         by = min_span)))
                for (dn in setdiff(dims_names_x, dim_names_x_span_dims_only)) {
                    grid_list <- c(grid_list,
                                   setNames(list(unique(z[, dn])), dn))
                }

        ## if ("sex" %in% colnames_x_no_spans)
        ##     grid_list <- c(grid_list, list(sex = unique(z[, "sex"])))
        ## if ("indicator" %in% colnames_x_no_spans)
        ##     grid_list <- c(grid_list, list(indicator = unique(z[, "indicator"])))
        return(expand.grid(grid_list))
        }
    })
    x <- do.call(rbind, x)

    ## Tabulate ----

        return(table(x))
        }
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

## Check value type
check_value_type_of_value_in_df <- function(value, type) {

    is_na <- is.na(value)
    if (all(is_na)) {
        S3_class_warning("All 'value' entries are 'NA'.")
        return(invisible())
    }
    if (any(is_na)) {
        S3_class_warning("'value' column has some 'NA' entries.")
        value <- value[!is_na]
    }

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
            return(invisible())

        if (identical(type, "proportion")) {
            if (any(value < 0 | value > 1))
                stop(stop_msg("values less than 0 or greater than 1 are present."))
        } else if (identical(type, "percentage")) {
            if (any(value < 0 | value > 100))
                stop(stop_msg("values less than 0 or greater than 100 are present."))
        } else {
            return(invisible())
        }
    }
}
