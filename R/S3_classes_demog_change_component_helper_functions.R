################################################################################
###
### Internal functions for constructing classes and methods
###
################################################################################


###-----------------------------------------------------------------------------
### * Attributes

## Define required attributes
get_req_attr_names <- function(dimensions) {
    out <- c(names(attributes(data.frame())), "dimensions", "value_type")
    if ("time" %in% dimensions)
        out <- c(out, "time_span")
    if ("age" %in% dimensions)
        out <- c(out, "age_span")
    return(out)
}

###-----------------------------------------------------------------------------
### * Manage 'class' attribute

get_all_demog_change_component_df_class_names <- function() {
    c("mig_net_count_input_df", "mig_net_count_input_df",
      "mig_net_rate_input_df", "srb_input_df", "pop_count_base_input_df",
      "survival_ratio_input_df", "fert_rate_input_df",
      "ccmpp_input_df", "demog_change_component_df")
}

strip_demog_change_component_df_classes_attribute <- function(class_att) {
    class_att[!(class_att %in% get_all_demog_change_component_df_class_names())]
}

###-----------------------------------------------------------------------------
### * Time and Age Attributes

## Attributes with corresponding '_span'
get_attr_w_span_names <- function() {
    c("time", "age")
    }

## Define allowed dimensions
get_allowed_dimensions <- function() {
    ## The name component is a label for internal indexing. The data
    ## component is the actual name of the dimension.
    ## !! ORDER MATTERS !! Determines the order of sorting!
    c("indicator" = "indicator", "time" = "time", "sex" = "sex", "age" = "age")
    }

## Data base of column names, types, and corresp. dimension.
get_dim_col_info <- function(dimensions) {
    x <- data.frame(dimension = c(get_allowed_dimensions()),
               colname = c("indicator", "time_start", "sex", "age_start"),
               type = c("character", "numeric", "character", "numeric"))
                                # rownames will have the dimension
                                # 'names' from
                                # 'get_allowed_dimensions()'
    return(x[x$dimension %in% dimensions, ])
}

## All required columns
get_all_req_col_names <- function(dimensions) {
    c(get_dim_col_info(dimensions)$colname, "value")
}

## All column types
get_all_req_col_types <- function(dimensions) {
    c(get_dim_col_info(dimensions)$type, "numeric")
}

## Get the column name in a data frame corresponding to the given
## attribute label (as in 'get_dim_col_info()')
get_attr_col_name <- function(label) {
    all_d <- get_allowed_dimensions()
    stopifnot(label %in% all_d)
    dim_col_info <- get_dim_col_info(dimensions = all_d)
    dim_col_info[label, "colname"]
}

###-----------------------------------------------------------------------------
### * Data frame checking

## Guess dimensions from data frame columns
guess_demog_change_component_dimensions <- function(x) {
    ## Attempt to guess dimensions
    col_info <-
        get_dim_col_info(dimensions = get_allowed_dimensions())
    dim_cols <- col_info$dimension[col_info$colname %in% colnames(x)]
}

## Definine the proper sort order of the class
sort_demog_change_component_df <- function(x) {
    coln_x <- colnames(x)
    coln_info_x <- get_dim_col_info(dimensions = get_allowed_dimensions())
    coln_info_x <- coln_info_x[coln_info_x$colname %in% coln_x, ]
    dims_names_x <- rownames(coln_info_x)

    get_x_col <- function(rowname) {
        x[[coln_info_x[rowname, "colname"]]]
    }

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
    coln_info_x <- get_dim_col_info(dimensions = get_allowed_dimensions())
    coln_info_x <- coln_info_x[coln_info_x$colname %in% coln_x, ]
    dims_names_x <- rownames(coln_info_x)

    get_x_col <- function(rowname) {
        x[[coln_info_x[rowname, "colname"]]]
    }

    tab_factors <- lapply(dims_names_x, "get_x_col")
    return(table(tab_factors))
}

## Get min age within each dimension
get_min_age_in_dims <- function(x) {
    stopifnot(is_by_age(x))

    coln_x <- colnames(x)
    coln_info_x <- get_dim_col_info(dimensions = get_allowed_dimensions())
    coln_info_x <- coln_info_x[coln_info_x$colname %in% coln_x, ]
    dims_names_x <- rownames(coln_info_x)

    get_x_col <- function(rowname) {
        x[[coln_info_x[rowname, "colname"]]]
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

###-----------------------------------------------------------------------------
### * sexes

get_allowed_sexes <- function() {
    c("female", "male", "both")
    }

###-----------------------------------------------------------------------------
### * 'value_type' Attribute

## Define allowed 'value_type'
get_allowed_value_types <- function() {
    c("count", "rate", "ratio", "proportion", "percentage", "real", "categorical")
}

check_value_type <- function(value, type) {

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

tabulate_value_type <- function(class) {
    data.frame(rbind(c(class = "fert_rate_input_df",
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
                       value_type = "count")))
}

get_value_type <- function(class) {
    stopifnot(class %in% get_all_demog_change_component_df_class_names())
    tb <- tabulate_value_type()
    stopifnot(class %in% tb$class)
    tb[tb$class == class, "value_type"]
    }
