

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
### * Time and Age Attributes

## Attributes with corresponding '_span'
get_attr_w_span_names <- function() {
    c("time", "age")
    }

## Define allowed dimensions
get_allowed_dimensions <- function() {
    ## The name component is a label for internal indexing. The data
    ## component is the actual name of the dimension.
    c("time" = "time", "age" = "age", "sex" = "sex")
    }

## Data base of column names, types, and corresp. dimension.
get_dim_col_info <- function(dimensions) {
    x <- data.frame(dimension = c(get_allowed_dimensions()),
               colname = c("time_start", "age_start", "sex"),
               type = c("numeric", "numeric", "character"))
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

## Definine the proper sort order of the class
sort_demog_change_component_df <- function(x) {
    coln_x <- colnames(x)
    coln_info_x <- get_dim_col_info(dimensions = get_allowed_dimensions())
    coln_info_x <- coln_info_x[coln_info_x$colname %in% coln_x, ]
    dims_names_x <- rownames(coln_info_x)

    get_x_col <- function(rowname) {
        x[[coln_info_x[rowname, "colname"]]]
    }

    if (all(c("time", "sex", "age") %in% dims_names_x))
        return(x[order(get_x_col("time"),
                       rev(get_x_col("sex")),
                       get_x_col("age")
                       ), ])
    else if (all(c("time", "sex") %in% dims_names_x))
        return(x[order(get_x_col("time"),
                       rev(get_x_col("sex"))
                           ), ])
    else if (all(c("time", "age") %in% dims_names_x))
        return(x[order(get_x_col("time"),
                       get_x_col("age")
                       ), ])
    else if (all(c("sex", "age") %in% dims_names_x))
        return(x[order(rev(get_x_col("sex")),
                       get_x_col("age")
                       ), ])
    else if ("time" %in% dims_names_x)
        return(x[order(get_x_col("time")
                       ), ])
    else if ("age" %in% dims_names_x)
        return(x[order(get_x_col("age")
                       ), ])
    else if ("sex" %in% dims_names_x)
        return(x[order(get_x_col("sex")
                       ), ])
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

    if (all(c("time", "sex", "age") %in% dims_names_x))
        return(table(get_x_col("time"),
                     get_x_col("sex"),
                     get_x_col("age")))
    else if (all(c("time", "sex") %in% dims_names_x))
        return(table(get_x_col("time"),
                     get_x_col("sex")))
    else if (all(c("time", "age") %in% dims_names_x))
        return(table(get_x_col("time"),
                     get_x_col("age")))
    else if (all(c("sex", "age") %in% dims_names_x))
        return(table(get_x_col("sex"),
                     get_x_col("age")))
    else if ("time" %in% dims_names_x)
        return(table(get_x_col("time")))
    else if ("age" %in% dims_names_x)
        return(table(get_x_col("age")))
    else if ("sex" %in% dims_names_x)
        return(table(get_x_col("sex")))
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

    if (all(c("time", "sex", "age") %in% dims_names_x))
        return(tapply(get_x_col("age"),
                      INDEX = list(get_x_col("sex"),
                                   get_x_col("time")),
                      FUN = "min"))
    else if (all(c("time", "age") %in% dims_names_x))
        return(tapply(get_x_col("age"),
                      INDEX = list(get_x_col("time")),
                      FUN = "min"))
    else if (all(c("sex", "age") %in% dims_names_x))
        return(tapply(get_x_col("age"),
                      INDEX = list(get_x_col("sex")),
                      FUN = "min"))
    else if ("age" %in% dims_names_x)
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
    c("count", "rate", "ratio", "proportion", "percentage", "real")
}

check_value_type <- function(value, type) {

    if (!all(is.finite(value)))
        stop("Not all 'value's are finite and non-missing.")

    stop_msg <- function(suff) {
        paste0("'value_type' is '", type, "' but ", suff)
    }

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


