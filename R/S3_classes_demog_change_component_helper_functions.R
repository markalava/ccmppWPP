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
      "mort_rate_age_sex",
      "death_probability_age_sex",
      "death_count_age_sex",
      "fert_rate_age_f",
      "ccmpp_input_df",
      "pop_count_age_sex",
      "ccmpp_output_df",
      "pop_count_age_sex_reference",
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

## Dimensions with corresponding '_span'
get_all_dimensions_w_spans <- function() {
    c("time", "age")
}

## Classes with time_span == 0
get_all_classes_time_span_zero <- function() {
    classes <- c("pop_count_age_sex_base", "pop_count_age_sex")
    stopifnot(all(classes %in% get_all_demog_change_component_df_class_names()))
    return(classes)
}

has_time_span_zero <- function(x) {
    has <- any(unlist(sapply(get_all_classes_time_span_zero(),
                             function(z) inherits(x, z))))
    if (has) stopifnot(is_by_time(x))
    return(has)
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
get_req_attr_names <- function() {
    c(names(attributes(data.frame())), "dimensions", "value_type")
}

get_master_df_of_attr_modes <- function() {
    data.frame(name = c(names(attributes(data.frame())),
                        "dimensions", "value_type"),
               mode = c(sapply(attributes(data.frame()),
                                 "mode", USE.NAMES = FALSE),
                        "character", "character"),
               NA_OK = FALSE)
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

## Get the column names in a data frame corresponding to the given
## dimensions. Includes span but excludes value.
get_df_col_names_for_dimensions <- function(...) {
    subset_master_df_of_dimensions_colnames_coltypes(...)$colname
}

###-----------------------------------------------------------------------------
### ** Sexes, including ordering

## Define the labels for sex groupings and their order.
## Provide converters among numeric, character, and factors.
##
## Use DemoData mapping:
## '1' = 'male'
## '2' = 'female'
## '3' = 'both'

## sexes
get_all_allowed_sexes <- function() {
    ## *Do not change the order*
    c("male", "female", "both")
}

## Sex as factor
sex_as_factor <- function(x) {
    allowed_sexes_in_order <- get_all_allowed_sexes()
    if (is.factor(x)) x <- as.numeric(x)
    if (is.numeric(x)) {
        if (!all(unique(x) %in% c(1, 2, 3)))
            stop("sex must only take values '1', '2', or '3', which will be mapped to sexes ",
                 toString(allowed_sexes_in_order),
                 "respectively.")
        return(droplevels(factor(x, levels = c(1, 2, 3),
                      labels = allowed_sexes_in_order, ordered = TRUE)))
    } else if (is.character(x)) {
        if (!all(unique(x) %in% allowed_sexes_in_order))
            stop("sex must only take values ", toString(allowed_sexes_in_order))
        return(droplevels(factor(x, levels = allowed_sexes_in_order,
                      labels = allowed_sexes_in_order, ordered = TRUE)[, drop = TRUE]))
    } else stop("sex must be numeric, character, or factor. 'x' has type '", typeof(x), "'.")
}

## Return allowed sexes as an ordered factor
sex_as_character <- function(x) {
    x <- sex_as_factor(x)
    return(levels(x)[x])
}

## Sex as numeric index. This uses the ordering defined in
## 'get_all_allowed_sexes'
sex_as_numeric <- function(x) {
    as.integer(sex_as_factor(x))
}

###-----------------------------------------------------------------------------
### ** Spans

## Guess spans.

#' Guess the \dQuote{span} for a demographic dimension
#'
#' \code{guess_span_from_start} takes a
#' vector of \dQuote{\code{_start}} values as first argument;
#' \code{guess_span_for_dimension_for_df} is a convenience wrapper
#' that takes a data frame with such a column instead.
#'
#' Certain demographic dimensions, such as \dQuote{time} and
#' \dQuote{age} have associated spans. These should ordinarily be
#' supplied by the user when calling, e.g.,
#' \code{\link{demog_change_component_df}}, but a very simple guess
#' will be attempted if not. Generally, the smallest difference
#' between successive values in the corresponding
#' \dQuote{\code{_start}} column will be returned. However, if there
#' is only one unique \dQuote{\code{_start}} value, all \code{1} will
#' be returned and a warning issued.
#'
#' @param x Vector of \dQuote{start} values (e.g., the
#'     \code{time_start} or \code{age_start} column from a
#'     \code{\link{demog_change_component_df}} object).
#' @param dimension A single demographic dimension with a
#'     \dQuote{span} (e.g., \dQuote{age}, \dQuote{time}).
#' @return Guessed span.
#' @author Mark Wheldon
#' @name guess_span_for_dimension
guess_span_from_start <- function(x, span_name = character()) {
    unique_x <- unique(as.numeric(x))
    if (identical(length(unique_x), 1L)) {
        S3_class_warning("Cannot determine ", span_name, " span; setting it to '1'.")
        return(1)
    } else {
        min(diff(unique_x, differences = 1))
    }
}

#' @rdname guess_span_for_dimension
guess_span_for_dimension_for_df <- function(x, dimension = get_all_dimensions_w_spans()) {
    dimension <- match.arg(dimension, several.ok = FALSE)
    start_col_name <- grep("_start",
                           get_df_col_names_for_dimensions(dimension),
                           value = TRUE)
    guess_span_from_start(x = x[, start_col_name])
}

get_is_by_function_for_dimension <- function(dimension) {
    if (dimension %in% get_all_allowed_dimensions())
        return(paste("is_by", dimension, sep = "_"))
    else stop("Don't know what the 'is' function is for dimension '", dimension, "'.")
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

    ## The underlying CCMPP projection functions map '1' to 'male' and
    ## '2' to 'female'. This *must* be preserved throughout to defend
    ## against mis-ordering.
    sex_col <- which(dims_names_x == "sex")
    if (length(sex_col)) {
        sort_factors[, sex_col] <-
            sex_as_factor(sort_factors[, sex_col])
    }

    return(x[do.call("order", sort_factors), ])
}
