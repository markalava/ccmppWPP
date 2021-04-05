
not_a_valid_object_msg <- function(class, ...) {
    paste0("Not a valid '", class, "' object:\n", ..., "\n")
}

## Unusual values
get_mig_net_prop_value_warning_threshold <- function() {
    0.5
}

## Error checking: Report on near equality of sexes
#' @noRd
sexes_unequal <- function(x, x_name = "x", tolerance = 1e-6, scale = NULL,
                          check_by_time = FALSE #<-- with 'FALSE'
                                                #almost doubles time
                                                #for validation; with
                                                #'TRUE' quadruples it
                          ) {
    if (is_by_indicator(x)) {
        indicators_x <- indicators(x)
        msg <- lapply(setNames(indicators_x, indicators_x),
                      FUN = function(this_indicator) {
            y <- subset_indicator.demog_change_component_df(x, indicators = this_indicator,
                                                            include = TRUE, drop = TRUE) #'drop' so that loop over time activates
            sexes_unequal(y, x_name = x_name, tolerance = tolerance, scale = scale)
        })
    } else if (is_by_time(x) && check_by_time) {
        times_x <- times(x)
        msg <- lapply(setNames(times_x, times_x),
                      FUN = function(this_time) {
            ## save time..subset manually
            ## y <- subset_time.demog_change_component_df(x, times = this_time,
            ##                  include = TRUE, drop = TRUE)
            y <- x[x$time_start == this_time,
                   colnames(x)[!colnames(x) %in% get_df_col_names_for_dimensions(dimensions = "time")]]
            sexes_unequal(y, x_name = x_name, tolerance = tolerance, scale = scale)
        })
    } else {
        target <- x[x$sex == "female", "value"]
        current <- x[x$sex == "male", "value"]
        test <- all.equal(target = target, current = current, scale = scale, tolerance = tolerance,
                          countEQ = TRUE)
        if (!isTRUE(test)) return(TRUE)
        else {
            ## If equal, want to know what comparison 'all.equal' actually did.
            ## From 'all.equal.numeric'..
            out <- is.na(target)
            out <- out | target == current
            if (all(out))
                return(paste0("Female and male ",
                              x_name,
                              " are identical."))
            else {
                if (!is.null(scale)) what <- "absolute"
                else {
                    N <- length(out)
                    sabst0 <- sum(abs(target[out]))
                    target <- target[!out]
                    current <- current[!out]
                    if (is.integer(target) && is.integer(current))
                        target <- as.double(target)
                    xy <- sum(abs(target - current))/N
                    what <- {
                        xn <- (sabst0 + sum(abs(target)))/N
                        if (is.finite(xn) && xn > tolerance) {
                            xy <- xy/xn
                            "relative"
                        }
                        else "absolute"
                    }
                }
            }
            return(paste0("Female and male ",
                          x_name,
                          " are very similar; mean ",
                          what,
                          " difference less than ",
                          tolerance, "."))
        }
    }
    if (is.list(msg)) {
        if (suppressWarnings(isTRUE(all(unlist(msg))))) return(TRUE)
        else {
            ## Discard the 'TRUE' elements
            msg <- rapply(msg, function(z) if (isTRUE(z)) { NULL } else { z })
            ## Format: summarize duplicates
            list_to_txt <- function(w) {
                w <- w[!is.na(w)]
                fmtd_txt <- character(0)
                for (uw in unique(w)) {
                    fmtd_txt <- paste0(fmtd_txt,
                                       toString(names(w == uw), width = getOption("width")),
                                       "\n",
                                       "    ",
                                       uw)
                }
                return(paste0(fmtd_txt, "\n"))
            }
            return(list_to_txt(msg))
        }
    } else return(msg) #not a list.. not sure what would trigger this but cover the possibility
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

        span_col_names <- paste0(dim_names_x_span_dims_only, "_span")
        start_col_names <- paste0(dim_names_x_span_dims_only, "_start")
        ## check:
        if (!identical(length(span_col_names), length(start_col_names)))
            stop("Must have '_span' cols for all '_start' cols and vice versa.")

        ## if all spans the same then don't need to expand out the Lexis squares
        if (identical(length(unique(unlist(x[, span_col_names]))), 1L)) {
            return(table(x[, colnames_x_no_spans]))

        } else {

            ## Expand using '_spans' ----

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

            ## This is *slow*... needs speeding up! plyr::ddplyr shaves
            ## off about 8% user time relative to base::by. Would using
            ## '.parallel = TRUE' help?

            ## x <- by(x, x[, colnames_x_no_spans], function(z) {
            x <- plyr::ddply(x, colnames_x_no_spans, function(z) {
                if (all(z[, span_col_names] == min_span)) {
                    ## Don't need to expand anything
                    return(z[, colnames_x_no_spans])
                } else {
                    ## Will need to expand using the spans.

                    ## If there are multiple rows here it will
                    ## end up being invalid so check.
                    out_df <- data.frame()
                    for (i in seq_len(nrow(z))) {
                        ## lapply over the dims with spans
                        grid_list <-
                            lapply(setNames(seq_along(start_col_names), start_col_names),
                                   function(j) {
                                out <- vector(mode = "numeric")
                                seq(from = z[i, start_col_names[j]],
                                    length.out = z[i, span_col_names[j]],
                                    by = min_span)
                            })
                        grid_list2 <- list()
                        for (dn in setdiff(dims_names_x, dim_names_x_span_dims_only)) {
                            grid_list2 <- c(grid_list2,
                                            setNames(list(unique(z[i, dn])), dn))
                        }
                        grid_list <- c(grid_list, grid_list2)
                        out_df <- rbind(out_df, expand.grid(grid_list))
                    }
                }
                return(out_df)
            })
            ##x <- do.call(rbind, x)
                         # ^ goes with the base::by version

            ## Tabulate ----

            return(table(x))
            }
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
