
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


## Check that '_span' columns are consistent with first differences of
## corresponding '_start' columns.
verify_spans_equal_start_differences <- function(x, obj_class) {
    demog_change_component_dims_x <- demog_change_component_dims(x)
    attr_w_span_names <- get_all_dimensions_w_spans()
    attr_w_span_names <-
        attr_w_span_names[attr_w_span_names %in% demog_change_component_dims_x]

    ## If time_span is zero remove it from 'attr_w_span_names': This
    ## span _not_ be checked for consistency with first
    ## differences of time_start.
    if (has_time_span_zero(x))
        attr_w_span_names <- attr_w_span_names[!attr_w_span_names %in% "time"]

    for (att in attr_w_span_names) {

        ## Create names of the '_span' and '_start' variables for
        ## use later.
        span_name <- paste0(att, "_span")
        start_name <- paste0(att, "_start")

        ## Get the values of the attribute and column from x for
        ## use later.
        span_attr <- attr(x, span_name)
        start_col <- x[[start_name]]

        ## ## Check length of attribute
        ## if (!identical(length(span_attr), 1L))
        ##     stop(not_a_valid_object_msg("ccmpp_input_df",
        ##                                 "'", span_name, "' is not of length 1."))

        ## Spans must be consistent with the differences between the
        ## '_start' column values.

        ## Any 'sex' or 'indicator' cols?
        by_col_names <- sapply(demog_change_component_dims_x,
                               FUN = "get_df_col_names_for_dimensions", spans = FALSE)
        by_col_names <- by_col_names[!by_col_names %in% start_name]
        if (length(by_col_names)) {
            ## E.g., have to do it by 'sex' or by 'indicator'
            start_vs_span_diff <-
                lapply(split(x[, c(by_col_names, span_name, start_name)], x[, by_col_names]),
                       function(z) {
                    sum(head(z[, span_name], -1) - diff(z[, start_name], differences = 1))
                })
        } else {
            start_vs_span_diff <-
                sum(head(x[, span_name], -1) - diff(x[, start_name], differences = 1))
        }
        if (any(unlist(start_vs_span_diff) != 0))
            stop(not_a_valid_object_msg(obj_class,
                                        "Spacings between each 'x$", start_name,
                                        "' do not equal the corresponding values of 'x$",
                                        span_name, "'."))

        ## Check the span *attribute* as well. Not sure how the
        ## attribute would be defined if non-constant spans are
        ## allowed so, for now, assume the attribute will just list
        ## the unique values.
        if (!identical(sort(as.numeric(unique(x[[span_name]]))), sort(as.numeric(span_attr))))
            stop(not_a_valid_object_msg(obj_class,
                                        "The (sorted unique) spacings between each 'x$",
                                        start_name,
                                        "' do not equal 'attr(x, \"", span_name, "\")'."))
    }
    return(TRUE)
}


## Detect any missing indicator x sex x time x age combinations.
check_all_demog_dimension_combinations <- function(x) {
    no_combin <- Reduce("*",
           vapply(demog_change_component_dims(x), FUN = function(z) {
               if (do.call(get_is_by_function_for_dimension(z), list(x)))
                   return(length(unique(x[[get_df_col_names_for_dimensions(
                                              dimensions = z,
                                              spans = FALSE)]])))
               else return(1)
           }, FUN.VALUE = numeric(1)))
    return(identical(as.numeric(no_combin), as.numeric(nrow(x))))
}


#' Tabulate lexis squares
#'
#' For each cell on the lexis plane (e.g., each age-time-sex 'year')
#' tabulate the number of observations that provide information on
#' it. This should be exactly 1 for a valid CCMPP input data frame
#' because CCMPP requires one (and only one) value per cell.
#'
#' This function was written as a validation tool. It is currenlty not
#' used because valid \code{\link{ccmpp_input_df}} objects require all
#' spans to be the same and for them to equal the row-wise differences
#' between the corresponding \dQuote{\code{_start}} columns (e.g.,
#' \code{age_span[i]} must equal \code{age_start[i] - age_start[i-1]}
#' for 1 < \code{i} <= \code{nrow(x)}). In this case, there
#'
#' @param x A data frame.
#' @return A table.
#' @author Mark Wheldon
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

    if (!length(dim_names_x_span_dims_only)) {
        ## If no age or time then just tabulate sex * indicator
        return(table(x[, colnames_x_no_spans]))

    } else {

        span_col_names <- paste0(dim_names_x_span_dims_only, "_span")
        start_col_names <- paste0(dim_names_x_span_dims_only, "_start")
        ## check:
        if (!identical(length(span_col_names), length(start_col_names)))
            stop("Must have '_span' cols for all '_start' cols and vice versa.")

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


## Check that there is a complete sequence of time-age-sex
## entries. The sequence to be checked is implied by the min and max
## time_start and age_start, and their respective spans. E.g., if
## min(time_start) == 1950, max(time_start) == 1959 and all time_spans
## are 1 then there should be rows for all seq(from = 1950, to = 1959,
## by = 1) for all ages and sexes. Similarly for age_start.
##
## This function is restricted to the case where all time_span are
## identical and all age_span are identical (but not necessarily
## mutually identical).
##
## !! THIS IS NO LONGER USED AND MAY NOT BE VALID ANYMORE (2021-07-09)
##
verify_complete_time_age_sex_sequence <- function(x) {
    ## Get info about demographic dimensions and corresponding columns in 'x'
    demog_dims_x <- guess_dimensions_from_df_cols(x)
    dims_w_spans <- demog_dims_x[demog_dims_x %in% get_all_dimensions_w_spans()]

    ## Count the combinations (time x age x sex x indicator)
    n_combinations_span_dims <-
        Reduce("*",
               vapply(X = dims_w_spans, FUN = function(z) {
                   ## Define column names for this 'z'
                   start_col_name <- paste0(z, "_start")
                   span_col_name <- paste0(z, "_span")
                   ## Ensure spans are all the same
                   stopifnot(identical(length(unique(x[[span_col_name]])), 1L))
                   if (!identical(as.numeric(x[[span_col_name]][1]),
                                  as.numeric(0))) {
                       ## For spans > 0 measure length of sequence based on min, max, and span:
                       length(seq(from = min(x[[start_col_name]]),
                                  to = max(x[[start_col_name]]),
                                  by = x[[span_col_name]][1]    #< Assumes all spans are the same
                                  ))
                   } else {
                       ## If span == 0 count the
                       return(1L)
                   }
               },
               FUN.VALUE = integer(1), USE.NAMES = FALSE))
    if (is_by_sex(x)) {
        col_name <- get_df_col_names_for_dimensions(dimensions = "sex", spans = FALSE)
        n_combinations_span_dims <- n_combinations_span_dims * length(unique(x[[col_name]]))
    }
    if (is_by_indicator(x)) {
        col_name <- get_df_col_names_for_dimensions(dimensions = "indicator", spans = FALSE)
        n_combinations_span_dims <- n_combinations_span_dims * length(unique(x[[col_name]]))
    }

    if (identical(nrow(x), n_combinations_span_dims)) return(TRUE)
    else return(FALSE)
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

## Check mode of attributes
check_mode_of_attributes <- function(x, modes_df = get_master_df_of_attr_modes()) {
    lapply(names(attributes(x)), function(attr_nm, modes_df) {
        if (attr_nm %in% modes_df$name) {
            mode_is <- mode(attr(x, attr_nm))
            mode_should_be <- modes_df[modes_df$name == attr_nm, "mode"]
            NA_OK <- modes_df[modes_df$name == attr_nm, "NA_OK"]
            err <- FALSE
            if (NA_OK) {
                if (!is.na(attr(x, attr_nm)) && !identical(mode_is, mode_should_be))
                    err <- TRUE
            } else if (!identical(mode_is, mode_should_be)) {
                err <- TRUE
            }
            if (err)
                stop("'", attr_nm, "' should have 'mode' '",
                     mode_should_be,
                     "' but instead has 'mode' '",
                     mode_is, "'.")
            return(invisible())
        }
    },
    modes_df = modes_df)
    return(TRUE)
}
