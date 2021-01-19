## All validation functions are defined here and use inheritance. This
## is to ensure that the correct validation method is called according
## to the class of the object.

not_a_valid_object_msg <- function(class, ...) {
    paste0("Not a valid '", class, "' object:\n", ...)
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
