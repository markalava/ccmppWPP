###
### Methods for *existing* generics. See '..._utility_functions.R' for new generics and methods.
###

###-----------------------------------------------------------------------------
### * tibble

#' @importFrom tibble add_column
add_column.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tibble add_row
add_row.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tibble column_to_rownames
column_to_rownames.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tibble rownames_to_column
rownames_to_column.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tibble remove_rownames
remove_rownames.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

###-----------------------------------------------------------------------------
### * dplyr

###-----------------------------------------------------------------------------
### ** 'arrange' and friends

#' @importFrom dplyr across
across.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr arrange
arrange.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr filter
filter.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Subsetting a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr mutate
mutate.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr mutate_all
mutate_all.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr mutate_if
mutate_if.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr mutate_at
mutate_at.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr pull
pull.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr rename
rename.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr select
select.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr slice
slice.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr summarise
summarise.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr summarize
summarize.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr transmute
transmute.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr transmute_all
transmute_all.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr transmute_if
transmute_if.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr transmute_at
transmute_at.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

###-----------------------------------------------------------------------------
### ** joins

#' @importFrom dplyr anti_join
anti_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr bind_rows
bind_rows.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr full_join
full_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr inner_join
inner_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr intersect
intersect.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr left_join
left_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr right_join
right_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr semi_join
semi_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr setdiff
setdiff.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom dplyr union
union.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

###-----------------------------------------------------------------------------
### * tidyr

#' @importFrom tidyr complete
complete.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tidyr drop_na
drop_na.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tidyr expand
expand.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tidyr fill
fill.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tidyr gather
gather.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tidyr pivot_longer
pivot_longer.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tidyr pivot_wider
pivot_wider.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tidyr replace_na
replace_na.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tidyr separate
separate.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tidyr separate_rows
separate_rows.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tidyr spread
spread.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

#' @importFrom tidyr unite
unite.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes. Note that this can occurr even when this warning is not issued.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

