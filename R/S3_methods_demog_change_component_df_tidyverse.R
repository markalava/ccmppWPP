###-----------------------------------------------------------------------------
### * dplyr

###-----------------------------------------------------------------------------
### ** 'arrange' and friends

across.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

add_column.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

add_row.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

arrange.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

column_to_rownames.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

filter.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Subsetting a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

mutate.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

mutate_all.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

mutate_if.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

mutate_at.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

pull.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

remove_rownames.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

rename.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

rownames_to_column.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

select.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

slice.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

summarise.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

summarize.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

transmute.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

transmute_all.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

transmute_if.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

transmute_at.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

###-----------------------------------------------------------------------------
### ** joins

anti_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

bind_rows.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

full_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

inner_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

intersect.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

left_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

right_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

semi_join.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

setdiff.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

union.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

###-----------------------------------------------------------------------------
### * tidyr

complete.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

drop_na.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

expand.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

fill.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

gather.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

pivot_longer.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

pivot_wider.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

replace_na.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

separate.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

separate_rows.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

spread.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

unite.demog_change_component_df <- function(x, ...) {
    if (identical(parent.frame(), .GlobalEnv)) {
        S3_class_warning("Manipulating a '",
                oldClass(x)[1],
                "' will not preserve the class or attributes.")
    }
    x <- NextMethod()
    if (is_demog_change_component_df(x)) return(as.data.frame(x))
    else return(x)
}

