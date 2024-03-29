

### OBJECTS NEEDED (tested already)

dcc_df_time_age_sex <-
    demog_change_component_df(S3_demog_change_component_time_age_sex_test_df)


test_that("subsetting works as desired", {

    y <- dcc_df_time_age_sex

    ## NB Warning only issued if run from top level
    expect_not_s3_class(y[, "age_start"], "demog_change_component_df")
    expect_not_s3_class(y$age_start, "demog_change_component_df")
    expect_not_s3_class(y[["age_start"]], "demog_change_component_df")

    z <- y[y$age_start == 0 & y$time_start == 1950,]
    w <- y[y$age_start == 0, c("age_start", "value")]
})


test_that("replacement with valid columns drops class(time, age, sex)", {

    z <- dcc_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$age_start <- z$age_start
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_not_s3_class(z, "demog_change_component_df")

    z <- dcc_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$time_start <- z$time_start
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_not_s3_class(z, "demog_change_component_df")

    z <- dcc_df_time_age_sex
    z[, "sex"] <- z$sex
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$sex <- z$sex
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["sex"]] <- z$sex
    expect_not_s3_class(z, "demog_change_component_df")
 })


test_that("replacement with valid columns drops class(time, age)", {
    z <- dcc_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$age_start <- z$age_start
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_not_s3_class(z, "demog_change_component_df")

    z <- dcc_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$time_start <- z$time_start
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_not_s3_class(z, "demog_change_component_df")
 })


test_that("replacement with valid columns drops class (time, sex)", {
    z <- dcc_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$time_start <- z$time_start
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_not_s3_class(z, "demog_change_component_df")

    z <- dcc_df_time_age_sex
    z[, "sex"] <- z$sex
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$sex <- z$sex
    expect_not_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["sex"]] <- z$sex
    expect_not_s3_class(z, "demog_change_component_df")
})


test_that("names, rownames and colnames drop class", {
    ## Currently, cannot figure out how to make 'names' work without
    ## infinite recursion errors.
    x <- dcc_df_time_age_sex
    names(x) <-
        gsub("value", "test", names(x))
    expect_s3_class(x, "demog_change_component_df")

    x <- dcc_df_time_age_sex
    dimnames(x)[[2]] <-
        gsub("value", "test", dimnames(x)[[2]])
    expect_not_s3_class(x, "demog_change_component_df")

    x <- dcc_df_time_age_sex
    row.names(x) <-
        gsub("value", "test", row.names(x))
    expect_not_s3_class(x, "demog_change_component_df")

    x <- dcc_df_time_age_sex
    rownames(x) <-
        gsub("value", "test", rownames(x))
    expect_not_s3_class(x, "demog_change_component_df")

    x <- dcc_df_time_age_sex
    colnames(x) <-
        gsub("value", "test", colnames(x))
    expect_not_s3_class(x, "demog_change_component_df")
})


test_that("'rbind()' drops class", {
    z <- rbind(dcc_df_time_age_sex, dcc_df_time_age_sex)
    expect_not_s3_class(z, "demog_change_component_df")
})


test_that("'transform()' drops class", {
    z <- transform(dcc_df_time_age_sex, age_start = age_start - 1)
    expect_not_s3_class(z, "demog_change_component_df")
})


test_that("'within()' drops class", {
    z <- within(dcc_df_time_age_sex, { age_start <- age_start - 1 })
    expect_not_s3_class(z, "demog_change_component_df")
})


test_that("'plot' method produces a valid 'ggplot2' object", {
    if (!requireNamespace("ggplot2")) skip("'ggplot2' not installed.")

    expect_s3_class(plot(dcc_df_time_age_sex,
                         type = "line"),
                    c("gg", "ggplot"))

})


test_that("printing returns an object of class 'demog_change_component_df'", {
    capture.output(x <- print(dcc_df_time_age_sex),
                   file = OS_null_file_string)
    expect_s3_class(x, "demog_change_component_df")

    capture.output(x <- print(dcc_df_time_age_sex, print_what = "info"),
                   file = OS_null_file_string)
    expect_s3_class(x, "demog_change_component_df")

    capture.output(x <- print(dcc_df_time_age_sex, print_what = "table"),
                   file = OS_null_file_string)
    expect_s3_class(x, "demog_change_component_df")
})







