context("Test methods for S3 class 'demog_change_component_df'")

### OBJECTS NEEDED (tested already)

dcc_df_time_age_sex <-
    demog_change_component_df(S3_demog_change_component_time_age_sex_test_df,
                              dimensions = c("time", "age", "sex"))


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
                         type = "line",
                         framework = "ggplot2",
                         plot = FALSE),
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


test_that("aggregate method works", {

    expect_s3_class(aggregate(dcc_df_time_age_sex, dimension = "time"),
                    "demog_change_component_df")
    expect_s3_class(aggregate(dcc_df_time_age_sex, dimension = "age"),
                    "demog_change_component_df")

    x <- aggregate(as_ccmpp_input_df(dcc_df_time_age_sex), dimension = "time")
    expect_s3_class(x, "ccmpp_input_df")
    x <- aggregate(as_ccmpp_input_df(dcc_df_time_age_sex), dimension = "age")
    expect_s3_class(x, "ccmpp_input_df")

    x <- subset_age(dcc_df_time_age_sex, ages = 2:10)
    class(x) <- c("ccmpp_input_df", class(x))
    expect_error(aggregate(x, dimension = "age"),
                 "The aggregate of 'x' cannot be coerced to the class in argument 'out_class'")
    expect_s3_class(aggregate(as_demog_change_component_df(x), dimension = "time"),
                    "demog_change_component_df")
    expect_s3_class(aggregate(x, dimension = "time", out_class = "demog_change_component_df"),
                    "demog_change_component_df")
    expect_s3_class(aggregate(x, dimension = "time", out_class = "data.frame"),
                    "data.frame")

    x <- dcc_df_time_age_sex
    value_type(x) <- "ratio"
    expect_error(aggregate(x, dimension = "time"),
                 "but the only aggregatable or abridgable 'value_type's are")

    expect_error(aggregate(dcc_df_time_age_sex, "age", out_class = "matrix"),
                 "'out_class' must only use classes in this list")
})







