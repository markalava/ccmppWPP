context("Test methods for S3 class 'demog_change_component_df'")

test_that("subsetting works as desired", {

    expect_not_s3_class <- function(object, class, exact = FALSE) {
        expect_error(expect_s3_class(object = object, class = class,
                                     exact = exact))
    }

    y <- dcc_df_time_age_sex

    ## NB Warning only issued if run from top level
    expect_not_s3_class(y[, "age_start"], "demog_change_component_df")
    expect_not_s3_class(y$age_start, "demog_change_component_df")
    expect_not_s3_class(y[["age_start"]], "demog_change_component_df")
})


test_that("replacement with valid columns passes (time, age, sex)", {

    z <- dcc_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$age_start <- z$age_start
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_s3_class(z, "demog_change_component_df")

    z <- dcc_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$time_start <- z$time_start
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_s3_class(z, "demog_change_component_df")

    z <- dcc_df_time_age_sex
    z[, "sex"] <- z$sex
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$sex <- z$sex
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["sex"]] <- z$sex
    expect_s3_class(z, "demog_change_component_df")
 })


test_that("replacement with valid columns passes (time, age)", {
    z <- dcc_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$age_start <- z$age_start
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_s3_class(z, "demog_change_component_df")

    z <- dcc_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$time_start <- z$time_start
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_s3_class(z, "demog_change_component_df")
 })


test_that("replacement with valid columns passes (time, sex)", {
    z <- dcc_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$time_start <- z$time_start
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_s3_class(z, "demog_change_component_df")

    z <- dcc_df_time_age_sex
    z[, "sex"] <- z$sex
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z$sex <- z$sex
    expect_s3_class(z, "demog_change_component_df")
    z <- dcc_df_time_age_sex
    z[["sex"]] <- z$sex
    expect_s3_class(z, "demog_change_component_df")
 })


test_that("bad sorting is detected (time, age, sex)", {
    z <- dcc_df_time_age_sex
    expect_error((z[, "age_start"] <- rev(z$age_start)),
                 "must be sorted")
    expect_error((z$age_start <- rev(z$age_start)),
                 "must be sorted")
    expect_error((z[["age_start"]] <- rev(z$age_start)),
                 "must be sorted")

    expect_error((z[, "time_start"] <- rev(z$time_start)),
                 "must be sorted")
    expect_error((z$time_start <- rev(z$time_start)),
                 "must be sorted")
    expect_error((z[["time_start"]] <- rev(z$time_start)),
                 "must be sorted")

    expect_error((z[, "sex"] <- rep(c("male", "female"), length.out = nrow(z))),
                 "must be sorted")
    expect_error((z$sex <- rep(c("male", "female"), length.out = nrow(z))),
                 "must be sorted")
    expect_error((z[["sex"]] <- rep(c("male", "female"), length.out = nrow(z))),
                 "must be sorted")
})


test_that("bad sorting is detected (time, age)", {
    z <- dcc_df_time_age_sex
    expect_error((z[, "age_start"] <- rev(z$age_start)),
                 "must be sorted")
    expect_error((z$age_start <- rev(z$age_start)),
                 "must be sorted")
    expect_error((z[["age_start"]] <- rev(z$age_start)),
                 "must be sorted")

    expect_error((z[, "time_start"] <- rev(z$time_start)),
                 "must be sorted")
    expect_error((z$time_start <- rev(z$time_start)),
                 "must be sorted")
    expect_error((z[["time_start"]] <- rev(z$time_start)),
                 "must be sorted")
})


test_that("bad sorting is detected (time, sex)", {
    z <- dcc_df_time_age_sex
    expect_error((z[, "time_start"] <- rev(z$time_start)),
                 "must be sorted")
    expect_error((z$time_start <- rev(z$time_start)),
                 "must be sorted")
    expect_error((z[["time_start"]] <- rev(z$time_start)),
                 "must be sorted")

    set.seed(1)
    expect_error((z[, "sex"] <- sample(c("male", "female"), size = nrow(z), replace = TRUE)),
                 "must be sorted")
    expect_error((z$sex <- sample(c("male", "female"), size = nrow(z), replace = TRUE)),
                 "must be sorted")
    expect_error((z[["sex"]] <- sample(c("male", "female"), size = nrow(z), replace = TRUE)),
                 "must be sorted")
})


test_that("bad sorting is detected (time)", {
    z <- dcc_df_time_age_sex
    expect_error((z[, "time_start"] <- rev(z$time_start)),
                 "must be sorted")
    expect_error((z$time_start <- rev(z$time_start)),
                 "must be sorted")
    expect_error((z[["time_start"]] <- rev(z$time_start)),
                 "must be sorted")
})


test_that("superfluous columns are detected", {

    y <- dcc_df_time_age_sex

    expect_error((y$source <- "census"), "superfluous columns")
    expect_error((y[, "source"] <- "census"), "superfluous columns")
    expect_error((y[["source"]] <- "census"), "superfluous columns")
    })
