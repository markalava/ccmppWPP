context("Test methods for S3 class 'ccmpp_input_df'")

test_that("subsetting works as desired", {

    y <- ccmpp_input_df_time_age_sex

    ## NB Warning only issued if run from top level
    expect_not_s3_class(y[, "age_start"], "ccmpp_input_df")
    expect_not_s3_class(y$age_start, "ccmpp_input_df")
    expect_not_s3_class(y[["age_start"]], "ccmpp_input_df")
})


test_that("replacement with valid columns passes (time, age, sex)", {

    z <- ccmpp_input_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$age_start <- z$age_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_not_s3_class(z, "ccmpp_input_df")

    z <- ccmpp_input_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$time_start <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")

    z <- ccmpp_input_df_time_age_sex
    z[, "sex"] <- z$sex
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$sex <- z$sex
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["sex"]] <- z$sex
    expect_not_s3_class(z, "ccmpp_input_df")
 })


test_that("replacement with valid columns passes (time, age)", {
    z <- ccmpp_input_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$age_start <- z$age_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_not_s3_class(z, "ccmpp_input_df")

    z <- ccmpp_input_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$time_start <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")
 })


test_that("replacement with valid columns passes (time, sex)", {
    z <- ccmpp_input_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$time_start <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_not_s3_class(z, "ccmpp_input_df")

    z <- ccmpp_input_df_time_age_sex
    z[, "sex"] <- z$sex
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$sex <- z$sex
    expect_not_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["sex"]] <- z$sex
    expect_not_s3_class(z, "ccmpp_input_df")
 })
