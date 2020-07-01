context("Test methods for S3 class 'ccmpp_input_df'")

test_that("subsetting works as desired", {

    expect_not_s3_class <- function(object, class, exact = FALSE) {
        expect_error(expect_s3_class(object = object, class = class,
                                     exact = exact))
    }

    y <- ccmpp_input_df_time_age_sex

    ## NB Warning only issued if run from top level
    expect_not_s3_class(y[, "age_start"], "ccmpp_input_df")
    expect_not_s3_class(y$age_start, "ccmpp_input_df")
    expect_not_s3_class(y[["age_start"]], "ccmpp_input_df")
})


test_that("replacement with valid columns passes (time, age, sex)", {

    z <- ccmpp_input_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$age_start <- z$age_start
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_s3_class(z, "ccmpp_input_df")

    z <- ccmpp_input_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$time_start <- z$time_start
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_s3_class(z, "ccmpp_input_df")

    z <- ccmpp_input_df_time_age_sex
    z[, "sex"] <- z$sex
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$sex <- z$sex
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["sex"]] <- z$sex
    expect_s3_class(z, "ccmpp_input_df")
 })


test_that("replacement with valid columns passes (time, age)", {
    z <- ccmpp_input_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$age_start <- z$age_start
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_s3_class(z, "ccmpp_input_df")

    z <- ccmpp_input_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$time_start <- z$time_start
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_s3_class(z, "ccmpp_input_df")
 })


test_that("replacement with valid columns passes (time, sex)", {
    z <- ccmpp_input_df_time_age_sex
    z[, "time_start"] <- z$time_start
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$time_start <- z$time_start
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["time_start"]] <- z$time_start
    expect_s3_class(z, "ccmpp_input_df")

    z <- ccmpp_input_df_time_age_sex
    z[, "sex"] <- z$sex
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z$sex <- z$sex
    expect_s3_class(z, "ccmpp_input_df")
    z <- ccmpp_input_df_time_age_sex
    z[["sex"]] <- z$sex
    expect_s3_class(z, "ccmpp_input_df")
 })


test_that("bad sorting is detected (time, age, sex)", {
    z <- ccmpp_input_df_time_age_sex
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
    z <- ccmpp_input_df_time_age_sex
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
    z <- ccmpp_input_df_time_age_sex
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
    z <- ccmpp_input_df_time_age_sex
    expect_error((z[, "time_start"] <- rev(z$time_start)),
                 "must be sorted")
    expect_error((z$time_start <- rev(z$time_start)),
                 "must be sorted")
    expect_error((z[["time_start"]] <- rev(z$time_start)),
                 "must be sorted")
})


test_that("superfluous columns are detected", {

    y <- ccmpp_input_df_time_age_sex

    expect_error((y$source <- "census"), "superfluous columns")
    expect_error((y[, "source"] <- "census"), "superfluous columns")
    expect_error((y[["source"]] <- "census"), "superfluous columns")
    })


test_that("method for 'subset' works as expected", {
    expect_is(subset(ccmpp_input_df_time_age_sex, age_start == 0), class = "ccmpp_input_df")
    expect_is(subset(ccmpp_input_df_time_age_sex, age_start == 0 & time_start == 1950),
              class = "ccmpp_input_df")

    expect_warning(expect_is(subset(ccmpp_input_df_time_age_sex, value < 0), class = "data.frame"),
                   "Subset result is not a valid 'ccmpp_input_df'")
})
