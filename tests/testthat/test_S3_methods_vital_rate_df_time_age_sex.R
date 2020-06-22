context("Test methods for S3 class 'vital_rate_df_time_age_sex'")

test_that("subsetting works as desired", {

    expect_not_s3_class <- function(object, class, exact = FALSE) {
        expect_error(expect_s3_class(object = object, class = class,
                                     exact = exact))
    }

    data("S3_vital_rate_time_age_sex_test_df")
    x <- S3_vital_rate_time_age_sex_test_df
    y <- vital_rate_df_time_age_sex(x)
    expect_s3_class(y, "vital_rate_df_time_age_sex")
    expect_s3_class(y, "data.frame")

    ## NB Warning only issued if run from top level
    expect_not_s3_class(y[, "age_start"], "vital_rate_df_time_age_sex")
    expect_not_s3_class(y$age_start, "vital_rate_df_time_age_sex")
    expect_not_s3_class(y[["age_start"]], "vital_rate_df_time_age_sex")
})


test_that("replacement with valid columns passes", {

    data("S3_vital_rate_time_age_sex_test_df")
    x <- S3_vital_rate_time_age_sex_test_df
    y <- vital_rate_df_time_age_sex(x)
    expect_s3_class(y, "vital_rate_df_time_age_sex")
    expect_s3_class(y, "data.frame")

    z <- y
    z[, "age_start"] <- z$age_start
    expect_s3_class(z, "vital_rate_df_time_age_sex")
    z <- y
    z$age_start <- z$age_start
    expect_s3_class(z, "vital_rate_df_time_age_sex")
    z <- y
    z[["age_start"]] <- z$age_start
    expect_s3_class(z, "vital_rate_df_time_age_sex")

    z <- y
    z[, "time_start"] <- z$time_start
    expect_s3_class(z, "vital_rate_df_time_age_sex")
    z <- y
    z$time_start <- z$time_start
    expect_s3_class(z, "vital_rate_df_time_age_sex")
    z <- y
    z[["time_start"]] <- z$time_start
    expect_s3_class(z, "vital_rate_df_time_age_sex")

    z <- y
    z[, "sex"] <- z$sex
    expect_s3_class(z, "vital_rate_df_time_age_sex")
    z <- y
    z$sex <- z$sex
    expect_s3_class(z, "vital_rate_df_time_age_sex")
    z <- y
    z[["sex"]] <- z$sex
    expect_s3_class(z, "vital_rate_df_time_age_sex")
})


test_that("bad sorting is detected", {

    data("S3_vital_rate_time_age_sex_test_df")
    y <- vital_rate_df_time_age_sex(S3_vital_rate_time_age_sex_test_df)
    expect_s3_class(y, "vital_rate_df_time_age_sex")
    expect_s3_class(y, "data.frame")

    z <- y
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


test_that("superfluous columns are detected", {
    data("S3_vital_rate_time_age_sex_test_df")

    y <- vital_rate_df_time_age_sex(S3_vital_rate_time_age_sex_test_df)
    expect_s3_class(y, "vital_rate_df_time_age_sex")
    expect_s3_class(y, "data.frame")

    expect_error((y$source <- "census"), "superfluous columns")
    expect_error((y[, "source"] <- "census"), "superfluous columns")
    expect_error((y[["source"]] <- "census"), "superfluous columns")
    })
