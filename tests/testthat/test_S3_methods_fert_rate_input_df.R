context("Test methods for S3 class 'fert_rate_age_f'")

test_that("valid member created", {
    expect_s3_class(fert_rate_input_df_time_age,
                    "fert_rate_age_f")
    })


test_that("subsetting drops class", {

    y <- fert_rate_input_df_time_age

    ## NB Warning only issued if run from top level
    expect_not_s3_class(y[, "age_start"], "fert_rate_age_f")
    expect_not_s3_class(y$age_start, "fert_rate_age_f")
    expect_not_s3_class(y[["age_start"]], "fert_rate_age_f")

    expect_not_s3_class(y[1:nrow(y), colnames(y)], "fert_rate_age_f")
})


test_that("subset-replacement drops class", {
    z <- fert_rate_input_df_time_age
    z[, "age_start"] <- z$age_start
    expect_not_s3_class(z, "fert_rate_age_f")
    z <- fert_rate_input_df_time_age
    z$age_start <- z$age_start
    expect_not_s3_class(z, "fert_rate_age_f")
    z <- fert_rate_input_df_time_age
    z[["age_start"]] <- z$age_start
    expect_not_s3_class(z, "fert_rate_age_f")
    })

