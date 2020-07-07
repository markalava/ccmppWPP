context("Tst methods for S3 class 'survival_ratio_input_df'")

test_that("subsetting drops class", {

    y <- survival_ratio_input_df_time_age_sex

    ## NB Warning only issued if run from top level
    expect_not_s3_class(y[, "age_start"], "survival_ratio_input_df")
    expect_not_s3_class(y$age_start, "survival_ratio_input_df")
    expect_not_s3_class(y[["age_start"]], "survival_ratio_input_df")

    expect_not_s3_class(y[1:nrow(y), colnames(y)], "survival_ratio_input_df")
})


test_that("subset-replacement drops class", {
    z <- survival_ratio_input_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_not_s3_class(z, "survival_ratio_input_df")
    z <- survival_ratio_input_df_time_age_sex
    z$age_start <- z$age_start
    expect_not_s3_class(z, "survival_ratio_input_df")
    z <- survival_ratio_input_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_not_s3_class(z, "survival_ratio_input_df")
    })

