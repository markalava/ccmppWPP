context("Test methods for S3 class 'survival_ratio_age_sex'")

### MAKE OBJECTS AVAILABLE TO REMAINDER OF TESTS (already tested)

survival_ratio_input_df_time_age_sex <-
    survival_ratio_age_sex(subset(wpp_input_example$life_table_age_sex,
           indicator == "lt_Sx", select = -indicator))


test_that("subsetting drops class", {

    y <- survival_ratio_input_df_time_age_sex

    ## NB Warning only issued if run from top level
    expect_not_s3_class(y[, "age_start"], "survival_ratio_age_sex")
    expect_not_s3_class(y$age_start, "survival_ratio_age_sex")
    expect_not_s3_class(y[["age_start"]], "survival_ratio_age_sex")

    expect_not_s3_class(y[1:nrow(y), colnames(y)], "survival_ratio_age_sex")
})


test_that("subset-replacement drops class", {
    z <- survival_ratio_input_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_not_s3_class(z, "survival_ratio_age_sex")
    z <- survival_ratio_input_df_time_age_sex
    z$age_start <- z$age_start
    expect_not_s3_class(z, "survival_ratio_age_sex")
    z <- survival_ratio_input_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_not_s3_class(z, "survival_ratio_age_sex")
    })

