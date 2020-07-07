context("Test methods for S3 class 'survival_ratio_input_df'")

test_that("valid member created", {
    expect_s3_class(survival_ratio_input_df_time_age_sex,
                    "survival_ratio_input_df")
    })


test_that("Non-zero age detected", {
    y <- survival_ratio_input_df_time_age_sex
    z <- subset(y, age_start > 0)
    z <- new_survival_ratio_input_df(z,
                                     age_span = age_span(y),
                                     time_span = time_span(y),
                                     dimensions = demog_change_component_dimensions(y))
    expect_error(validate_ccmpp_object(z),
                 "'age_start' does not start at '0'")
})
