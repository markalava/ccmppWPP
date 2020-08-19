context("Test methods for S3 class 'survival_ratio_age_sex'")

test_that("valid member created", {
    expect_s3_class(survival_ratio_input_df_time_age_sex,
                    "survival_ratio_age_sex")
})


test_that("successfully extracted from 'life_table...' object", {
    expect_s3_class(survival_ratio_component(
        life_table_input_df_indicator_time_age_sex),
        "survival_ratio_age_sex")
    })


test_that("Non-zero age detected", {
    y <- survival_ratio_input_df_time_age_sex
    z <- subset(y, age_start > 0)
    z <- ccmppWPP:::new_survival_ratio_age_sex(z,
                                     age_span = age_span(y),
                                     time_span = time_span(y))
    expect_error(validate_ccmpp_object(z),
                 "'age_start' does not start at '0'")
})


test_that("Required dimensions enforced", {
    x <- survival_ratio_input_df_time_age_sex
    expect_error(survival_ratio_age_sex(subset(as.data.frame(x),
                                        select =
                                            -c(time_span, time_start))),
                 "must have columns")
})
