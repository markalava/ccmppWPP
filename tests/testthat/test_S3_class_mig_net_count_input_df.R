context("Test methods for S3 class 'mig_net_prop_age_sex'")

test_that("valid member created", {
    expect_s3_class(mig_net_prop_input_df_time_age_sex,
                    "mig_net_prop_age_sex")
    })
