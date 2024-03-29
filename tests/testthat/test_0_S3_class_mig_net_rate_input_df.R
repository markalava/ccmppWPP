

test_that("valid member created", {
    expect_s3_class(mig_net_rate_age_sex(wpp_input_example$mig_net_rate_age_sex),
                    "mig_net_rate_age_sex")
})

### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS (already tested)

mig_net_rate_input_df_time_age_sex <-
    mig_net_rate_age_sex(wpp_input_example$mig_net_rate_age_sex)



test_that("Non-zero age detected", {
    y <- mig_net_rate_input_df_time_age_sex
    z <- subset(y, age_start > 0)
    z <- ccmppWPP:::new_mig_net_rate_age_sex(z,
                                             value_scale = 1,
                                     age_span = age_span(y),
                                     time_span = time_span(y))
    expect_error(validate_ccmppWPP_object(z),
                 "'age_start' does not start at '0'")
})
