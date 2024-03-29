test_that("Message suppression by option works", {
    expect_message(fert_rate_age_f(S3_fert_rate_time_age_df),
                   "'non_zero_fert_ages' set to ")
    op <- options()
    options(ccmppWPP.suppress_S3_class_messages = TRUE)
    expect_silent(fert_rate_age_f(S3_fert_rate_time_age_df))
    options(op)
    })
