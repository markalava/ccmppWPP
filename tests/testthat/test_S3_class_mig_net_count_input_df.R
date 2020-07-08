context("Test methods for S3 class 'mig_net_count_input_df'")

test_that("valid member created", {
    expect_s3_class(mig_net_count_input_df_time_age_sex,
                    "mig_net_count_input_df")
    })


test_that("Non-zero age detected", {
    y <- mig_net_count_input_df_time_age_sex
    z <- subset(y, age_start > 0)
    z <- new_mig_net_count_input_df(z,
                                     age_span = age_span(y),
                                     time_span = time_span(y))
    expect_error(validate_ccmpp_object(z),
                 "'age_start' does not start at '0'")
})


test_that("indicator dimension detected", {
    y <- mig_net_count_input_df_time_age_sex
    z <- cbind(y, indicator = "ltX")
    z <- new_mig_net_count_input_df(z, time_span = time_span(y),
                                age_span = age_span(y))
    attr(z, "dimensions") <- c(attr(z, "dimensions"), "indicator")
    expect_error(validate_ccmpp_object(z),
                 "has a indicator dimension")

    expect_error(mig_net_count_input_df(z, time_span = time_span(y)),
                 "has a indicator dimension")
})


test_that("indicator column removed", {
    y <- mig_net_count_input_df_time_age_sex
    z <- cbind(y, indicator = "ltX")
    z <- mig_net_count_input_df(z, time_span = time_span(y),
                            age_span = age_span(y))
    expect_false("indicator" %in% colnames(z))
})
