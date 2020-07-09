context("Test methods for S3 class 'mig_net_count_input_df'")

test_that("valid member created", {
    expect_s3_class(mig_net_count_input_df_time_age_sex,
                    "mig_net_count_input_df")
    })


test_that("Non-zero age detected", {
    x <- mig_net_count_input_df_time_age_sex
    y <- subset(x, age_start > 0)
    z <- new_mig_net_count_input_df(y, age_span = 1, time_span = 1)
    expect_error(validate_ccmpp_object(z),
                 "'age_start' does not start at '0'")
    expect_error(mig_net_count_input_df(y),
                 "'age_start' does not start at '0'")
})


test_that("indicator dimension detected", {
    x <- mig_net_count_input_df_time_age_sex
    y <- cbind(x, indicator = "ltX")
    z <- new_mig_net_count_input_df(y, age_span = 1, time_span = 1)
    attr(z, "dimensions") <- c(attr(z, "dimensions"), "indicator")
    expect_error(validate_ccmpp_object(z),
                 "must have dimension")

    expect_false("indicator" %in% colnames(mig_net_count_input_df(y)))
})


test_that("indicator column removed", {
    y <- mig_net_count_input_df_time_age_sex
    z <- cbind(y, indicator = "ltX")
    z <- mig_net_count_input_df(z)
    expect_false("indicator" %in% colnames(z))
})
