context("Test methods for S3 class 'mig_net_count_tot_input_df'")

test_that("valid member created", {
    expect_s3_class(mig_net_count_tot_input_df_time,
                    "mig_net_count_tot_input_df")
    })


test_that("age dimension detected", {
    y <- mig_net_count_tot_input_df_time
    z <- cbind(y, age_start = 0, age_span = 1)
    z <- new_mig_net_count_tot_input_df(z, time_span = time_span(y), dimensions = "time")
    expect_error(validate_ccmpp_object(z),
                 "has an age dimension")

    expect_error(mig_net_count_tot_input_df(z, time_span = time_span(y),
                              dimensions = c("age", "time")),
                 "cannot have an age dimension")
})


test_that("age columns removed", {
    y <- mig_net_count_tot_input_df_time
    z <- cbind(y, age_start = 0, age_span = 1)
    z <- mig_net_count_tot_input_df(z, time_span = time_span(y), dimensions = "time")
    expect_false("age_start" %in% colnames(z))
    expect_false("age_span" %in% colnames(z))
})


test_that("sex dimension detected", {
    y <- mig_net_count_tot_input_df_time
    z <- cbind(y, sex = "female")
    z <- new_mig_net_count_tot_input_df(z, time_span = time_span(y),
                                dimensions = c("time", "sex"))
    expect_error(validate_ccmpp_object(z),
                 "has a sex dimension")

    expect_error(mig_net_count_tot_input_df(z, time_span = time_span(y),
                              dimensions = c("sex", "time")),
                 "has a sex dimension")
})


test_that("sex column removed", {
    y <- mig_net_count_tot_input_df_time
    z <- cbind(y, sex = "female")
    z <- mig_net_count_tot_input_df(z, time_span = time_span(y),
                            dimensions = c("time"))
    expect_false("sex" %in% colnames(z))
})
