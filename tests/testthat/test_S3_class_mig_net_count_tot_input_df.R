context("Test methods for S3 class 'mig_net_count_tot_b'")

test_that("valid member created", {
    expect_s3_class(mig_net_count_tot_input_df_time,
                    "mig_net_count_tot_b")
    })


test_that("age dimension detected", {
    x <- mig_net_count_tot_input_df_time
    y <- cbind(x, age_start = 0, age_span = 1)
    z <- new_mig_net_count_tot_b(y, time_span = time_span(x))
    attr(z, "dimensions") <- c(attr(z, "dimensions"), "age")
    attr(z, "age_span") <- 1
    expect_error(validate_ccmpp_object(z),
                 "must have dimension")
})


test_that("age columns removed", {
    y <- mig_net_count_tot_input_df_time
    z <- cbind(y, age_start = 0, age_span = 1)
    z <- mig_net_count_tot_b(z)
    expect_false("age_start" %in% colnames(z))
    expect_false("age_span" %in% colnames(z))
})


test_that("sex dimension detected", {
    y <- mig_net_count_tot_input_df_time
    z <- cbind(y, sex = "female")
    z <- new_mig_net_count_tot_b(z, time_span = time_span(y))
    attr(z, "dimensions") <- c(attr(z, "dimensions"), "sex")
    expect_error(validate_ccmpp_object(z),
                 "must have dimension")
})


test_that("sex column removed", {
    y <- mig_net_count_tot_input_df_time
    z <- cbind(y, sex = "female")
    z <- mig_net_count_tot_b(z)
    expect_false("sex" %in% colnames(z))
})


test_that("indicator dimension detected", {
    y <- mig_net_count_tot_input_df_time
    z <- cbind(y, indicator = "ltX")
    z <- new_mig_net_count_tot_b(z, time_span = time_span(y))
    attr(z, "dimensions") <- c(attr(z, "dimensions"), "indicator")
    expect_error(validate_ccmpp_object(z),
                 "must have dimension")
})


test_that("indicator column removed", {
    y <- mig_net_count_tot_input_df_time
    z <- cbind(y, indicator = "ltX")
    z <- mig_net_count_tot_b(z)
    expect_false("indicator" %in% colnames(z))
})
