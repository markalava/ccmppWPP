context("Test methods for S3 class 'mig_net_count_age_sex'")

test_that("valid member created", {
    expect_s3_class(mig_net_count_tot_b(wpp_input_example$mig_net_count_tot_b),
                    "mig_net_count_tot_b")
    })

### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS (already tested)

mig_net_count_tot_input_df_time <-
    mig_net_count_tot_b(wpp_input_example$mig_net_count_tot_b)


test_that("indicator dimension detected", {
    x <- mig_net_count_tot_input_df_time
    y <- cbind(x, indicator = "ltX")
    z <- ccmppWPP:::new_mig_net_count_tot_b(y, value_scale = 1, time_span = 1)
    attr(z, "dimensions") <- c(attr(z, "dimensions"), "indicator")
    expect_error(validate_ccmppWPP_object(z),
                 "must have dimension")
    ## Indicator removed
    expect_false("indicator" %in% colnames(mig_net_count_tot_b(y)))
})


test_that("indicator column removed", {
    y <- mig_net_count_tot_input_df_time
    z <- cbind(y, indicator = "ltX")
    z <- mig_net_count_tot_b(z)
    expect_false("indicator" %in% colnames(z))
})
