context("Test methods for S3 class 'ccmpp_input_list'")

test_that("subsetting returns valid objects", {
    expect_s3_class(subset_time(ccmpp_input_list_example, times = 1950),
                    "ccmpp_input_list")

    expect_s3_class(subset_age(ccmpp_input_list_example, ages = 0:60),
                    "ccmpp_input_list")

    expect_error(subset_age(ccmpp_input_list_example, ages = 10:60),
                 "'0' must be in ages")
})
