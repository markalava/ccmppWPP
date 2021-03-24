context("Test that raw data extracts can be converted to 'ccmppWPP' objects.")


## Load required data
data("france_pop_data_raw")


test_that("Raw data can be coerced to ccmpp_input_df objects", {
    x <- lapply(france_pop_data_raw$ccmppWPP_inputs,
                function(z) is_ccmpp_input_df(as_ccmpp_input_df(z)))
    expect_true(all(unlist(x)))
    })


test_that("Raw data can be coerced to ccmpp_input_list objects", {
    x <- lapply(france_pop_data_raw$ccmppWPP_inputs,
                function(z) as_ccmpp_input_df(z))
    ## Make all have common times
    common_times <- Reduce("intersect", lapply(x, function(z) ccmppWPP::times(z)))
    x <- lapply(x, function(z, common_times) {
        subset_time(z, common_times)
    }, common_times = common_times)
    expect_s3_class(as_ccmpp_input_list(x), "ccmpp_input_list")
    })

