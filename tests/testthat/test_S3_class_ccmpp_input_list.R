context("Test S3 class 'ccmpp_input_list'")

test_that("valid member created", {
    expect_s3_class(ccmpp_input_list_example,
                    "ccmpp_input_list")
})


test_that("all elements required", {
    x <- ccmpp_input_list_example
    y <- x[1:3]
    y <- new_ccmpp_input_list(y, age_span = age_span(x),
                              time_span = time_span(x))
    expect_error(validate_ccmpp_object(y),
                 "must have these elements")
})
