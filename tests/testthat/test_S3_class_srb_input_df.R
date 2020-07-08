context("Test methods for S3 class 'srb_input_df'")

test_that("valid member created", {
    expect_s3_class(srb_input_df_time,
                    "srb_input_df")
    })


test_that("age dimension detected", {
    y <- srb_input_df_time
    z <- cbind(y, age_start = 0, age_span = 1)
    z <- new_srb_input_df(z, time_span = time_span(y))
    attr(z, "dimensions") <- c(attr(z, "dimensions"), "age")
    expect_error(validate_ccmpp_object(z),
                 "has an age dimension")

    expect_error(srb_input_df(z, time_span = time_span(y)),
                 "has an age dimension")
})


test_that("age columns removed", {
    y <- srb_input_df_time
    z <- cbind(y, age_start = 0, age_span = 1)
    z <- srb_input_df(z, time_span = time_span(y))
    expect_false("age_start" %in% colnames(z))
    expect_false("age_span" %in% colnames(z))
})

