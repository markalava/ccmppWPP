context("Test methods for S3 class 'srb'")


test_that("valid member created", {
    expect_s3_class(srb(wpp_input_example$srb),
                    "srb")
})


### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS

srb_time <- srb(wpp_input_example$srb)


test_that("age dimension detected", {
    x <- srb_time
    y <- cbind(x, age_start = 0, age_span = 1)
    z <- ccmppWPP:::new_srb(y, time_span = 1)
    attr(z, "dimensions") <- c(attr(z, "dimensions"), "age")
    attr(z, "age_span") <- 1
    expect_error(validate_ccmppWPP_object(z),
                 "must have dimension")
})


test_that("age columns removed", {
    y <- srb_time
    z <- cbind(y, age_start = 0, age_span = 1)
    z <- srb(z)
    expect_false("age_start" %in% colnames(z))
    expect_false("age_span" %in% colnames(z))
})


test_that("attribute replacement functions work", {

    expect_error(value_scale(srb_time) <- 10,
                 "Cannot change the value_scale of an object")

    expect_error(value_type(srb_time) <- "real",
                 "'value_type' of 'x' cannot be changed")
})


test_that("Invalid 'value's are caught", {
    x <- srb_time
    x[1, "value"]  <- -1
    expect_error(srb(x),
                 "'value' column has negative elements")
    })
