context("Test methods for S3 class 'srb'")

test_that("valid member created", {
    expect_s3_class(srb_time,
                    "srb")
    })


test_that("age dimension detected", {
    x <- srb_time
    y <- cbind(x, age_start = 0, age_span = 1)
    z <- new_srb(y, time_span = 1)
    attr(z, "dimensions") <- c(attr(z, "dimensions"), "age")
    attr(z, "age_span") <- 1
    expect_error(validate_ccmpp_object(z),
                 "must have dimension")
})


test_that("age columns removed", {
    y <- srb_time
    z <- cbind(y, age_start = 0, age_span = 1)
    z <- srb(z)
    expect_false("age_start" %in% colnames(z))
    expect_false("age_span" %in% colnames(z))
})

