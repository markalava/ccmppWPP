context("Test construction and validation of S3 class 'fert_rate_input_df'")

test_that("objects are created properly", {

    ## Time, Age
    x <- fert_rate_input_df_time_age
    z <- fert_rate_input_df(x, age_span = 1, time_span = 1,
                       dimensions = c("time", "age"))
    expect_s3_class(z, "fert_rate_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dimensions(z), c("time", "age")))
    expect_true(is_by_age(z))

    x <- fert_rate_input_df_time_age
    z <- fert_rate_input_df(x, age_span = 1, time_span = 1)
    expect_s3_class(z, "fert_rate_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dimensions(z), c("time", "age")))
    expect_true(is_by_age(z))
})


test_that("invalid data objects are caught", {

    x <- fert_rate_input_df_time_age

    expect_error(fert_rate_input_df(as.list(x),
                      age_span = attr(y, "age_span"),
                      time_span = attr(y, "time_span"),
                       dimensions = c("time", "age")),
                 "not a data.frame")

    expect_error(fert_rate_input_df(as.matrix(x),
                      age_span = attr(y, "age_span"),
                      time_span = attr(y, "time_span"),
                       dimensions = c("time", "age")),
                 "not a data.frame")

    expect_error(fert_rate_input_df(data.matrix(x),
                      age_span = attr(y, "age_span"),
                      time_span = attr(y, "time_span"),
                       dimensions = c("time", "age")),
                 "not a data.frame")
})


test_that("missing columns are caught", {

    x <- fert_rate_input_df_time_age

    must_have <-
        "must have columns 'time_start', 'age_start', 'value'"

    expect_error(fert_rate_input_df(x[, c("age_start", "value")],
                                            age_span = 1, time_span = 1,
                       dimensions = c("time", "age")),
                 must_have)

    expect_error(fert_rate_input_df(x[, c("time_start",
                                                  "value")],
                                            age_span = 1, time_span = 1,
                       dimensions = c("time", "age")),
                 must_have)
})


test_that("superfluous columns are caught", {

    x <- fert_rate_input_df_time_age
    z <- data.frame(x, source = "census")

    expect_true(## No fail: Automatically removes column
        !("source" %in%
          colnames(fert_rate_input_df(z, age_span = 1, time_span = 1,
                       dimensions = c("time", "age")))))

    y <- new_fert_rate_input_df(z[,
                             c(ccmppWPP::get_all_req_col_names_for_dimensions(
                                             dimensions =
                                                 c("age", "time")),
                               "source")],
                             age_span = 1, time_span = 1,
                       dimensions = c("time", "age"))
    expect_error(## Fail: Catches the extra column
        validate_ccmpp_object(y),
        "has superfluous columns. The following are not permitted: 'source'")
})


test_that("dimensions are correctly detected", {
    y <- fert_rate_input_df(fert_rate_input_df_time_age,
                                   dimensions = c("time", "age"))
    expect_true(is_by_time(y))
    expect_true(is_by_age(y))
    expect_false(is_by_sex(y))
})


test_that("sorting is handled properly", {

    x <- fert_rate_input_df_time_age

    ## Should not Fail: Should re-sort correctly

    z <- x
    z[, "age_start"] <- rev(z$age_start)
    z <- fert_rate_input_df(z, age_span = 1, time_span = 1,
                       dimensions = c("time", "age"))
    expect_s3_class(z, "fert_rate_input_df")
    expect_identical(z$age_start, x$age_start)

    z <- x
    z[, "time_start"] <- rev(z$time_start)
    z <- fert_rate_input_df(z, age_span = 1, time_span = 1,
                       dimensions = c("time", "age"))
    expect_s3_class(z, "fert_rate_input_df")
    expect_identical(z$time_start, x$time_start)

    z <- x[order(x$time_start, x$age_start),]
    z <- fert_rate_input_df(z, age_span = 1, time_span = 1,
                       dimensions = c("time", "age"))
    expect_s3_class(z, "fert_rate_input_df")
    expect_identical(z$sex, x$sex)

    ## Should fail
    validate_ccmpp_object(z)

})


test_that("sex dimension detected", {
    y <- fert_rate_input_df_time_age
    z <- cbind(y, sex = "female")
    z <- new_fert_rate_input_df(z, time_span = time_span(y),
                                age_span = age_span(y),
                                dimensions = c("time", "age", "sex"),
                                non_zero_fert_ages = non_zero_fert_ages(y))
    expect_error(validate_ccmpp_object(z),
                 "has a sex dimension")

    expect_error(fert_rate_input_df(z, time_span = time_span(y),
                              dimensions = c("sex", "time")),
                 "has a sex dimension")
})


test_that("sex column removed", {
    y <- fert_rate_input_df_time_age
    z <- cbind(y, sex = "female")
    z <- fert_rate_input_df(z, time_span = time_span(y),
                            age_span = age_span(y),
                            dimensions = c("time", "age"))
    expect_false("sex" %in% colnames(z))
})



test_that("indicator dimension detected", {
    y <- fert_rate_input_df_time_age
    z <- cbind(y, indicator = "ltX")
    z <- new_fert_rate_input_df(z, time_span = time_span(y),
                                age_span = age_span(y),
                                dimensions = c("time", "age", "indicator"),
                                non_zero_fert_ages = non_zero_fert_ages(y))
    expect_error(validate_ccmpp_object(z),
                 "has a indicator dimension")

    expect_error(fert_rate_input_df(z, time_span = time_span(y),
                              dimensions = c("indicator", "time")),
                 "has a indicator dimension")
})


test_that("indicator column removed", {
    y <- fert_rate_input_df_time_age
    z <- cbind(y, indicator = "ltX")
    z <- fert_rate_input_df(z, time_span = time_span(y),
                            age_span = age_span(y),
                            dimensions = c("time", "age"))
    expect_false("indicator" %in% colnames(z))
})
