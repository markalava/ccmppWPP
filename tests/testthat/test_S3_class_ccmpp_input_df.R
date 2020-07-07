context("Test construction and validation of S3 class 'ccmpp_input_df'")

test_that("objects are created properly", {

    ## Time, Age, Sex
    y <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df,
                       dimensions = c("time", "age", "sex"))
    expect_s3_class(y, "ccmpp_input_df")
    expect_s3_class(y, "data.frame")
    expect_true(setequal(demog_change_component_dimensions(y), c("time", "age", "sex")))

    y <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df)
    expect_s3_class(y, "ccmpp_input_df")
    expect_s3_class(y, "data.frame")
    expect_true(setequal(demog_change_component_dimensions(y), c("time", "age", "sex")))

    x <- S3_demog_change_component_time_age_sex_test_df[, c("time_start", "sex", "age_start", "value")]
    z <- ccmpp_input_df(x, age_span = 1, time_span = 1,
                       dimensions = c("time", "age", "sex"))
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dimensions(y), c("time", "age", "sex")))

    ## Time, Age
    x <- S3_demog_change_component_time_age_test_df[, c("time_start", "age_start", "value")]
    z <- ccmpp_input_df(x, age_span = 1, time_span = 1,
                       dimensions = c("time", "age"))
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dimensions(z), c("time", "age")))
    expect_true(is_by_age(z))

    x <- S3_demog_change_component_time_age_test_df[, c("time_start", "age_start", "value")]
    z <- ccmpp_input_df(x, age_span = 1, time_span = 1)
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dimensions(z), c("time", "age")))
    expect_true(is_by_age(z))

    ## Time, Sex
    x <- S3_demog_change_component_time_sex_test_df[, c("time_start", "sex", "value")]
    z <- ccmpp_input_df(x, time_span = 1,
                       dimensions = c("time", "sex"))
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dimensions(z), c("time", "sex")))
    expect_true(is_by_sex(z))

    x <- S3_demog_change_component_time_sex_test_df[, c("time_start", "time_span", "sex", "value")]
    z <- ccmpp_input_df(x)
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dimensions(z), c("time", "sex")))
    expect_true(is_by_sex(z))

    ## Time
    x <- S3_demog_change_component_time_test_df[, c("time_start", "value")]
    z <- ccmpp_input_df(x, time_span = 1,
                       dimensions = "time")
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dimensions(z), "time"))
    expect_true(is_by_time(z))

    x <- S3_demog_change_component_time_test_df[, c("time_start", "time_span", "value")]
    z <- ccmpp_input_df(x)
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dimensions(z), "time"))
    expect_true(is_by_time(z))
})


test_that("invalid data objects are caught", {

    x <- S3_demog_change_component_time_age_sex_test_df

    expect_error(ccmpp_input_df(as.list(x),
                      age_span = attr(y, "age_span"),
                      time_span = attr(y, "time_span"),
                       dimensions = c("time", "age", "sex")),
                 "not a data.frame")

    expect_error(ccmpp_input_df(as.matrix(x),
                      age_span = attr(y, "age_span"),
                      time_span = attr(y, "time_span"),
                       dimensions = c("time", "age", "sex")),
                 "not a data.frame")

    expect_error(ccmpp_input_df(data.matrix(x),
                      age_span = attr(y, "age_span"),
                      time_span = attr(y, "time_span"),
                       dimensions = c("time", "age", "sex")),
                 "not a data.frame")
})


test_that("missing columns are caught", {

    x <- S3_demog_change_component_time_age_sex_test_df

    must_have <-
        "must have columns 'time_start', 'sex', 'age_start', 'value'"

    expect_error(ccmpp_input_df(x[, c("sex",
                                                  "age_start", "value")],
                                            age_span = 1, time_span = 1,
                       dimensions = c("time", "age", "sex")),
                 must_have)

    expect_error(ccmpp_input_df(x[, c("time_start",
                                                  "age_start", "value")],
                                            age_span = 1, time_span = 1,
                       dimensions = c("time", "age", "sex")),
                 must_have)

    expect_error(ccmpp_input_df(x[, c("time_start", "sex",
                                                  "value")],
                                            age_span = 1, time_span = 1,
                       dimensions = c("time", "age", "sex")),
                 must_have)

    expect_error(ccmpp_input_df(x[, c("time_start", "sex",
                                                  "age_start")],
                                            age_span = 1, time_span = 1,
                       dimensions = c("time", "age", "sex")),
                 must_have)
})


test_that("superfluous columns are caught", {

    x <- S3_demog_change_component_time_age_sex_test_df
    z <- data.frame(x, source = "census")

    expect_true(## No fail: Automatically removes column
        !("source" %in%
          colnames(ccmpp_input_df(z, age_span = 1, time_span = 1,
                       dimensions = c("time", "age", "sex")))))

    y <- new_ccmpp_input_df(z[,
                             c(ccmppWPP::get_all_req_col_names(
                                             dimensions =
                                                 c("age", "time", "sex")),
                               "source")],
                             age_span = 1, time_span = 1,
                             value_type = "real",
                       dimensions = c("time", "age", "sex"))
    expect_error(## Fail: Catches the extra column
        validate_ccmpp_object(y),
        "has superfluous columns. The following are not permitted: 'source'")
})


test_that("'value_type' is checked properly", {

    x <- S3_demog_change_component_time_age_sex_test_df

    expect_error(ccmpp_input_df(x, age_span = 1, time_span = 1,
                                           value_type = "census"),
                 "'value_type' must be one of")

        expect_error(ccmpp_input_df(x, age_span = 1, time_span = 1,
                                           value_type = "proportion"),
                     "values less than 0 or greater than 1 are present")
})


test_that("dimensions are correctly detected", {
    y <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df,
                                   dimensions = c("time", "age", "sex"))
    expect_true(is_by_time(y))
    expect_true(is_by_age(y))
    expect_true(is_by_sex(y))
})


test_that("sorting is handled properly", {

    x <- S3_demog_change_component_time_age_sex_test_df

    ## Should not Fail: Should re-sort correctly

    z <- x
    z[, "age_start"] <- rev(z$age_start)
    z <- ccmpp_input_df(z, age_span = 1, time_span = 1,
                       dimensions = c("time", "age", "sex"))
    expect_s3_class(z, "ccmpp_input_df")
    expect_identical(z$age_start, x$age_start)

    z <- x
    z[, "time_start"] <- rev(z$time_start)
    z <- ccmpp_input_df(z, age_span = 1, time_span = 1,
                       dimensions = c("time", "age", "sex"))
    expect_s3_class(z, "ccmpp_input_df")
    expect_identical(z$time_start, x$time_start)

    z <- x[order(x$time_start, x$age_start),] #sex = male,female,male,female,...
    z <- ccmpp_input_df(z, age_span = 1, time_span = 1,
                       dimensions = c("time", "age", "sex"))
    expect_s3_class(z, "ccmpp_input_df")
    expect_identical(z$sex, x$sex)

    ## Should fail
    validate_ccmpp_object(z)

})
