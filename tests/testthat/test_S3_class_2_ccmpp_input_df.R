context("Test construction and validation of S3 class 'ccmpp_input_df'")

test_that("objects are created properly", {

### Time, Age, Sex
    ## Specify dimensions
    y <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df,
                       dimensions = c("time", "age", "sex"))
    expect_s3_class(y, "ccmpp_input_df")
    expect_s3_class(y, "data.frame")
    expect_true(setequal(demog_change_component_dims(y), c("time", "age", "sex")))

    ## Guess dimensions
    y <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df)
    expect_s3_class(y, "ccmpp_input_df")
    expect_s3_class(y, "data.frame")
    expect_true(setequal(demog_change_component_dims(y), c("time", "age", "sex")))

    ## Guess spans
    y <- ccmpp_input_df(
        subset(S3_demog_change_component_time_age_sex_test_df,
               select = -c(time_span, age_span)))
    expect_s3_class(y, "ccmpp_input_df")
    expect_s3_class(y, "data.frame")
    expect_true(setequal(demog_change_component_dims(y), c("time", "age", "sex")))

    ## CHARACTER values
    y <- as_ccmpp_input_df(wpp_input_example$mig_parameter)
    expect_s3_class(y, "ccmpp_input_df")

### Time, Age
    ## Specify dimensions
    x <- S3_demog_change_component_time_age_test_df[, c("time_start", "age_start",
                                                        "time_span", "age_span",
                                                        "value")]
    z <- ccmpp_input_df(x,
                       dimensions = c("time", "age"))
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), c("time", "age")))

    ## Guess dimensions and spans
    x <- S3_demog_change_component_time_age_test_df[, c("time_start", "age_start", "value")]
    z <- ccmpp_input_df(x)
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), c("time", "age")))

### Time, Sex
    ## specify dimensions
    x <- S3_demog_change_component_time_sex_test_df[, c("time_start",
                                                        "time_span", "sex", "value")]
    z <- ccmpp_input_df(x, dimensions = c("time", "sex"))
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), c("time", "sex")))

    ## Guess dimensions and spans
    x <- S3_demog_change_component_time_sex_test_df[,
                             c("time_start", "time_span", "sex", "value")]
    z <- ccmpp_input_df(x)
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), c("time", "sex")))

### Time
    ## Specify dimensions
    x <- S3_demog_change_component_time_test_df[, c("time_start", "time_span", "value")]
    z <- ccmpp_input_df(x, dimensions = "time")
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), "time"))

    x <- S3_demog_change_component_time_test_df[, c("time_start", "time_span", "value")]
    z <- ccmpp_input_df(x)
    expect_s3_class(z, "ccmpp_input_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), "time"))
})


test_that("invalid data objects are caught", {

    x <- S3_demog_change_component_time_age_sex_test_df

    expect_error(ccmpp_input_df(as.list(x),
                       dimensions = c("time", "age", "sex")),
                 "not a data.frame")

    expect_error(ccmpp_input_df(as.matrix(x),
                       dimensions = c("time", "age", "sex")),
                 "not a data.frame")

    expect_error(ccmpp_input_df(data.matrix(x),
                       dimensions = c("time", "age", "sex")),
                 "not a data.frame")
})


test_that("missing columns are caught", {

    x <- S3_demog_change_component_time_age_sex_test_df

    must_have <-
        "must have columns 'time_start', 'sex', 'age_start', 'value'"

    expect_error(ccmpp_input_df(x[, c("sex",
                                                  "age_start", "value")],
                       dimensions = c("time", "age", "sex")),
                 must_have)

    expect_error(ccmpp_input_df(x[, c("time_start",
                                                  "age_start", "value")],
                       dimensions = c("time", "age", "sex")),
                 must_have)

    expect_error(ccmpp_input_df(x[, c("time_start", "sex",
                                                  "value")],
                       dimensions = c("time", "age", "sex")),
                 must_have)

    expect_error(ccmpp_input_df(x[, c("time_start", "sex",
                                                  "age_start")],
                       dimensions = c("time", "age", "sex")),
                 must_have)
})


test_that("superfluous columns are caught", {

    x <- S3_demog_change_component_time_age_sex_test_df
    z <- data.frame(x, source = "census")

    expect_true(## No fail: Automatically removes column
        !("source" %in%
          colnames(ccmpp_input_df(z,
                       dimensions = c("time", "age", "sex")))))

    y <- ccmppWPP:::new_ccmpp_input_df(z[,
                             c(ccmppWPP:::get_all_req_col_names_for_dimensions(
                                             dimensions =
                                                 c("age", "time", "sex")),
                               "source")],
                             value_type = "real", value_scale = 1,
                       dimensions = c("time", "age", "sex"))
    expect_error(## Fail: Catches the extra column
        validate_ccmpp_object(y),
        "has superfluous columns. The following are not permitted: 'source'")
})


test_that("'indicator' column OK", {

    x <- S3_demog_change_component_time_age_sex_test_df
    z <- data.frame(x, indicator = "mig_type")

    expect_true(## No fail: Automatically removes column bc 'indicator' not in dimensions.
        !("indicator" %in%
          colnames(ccmpp_input_df(z,
                       dimensions = c("time", "age", "sex")))))

    expect_true(## No fail: Keeps column
        "indicator" %in%
          colnames(ccmpp_input_df(z,
                       dimensions = c("time", "age", "sex", "indicator"))))

    expect_true(## No fail: Keeps column
        "indicator" %in%
          colnames(ccmpp_input_df(z)))

    y <- ccmpp_input_df(z)
    expect_s3_class(validate_ccmpp_object(y), "ccmpp_input_df")

    z <- transform(z, indicator = 84)
    expect_error(ccmpp_input_df(z),
                 "Cannot coerce column 'indicator' to 'character'")
})


test_that("'value_type' is checked properly", {

    x <- S3_demog_change_component_time_age_sex_test_df

    expect_error(ccmpp_input_df(x,
                                           value_type = "census"),
                 "'value_type' must be one of")

        expect_error(ccmpp_input_df(x,
                                           value_type = "proportion"),
                     "values less than 0 or greater than 1 are present")
})


test_that("'value_type' is set properly", {
    x <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df,
                   dimensions = c("time", "age", "sex"))
    value_type(x) <- "real"
    expect_identical(value_type(x), "real")
    value_type(x) <- "ratio"
    expect_identical(value_type(x), "ratio")
    expect_true(is.na(value_scale(x)))
    })


test_that("dimensions are correctly detected", {
    y <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df,
                                   dimensions = c("time", "age", "sex"))
    expect_true(is_by_time(y))
    expect_true(is_by_age(y))
    expect_true(is_by_sex(y))
})


test_that("non-squareness is caught", {
    x <- S3_demog_change_component_time_age_sex_test_df

    ## OK to omit a whole time, or sex group. NB: omitting only the
    ## first time or age here because otherwise would have to also
    ## adjust the '_span' columns. Also only test time and sex because
    ## omitting age_start '0' triggers a different error.
    omit_i <- which(x$time_start == 1950)
    y <- x[-omit_i, ]
    expect_s3_class(ccmpp_input_df(y),
                    "ccmpp_input_df")

    omit_i <- which(x$sex == "male")
    y <- x[-omit_i, ]
    expect_s3_class(ccmpp_input_df(y),
                    "ccmpp_input_df")

    ## NOT OK to just remove one age-time-sex combination.
    omit_i <- which(x$age_start == 5 & x$time_start == 1950 & x$sex == "male")
    y <- x[-omit_i, ]
    ## 'y' has an entry for 1950, age 0, 'female', but the entry for
    ## 'male' is missing. This is invalid:
    expect_error(capture.output(ccmpp_input_df(y),
                                file = OS_null_file_string),
                 "does not have exactly one 'value'")
    ## It's not enough to omit 'female' entry as well because there
    ## are entries for age 5 for all other years and sexes. E.g.,
    ## 1951, age 5, 'male' and 'female' exists; the absence of the
    ## 1950 entry is invalid.
    omit_i <- which(x$age_start == 5 & x$time_start == 1950)
    y <- x[-omit_i, ]
    ## 'y' has an entry for 1950, age 0, 'female', but the entry for
    ## 'male' is missing. This is invalid:
    expect_error(capture.output(ccmpp_input_df(y),
                                file = OS_null_file_string),
                 "does not have exactly one 'value'")
})


test_that("sorting is handled properly", {

    x <- S3_demog_change_component_time_age_sex_test_df

    ## Should not Fail: Should re-sort correctly

    z <- x
    z[, "age_start"] <- rev(z$age_start)
    z <- ccmpp_input_df(z, dimensions = c("time", "age", "sex"))
    expect_s3_class(z, "ccmpp_input_df")
    expect_identical(z$age_start, x$age_start)

    z <- x
    z[, "time_start"] <- rev(z$time_start)
    z <- ccmpp_input_df(z, dimensions = c("time", "age", "sex"))
    expect_s3_class(z, "ccmpp_input_df")
    expect_identical(z$time_start, x$time_start)

    z <- x[order(x$time_start, x$age_start),] #sex = male,female,male,female,...
    z <- ccmpp_input_df(z, dimensions = c("time", "age", "sex"))
    expect_s3_class(z, "ccmpp_input_df")
    expect_identical(z$sex, x$sex)

})


test_that("'NA's are not allowed", {
    x <- S3_demog_change_component_time_age_sex_test_df
    x[1, "value"] <- NA
    expect_warning(expect_error(ccmpp_input_df(x),
                                "'value' column has missing entries"),
                   "'value' column has some 'NA' entries")
})
