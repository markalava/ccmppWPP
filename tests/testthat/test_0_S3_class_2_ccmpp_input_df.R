
test_that("objects are created properly", {

### Time, Age, Sex
    ## Specify dimensions
    y <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df)
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
    z <- ccmpp_input_df(x)
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
    z <- ccmpp_input_df(x)
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
    z <- ccmpp_input_df(x)
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

    expect_error(ccmpp_input_df(as.list(x)),
                 "not a data.frame")

    expect_error(ccmpp_input_df(as.matrix(x)),
                 "not a data.frame")

    expect_error(ccmpp_input_df(data.matrix(x)),
                 "not a data.frame")
})


test_that("superfluous columns are caught", {

    x <- S3_demog_change_component_time_age_sex_test_df
    z <- data.frame(x, source = "census")

    y <- ccmppWPP:::new_ccmpp_input_df(z[,
                             c(ccmppWPP:::get_all_req_col_names_for_dimensions(
                                             dimensions =
                                                 c("age", "time", "sex")),
                               "source")],
                             value_type = "real", value_scale = 1,
                       dimensions = c("time", "age", "sex"))
    expect_error(## Fail: Catches the extra column
        validate_ccmppWPP_object(y),
        "has superfluous columns. The following are not permitted: 'source'")
})


test_that("'indicator' column OK", {

    x <- S3_demog_change_component_time_age_sex_test_df
    z <- data.frame(x, indicator = "mig_type")

    expect_true(## No fail: Keeps column
        "indicator" %in%
          colnames(ccmpp_input_df(z)))

    y <- ccmpp_input_df(z)
    expect_s3_class(validate_ccmppWPP_object(y), "ccmpp_input_df")

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
    x <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df)
    value_type(x) <- "real"
    expect_identical(value_type(x), "real")
    value_type(x) <- "ratio"
    expect_identical(value_type(x), "ratio")
    expect_true(is.na(value_scale(x)))
    })


test_that("dimensions are correctly detected", {
    y <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df)
    expect_true(is_by_time(y))
    expect_true(is_by_age(y))
    expect_true(is_by_sex(y))
})


test_that("Attribute types are checked", {
    x <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df)
    attr(x, "time_span") <- "test"
    expect_error(validate_ccmppWPP_object(x),
                 "'time_span' should have 'mode' 'numeric'")

    x <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df)
    attr(x, "age_span") <- "test"
    expect_error(validate_ccmppWPP_object(x),
                 "'age_span' should have 'mode' 'numeric'")

    x <- ccmpp_input_df(S3_demog_change_component_time_age_sex_test_df)
    attr(x, "value_scale") <- "test"
    expect_error(validate_ccmppWPP_object(x),
                 "'value_scale' should have 'mode' 'numeric'")
})


test_that("Unequal spans are caught", {
    x <- S3_demog_change_component_time_age_sex_test_df
    omit_i <- which(x$time_start == 1951)
    y <- x[-omit_i, ]
    change_i <- which(y$time_start == 1950)
    y[change_i, "time_span"] <- 2
    attr(y, "time_span") <- c(1, 2)
    expect_error(ccmpp_input_df(y),
                 "All spans must be equal to a \\*single\\* common value")

    ## This will pass the 'unequal spans' test because time_span and
    ## age_span both have values c(1,2). However it will fail the
    ## requirement that all spans be equal to a single common value.
    omit_i <- which(x$time_start == 1951)
    y <- x[-omit_i, ]
    change_i <- which(x$time_start == 1950)
    y[change_i, "time_span"] <- 2
    attr(y, "time_span") <- c(1, 2)

    omit_j <- which(y$age_start == 1)
    y <- y[-omit_j, ]
    change_j <- which(y$age_start == 0)
    y[change_j, "age_span"] <- 2
    attr(y, "age_span") <- c(1, 2)

    expect_error(ccmpp_input_df(y),
                 "All spans must be equal to a \\*single\\* common value")
})


test_that("Inconsistency between spans and starts are detected", {
    x <- S3_demog_change_component_time_age_sex_test_df
    x[2, "age_span"] <- 2
    expect_error(ccmpp_input_df(x),
                 "Spacings between each 'x\\$age_start' do not equal the corresponding values of 'x\\$age_span")

    x <- S3_demog_change_component_time_age_sex_test_df
    x <- x[!x$age_start == 1, ]
    expect_error(ccmpp_input_df(x),
                 "Spacings between each 'x\\$age_start' do not equal the corresponding values of 'x\\$age_span")
})


test_that("non-squareness is caught", {
    x <- S3_demog_change_component_time_age_sex_test_df

    ## Remove 1950 just for 'male'.
    omit_i <- which(x$time_start == 1950 & x$sex == "male")
    y <- x[-omit_i, ]

    ## 'y' has an entry for 1950, age 0, 'female', but the entry for
    ## 'male' is missing. This is invalid:
    expect_error(ccmpp_input_df(y),
                 "Some combinations of")

    ## The above example must use '1950', not any other year, because
    ## then the spans would not equal the differences between the
    ## time_start values and a different error would be triggered.
})


test_that("sorting is handled properly", {

    x <- S3_demog_change_component_time_age_sex_test_df

    ## Should not Fail: Should re-sort correctly

    z <- x
    z[, "age_start"] <- rev(z$age_start)
    z <- ccmpp_input_df(z)
    expect_s3_class(z, "ccmpp_input_df")
    expect_identical(z$age_start, x$age_start)

    z <- x
    z[, "time_start"] <- rev(z$time_start)
    z <- ccmpp_input_df(z)
    expect_s3_class(z, "ccmpp_input_df")
    expect_identical(z$time_start, x$time_start)

    z <- x[order(x$time_start, x$age_start),] #sex = male,female,male,female,...
    z <- ccmpp_input_df(z)
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
