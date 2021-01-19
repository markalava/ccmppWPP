context("Test construction and validation of S3 class 'fert_rate_age_f'")

test_that("objects are created properly", {

    expect_s3_class(fert_rate_input_df_time_age <-
                        fert_rate_age_f(S3_fert_rate_time_age_df),
                    "fert_rate_age_f")

    ## Time, Age
    z <- fert_rate_age_f(fert_rate_input_df_time_age)
    expect_s3_class(z, "fert_rate_age_f")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), c("time", "age")))
    expect_true(is_by_age(z))

    x <- subset(fert_rate_input_df_time_age,
                select = -c(time_span, age_span))
    z <- fert_rate_age_f(x)
    expect_s3_class(z, "fert_rate_age_f")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), c("time", "age")))
    expect_true(is_by_age(z))

    x <- subset(fert_rate_input_df_time_age,
                select = -c(time_span, age_span))
    z <- fert_rate_age_f(x, non_zero_fert_ages = 20:25)
    expect_equal(non_zero_fert_ages(z), 20:25)
})


### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS

fert_rate_input_df_time_age <-
    fert_rate_age_f(S3_fert_rate_time_age_df)


test_that("invalid data objects are caught", {

    x <- fert_rate_input_df_time_age

    expect_error(fert_rate_age_f(as.list(x)),
                 "not a data.frame")

    expect_error(fert_rate_age_f(as.matrix(x)),
                 "not a data.frame")

    expect_error(fert_rate_age_f(data.matrix(x)),
                 "not a data.frame")
})


test_that("invalid 'value' column values are caught", {
    x <- fert_rate_input_df_time_age
    x[1, "value"] <- -1
    expect_error(fert_rate_age_f(x), "'value' column has negative elements")
    })


test_that("missing columns are caught", {

    x <- fert_rate_input_df_time_age

    must_have <-
        "must have columns 'time_start', 'age_start', 'value'"

    expect_error(fert_rate_age_f(x[, c("age_start", "value")]),
                 must_have)

    expect_error(fert_rate_age_f(x[, c("time_start",
                                                  "value")]),
                 must_have)
})


test_that("superfluous columns are caught", {

    z <- data.frame(fert_rate_input_df_time_age, source = "census")

    expect_true(## No fail: Automatically removes column
        !("source" %in%
          colnames(fert_rate_age_f(z))))

    y <- ccmppWPP:::new_fert_rate_age_f(z[,
                             c(ccmppWPP:::get_all_req_col_names_for_dimensions(
                                             dimensions =
                                                 c("age", "time")),
                               "source")], value_scale = 1,
                             age_span = 1, time_span = 1)
    expect_error(## Fail: Catches the extra column
        validate_ccmpp_object(y),
        "has superfluous columns. The following are not permitted: 'source'")
})


test_that("dimensions are correctly detected", {
    y <- fert_rate_age_f(fert_rate_input_df_time_age)
    expect_true(is_by_time(y))
    expect_true(is_by_age(y))
    expect_false(is_by_sex(y))
})


test_that("sorting is handled properly", {

    x <- fert_rate_input_df_time_age

    ## Should not Fail: Should re-sort correctly

    z <- x
    z[, "age_start"] <- rev(z$age_start)
    z <- fert_rate_age_f(z)
    expect_s3_class(z, "fert_rate_age_f")
    expect_identical(z$age_start, x$age_start)

    z <- x
    z[, "time_start"] <- rev(z$time_start)
    z <- fert_rate_age_f(z)
    expect_s3_class(z, "fert_rate_age_f")
    expect_identical(z$time_start, x$time_start)

    z <- x[order(x$time_start, x$age_start),]
    z <- fert_rate_age_f(z)
    expect_s3_class(z, "fert_rate_age_f")
    expect_identical(z$sex, x$sex)

    ## Should fail
    validate_ccmpp_object(z)

})


test_that("erroneous sex dimension detected", {
    y <- fert_rate_input_df_time_age
    z <- cbind(y, sex = "female")
    z <- ccmppWPP:::new_fert_rate_age_f(z, non_zero_fert_ages = non_zero_fert_ages(y),
                                time_span = 1, age_span = 1, value_scale = 1)
    expect_error(check_dimensions_for_ccmpp_in_out_df(z),
                 "that correspond to dimensions")

    attr(z, "dimensions") <- unique(c(attr(z, "dimensions"), "sex"))
    expect_error(validate_ccmpp_object(z),
                 "must have dimensions")

    expect_false("sex" %in% colnames(fert_rate_age_f(z)))
})


test_that("sex column removed", {
    z <- cbind(fert_rate_input_df_time_age, sex = "female")
    z <- fert_rate_age_f(z)
    expect_false("sex" %in% colnames(z))
})


test_that("indicator dimension detected", {
    y <- fert_rate_input_df_time_age
    z <- cbind(y, indicator = "ltX")
    z <- ccmppWPP:::new_fert_rate_age_f(z, time_span = time_span(y),
                                age_span = age_span(y), value_scale = 1,
                                non_zero_fert_ages = non_zero_fert_ages(y))
    attr(z, "dimensions") <- unique(c(attr(z, "dimensions"), "indicator"))

    expect_error(validate_ccmpp_object(z),
                 "must have dimensions")

    expect_false("indicator" %in% colnames(fert_rate_age_f(z)))
})


test_that("non-zero fertility rate ages can be changed", {
    y <- fert_rate_input_df_time_age
    nzfa_y <- non_zero_fert_ages(y)
    non_zero_fert_ages(y) <- 30:40
    expect_false(identical(as.double(nzfa_y),
                           as.double(non_zero_fert_ages(y))))
    expect_identical(as.double(non_zero_fert_ages(y)),
                     as.double(seq(from = 30, to = 40, by = 1)))
})


test_that("zero fertility rate ages are set to zero (value column)", {
    test <- fert_rate_age_f(expand.grid(age_start = 0:3,
                                        time_start = 1950:1951, value = 1),
                            non_zero_fert_ages = 2)
    zero_fert_rows <- !test$age_start %in% non_zero_fert_ages(test)
    expect_identical(as.double(test[zero_fert_rows, "value"]),
                     rep(as.double(0), sum(zero_fert_rows)))
})


test_that("zero fertility rate ages that are non-zero are caught", {
    y <- expand.grid(age_start = 0:5, time_start = 1950:1952,
                    age_span = 1, time_span = 1,
                    value = 1)
    y <- new_fert_rate_age_f(y,
             age_span = 1,
             time_span = 1,
             dimensions = get_req_dimensions_for_ccmpp_in_out_classes("fert_rate_age_f"),
             value_type = get_value_types_for_ccmpp_in_out_classes("fert_rate_age_f"),
             value_scale = 1,
             non_zero_fert_ages = 0:5)
    expect_s3_class(validate_ccmpp_object(y), "fert_rate_age_f")
    y <- new_fert_rate_age_f(y,
             age_span = 1,
             time_span = 1,
             dimensions = get_req_dimensions_for_ccmpp_in_out_classes("fert_rate_age_f"),
             value_type = get_value_types_for_ccmpp_in_out_classes("fert_rate_age_f"),
             value_scale = 1,
             non_zero_fert_ages = 2)
    expect_error(validate_ccmpp_object(y), "have non-zero 'value' for at least some 'time_start's")
    })

