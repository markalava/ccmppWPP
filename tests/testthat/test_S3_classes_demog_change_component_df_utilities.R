context("Test S3 class 'demog_change_component_df' utility functions")

test_that("subsetting works properly", {

    z <- subset_time(dcc_df_time_age_sex, times = 1950, drop = TRUE)
    expect_false(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_age(dcc_df_time_age_sex, ages = 10, drop = TRUE)
    expect_false(is_by_age(z))
    expect_true(is_by_time(z))
    expect_true(is_by_sex(z))

    z <- subset_sex(dcc_df_time_age_sex, sexes = "female", drop = TRUE)
    expect_false(is_by_sex(z))
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))

    z <- subset_time(dcc_df_time_age_sex, times = 1950, drop = FALSE)
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_age(dcc_df_time_age_sex, ages = 0, drop = FALSE)
    expect_true(is_by_age(z))
    expect_true(is_by_time(z))
    expect_true(is_by_sex(z))

    z <- subset_sex(dcc_df_time_age_sex, sexes = "female", drop = FALSE)
    expect_true(is_by_sex(z))
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))

    z <- subset_time(dcc_df_time_age_sex, times = 1950:1960)
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_age(dcc_df_time_age_sex, ages = 0:10)
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_sex(dcc_df_time_age_sex, sexes = c("female", "male"))
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))
})


test_that("subsetting errors are caught", {

    expect_warning(expect_error(subset_time(dcc_df_time_age_sex, times = "a"),
                 "is.finite"), "NAs introduced")
    expect_warning(expect_error(subset_age(dcc_df_time_age_sex, ages = "a"),
                 "is.finite"), "NAs introduced")
    expect_error(subset_sex(dcc_df_time_age_sex, sexes = 1),
                 "character")

    expect_error(subset_time(subset_time(dcc_df_time_age_sex, times = 1950:1960), 1970),
                 "does not have any entries with")
    expect_error(subset_age(subset_age(dcc_df_time_age_sex, ages = 0:10), 11),
                 "does not have any entries with")

    expect_error(subset_sex(subset_sex(dcc_df_time_age_sex, sexes = "male", drop = TRUE),
                            sex = "female"),
                 "is_by_sex")
    expect_error(subset_time(subset_time(dcc_df_time_age_sex, times = 1950, drop = TRUE),
                             time = 1960),
                 "is_by_time")
    expect_error(subset_age(subset_age(dcc_df_time_age_sex, ages = 10, drop = TRUE),
                            age = 20),
                 "is_by_age")
})


test_that("'value_type' is checked properly", {

    x <- S3_demog_change_component_time_age_sex_test_df

    expect_error(demog_change_component_df(x,
                                           value_type = "census"),
                 "'value_type' must be one of")

        expect_error(demog_change_component_df(x,
                                           value_type = "proportion"),
                     "values less than 0 or greater than 1 are present")
})


test_that("'value_type' is set properly", {
    x <- dcc_df_time_age_sex
    value_type(x) <- "real"
    expect_identical(value_type(x), "real")
    value_type(x) <- "ratio"
    expect_identical(value_type(x), "ratio")
    expect_true(is.na(value_scale(x)))
})


test_that("'value_scale' is set properly", {
    z <- dcc_df_time_age_sex
    value_scale(z) <- 1
    expect_identical(as.numeric(value_scale(z)), 1)
    value_scale(z) <- 10
    expect_identical(as.numeric(value_scale(z)), 10)
    })


test_that("dimensions are correctly detected", {
    y <- demog_change_component_df(S3_demog_change_component_time_age_sex_test_df,
                                   dimensions = c("time", "age", "sex"))
    expect_true(is_by_time(y))
    expect_true(is_by_age(y))
    expect_true(is_by_sex(y))
})
