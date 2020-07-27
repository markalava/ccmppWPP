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
