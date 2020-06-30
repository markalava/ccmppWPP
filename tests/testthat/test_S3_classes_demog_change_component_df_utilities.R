context("Test S3 class 'demog_change_component_df' utility functions")

test_that("subsetting works properly", {

    z <- subset_time(dcc_df_time_age_sex, time = 1950)
    expect_false(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_age(dcc_df_time_age_sex, age = 10)
    expect_false(is_by_age(z))
    expect_true(is_by_time(z))
    expect_true(is_by_sex(z))

    z <- subset_sex(dcc_df_time_age_sex, sex = "female")
    expect_false(is_by_sex(z))
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))

    z <- subset_time(dcc_df_time_age_sex, time = 1950, drop = FALSE)
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_age(dcc_df_time_age_sex, age = 0, drop = FALSE)
    expect_true(is_by_age(z))
    expect_true(is_by_time(z))
    expect_true(is_by_sex(z))

    z <- subset_sex(dcc_df_time_age_sex, sex = "female", drop = FALSE)
    expect_true(is_by_sex(z))
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))

    z <- subset_time(dcc_df_time_age_sex, time = 1950:1960)
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_age(dcc_df_time_age_sex, age = 0:10)
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))

    z <- subset_sex(dcc_df_time_age_sex, sex = c("female", "male"))
    expect_true(is_by_time(z))
    expect_true(is_by_age(z))
    expect_true(is_by_sex(z))
})


test_that("method for 'subset' works as expected", {
    expect_is(subset(dcc_df_time_age_sex, age_start == 0), class = "demog_change_component_df")
    expect_is(subset(dcc_df_time_age_sex, age_start == 0 & time_start == 1950),
              class = "demog_change_component_df")

    expect_warning(expect_is(subset(dcc_df_time_age_sex, value < 0), class = "data.frame"),
                   "Subset result is not a valid 'demog_change_component_df'")
})


test_that("subsetting errors are caught", {

    expect_error(subset_time(dcc_df_time_age_sex, time = "a"),
                 "is.finite")
    expect_error(subset_age(dcc_df_time_age_sex, age = "a"),
                 "is.finite")
    expect_error(subset_sex(dcc_df_time_age_sex, sex = 1),
                 "character")

    expect_error(subset_time(subset_time(dcc_df_time_age_sex, time = 1950:1960), 1970),
                 "does not have any entries with")
    expect_error(subset_age(subset_age(dcc_df_time_age_sex, age = 0:10), 11),
                 "does not have any entries with")

    expect_error(subset_sex(subset_sex(dcc_df_time_age_sex, sex = "male"), sex = "female"),
                 "is_by_sex")
    expect_error(subset_time(subset_time(dcc_df_time_age_sex, time = 1950), time = 1960),
                 "is_by_time")
    expect_error(subset_age(subset_age(dcc_df_time_age_sex, age = 10), age = 20),
                 "is_by_age")
})


test_that("'age_time_matrix's are created properly", {
    expect_error(as_age_time_matrix(dcc_df_time_age_sex),
                 "'x' has dimension \"sex\"; select a single sex")
    expect_is(as_age_time_matrix(subset_sex(dcc_df_time_age_sex, sex = "female")),
              "matrix")
    })
