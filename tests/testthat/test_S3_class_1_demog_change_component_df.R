test_that("objects are created properly", {

### Time, Age, Sex
    ## Specify dimensions
    y <- demog_change_component_df(S3_demog_change_component_time_age_sex_test_df,
                       dimensions = c("time", "age", "sex"))
    expect_s3_class(y, "demog_change_component_df")
    expect_s3_class(y, "data.frame")
    expect_true(setequal(demog_change_component_dims(y), c("time", "age", "sex")))
    expect_true(identical(names(demog_change_component_attributes(y)),
                          c("dimensions", "value_type", "value_scale")))

    ## Guess dimensions
    y <- demog_change_component_df(S3_demog_change_component_time_age_sex_test_df)
    expect_s3_class(y, "demog_change_component_df")
    expect_s3_class(y, "data.frame")
    expect_true(setequal(demog_change_component_dims(y), c("time", "age", "sex")))
    expect_true(identical(names(demog_change_component_attributes(y)),
                          c("dimensions", "value_type", "value_scale")))

    ## Guess spans
    y <- demog_change_component_df(
        subset(S3_demog_change_component_time_age_sex_test_df,
               select = -c(time_span, age_span)))
    expect_s3_class(y, "demog_change_component_df")
    expect_s3_class(y, "data.frame")
    expect_true(setequal(demog_change_component_dims(y), c("time", "age", "sex")))
    expect_true(identical(names(demog_change_component_attributes(y)),
                          c("dimensions", "value_type", "value_scale")))

### Time, Age
    ## Specify dimensions
    x <- S3_demog_change_component_time_age_test_df[, c("time_start", "age_start",
                                                        "time_span", "age_span",
                                                        "value")]
    z <- demog_change_component_df(x,
                       dimensions = c("time", "age"))
    expect_s3_class(z, "demog_change_component_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), c("time", "age")))
    expect_true(identical(names(demog_change_component_attributes(z)),
                          c("dimensions", "value_type", "value_scale")))

    ## Guess dimensions and spans
    x <- S3_demog_change_component_time_age_test_df[, c("time_start", "age_start", "value")]
    z <- demog_change_component_df(x)
    expect_s3_class(z, "demog_change_component_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), c("time", "age")))
    expect_true(identical(names(demog_change_component_attributes(z)),
                          c("dimensions", "value_type", "value_scale")))

    ## Remove un-wanted dimensions
    x <- subset_sex(
        demog_change_component_df(
            S3_demog_change_component_time_age_sex_test_df),
        "male", drop = FALSE)
    y <- demog_change_component_df(as.data.frame(x), dimensions = c("time", "age"))
    expect_setequal(demog_change_component_dims(y), c("time", "age"))

### Time, Sex
    ## specify dimensions
    x <- S3_demog_change_component_time_sex_test_df[, c("time_start",
                                                        "time_span", "sex", "value")]
    z <- demog_change_component_df(x, dimensions = c("time", "sex"))
    expect_s3_class(z, "demog_change_component_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), c("time", "sex")))
    expect_true(identical(names(demog_change_component_attributes(z)),
                          c("dimensions", "value_type", "value_scale")))

    ## Guess dimensions and spans
    x <- S3_demog_change_component_time_sex_test_df[,
                             c("time_start", "time_span", "sex", "value")]
    z <- demog_change_component_df(x)
    expect_s3_class(z, "demog_change_component_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), c("time", "sex")))
    expect_true(identical(names(demog_change_component_attributes(z)),
                          c("dimensions", "value_type", "value_scale")))

### Time
    ## Specify dimensions
    x <- S3_demog_change_component_time_test_df[, c("time_start", "time_span", "value")]
    z <- demog_change_component_df(x, dimensions = "time")
    expect_s3_class(z, "demog_change_component_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), "time"))
    expect_true(identical(names(demog_change_component_attributes(z)),
                          c("dimensions", "value_type", "value_scale")))

    x <- S3_demog_change_component_time_test_df[, c("time_start", "time_span", "value")]
    z <- demog_change_component_df(x)
    expect_s3_class(z, "demog_change_component_df")
    expect_s3_class(z, "data.frame")
    expect_true(setequal(demog_change_component_dims(z), "time"))
    expect_true(identical(names(demog_change_component_attributes(z)),
                          c("dimensions", "value_type", "value_scale")))
})


test_that("invalid data objects are caught", {

    x <- S3_demog_change_component_time_age_sex_test_df

    expect_error(demog_change_component_df(as.list(x),
                       dimensions = c("time", "age", "sex")),
                 "not a data.frame")

    expect_error(demog_change_component_df(as.matrix(x),
                       dimensions = c("time", "age", "sex")),
                 "not a data.frame")

    expect_error(demog_change_component_df(data.matrix(x),
                       dimensions = c("time", "age", "sex")),
                 "not a data.frame")
})


test_that("missing columns are caught", {

    x <- S3_demog_change_component_time_age_sex_test_df

    must_have <-
        "must have columns 'time_start', 'sex', 'age_start', 'value'"

    expect_error(demog_change_component_df(x[, c("sex",
                                                  "age_start", "value")],
                       dimensions = c("time", "age", "sex")),
                 must_have)

    expect_error(demog_change_component_df(x[, c("time_start",
                                                  "age_start", "value")],
                       dimensions = c("time", "age", "sex")),
                 must_have)

    expect_error(demog_change_component_df(x[, c("time_start", "sex",
                                                  "value")],
                       dimensions = c("time", "age", "sex")),
                 must_have)

    expect_error(demog_change_component_df(x[, c("time_start", "sex",
                                                  "age_start")],
                       dimensions = c("time", "age", "sex")),
                 must_have)
})


test_that("superfluous columns are caught", {

    x <- S3_demog_change_component_time_age_sex_test_df
    z <- data.frame(x, source = "census")

    expect_true(## No fail: Automatically removes column
        !("source" %in%
          colnames(demog_change_component_df(z,
                       dimensions = c("time", "age", "sex")))))

    y <- ccmppWPP:::new_demog_change_component_df(z[,
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

    expect_true(## No fail: Automatically removes column bc 'indicator' not in dimensions.
        !("indicator" %in%
          colnames(demog_change_component_df(z,
                       dimensions = c("time", "age", "sex")))))

    expect_true(## No fail: Keeps column
        "indicator" %in%
          colnames(demog_change_component_df(z,
                       dimensions = c("time", "age", "sex", "indicator"))))

    expect_true(## No fail: Keeps column
        "indicator" %in%
          colnames(demog_change_component_df(z)))

    y <- ccmppWPP:::new_demog_change_component_df(z[,
                             c(ccmppWPP:::get_all_req_col_names_for_dimensions(
                                             dimensions =
                                                 c("age", "time", "sex", "indicator")))],
                             value_type = "real", value_scale = 1,
                       dimensions = c("time", "age", "sex", "indicator"))
    expect_s3_class(validate_ccmppWPP_object(y), "demog_change_component_df")

    z <- transform(z, indicator = 84)
    expect_error(demog_change_component_df(z),
                 "Cannot coerce column 'indicator' to 'character'")
})

test_that("'NA's are allowed", {
    x <- S3_demog_change_component_time_age_sex_test_df
    x[, "value"] <- NA
    expect_warning(demog_change_component_df(x),
                   "All 'value' entries are 'NA'")

    x <- S3_demog_change_component_time_age_sex_test_df
    x[1, "value"] <- NA
    expect_warning(demog_change_component_df(x),
                   "'value' column has some 'NA' entries")
    })


test_that("'subset_times' works as expected", {
    x <- demog_change_component_df(S3_demog_change_component_time_age_sex_test_df,
                              dimensions = c("time", "age", "sex"))

    expect_equal(times(subset_time(x, 1950:1960)),
                 1950:1960)
    expect_equal(times(subset_time(x, -(1950:1960))),
                 1961:max(times(x)))
    expect_equal(times(subset_time(x, 1950:1960, include = FALSE)),
                 1961:max(times(x)))
    expect_error(subset_time(x, -(1950:1960), include = FALSE),
                 "Negative 'times' can only be used with 'include = TRUE'")
    expect_error(subset_time(x, c(-1950, 1951)),
                 "Either supply all positive or all negative values for 'times'")
    })


test_that("'subset_ages' works as expected", {
    x <- demog_change_component_df(S3_demog_change_component_time_age_sex_test_df,
                              dimensions = c("time", "age", "sex"))

    expect_equal(ages(subset_age(x, 0:11)),
                 0:11)
    expect_equal(ages(subset_age(x, -(0:11))),
                 12:max(ages(x)))
    expect_equal(ages(subset_age(x, 0:11, include = FALSE)),
                 12:max(ages(x)))
    expect_error(subset_age(x, -(0:11), include = FALSE),
                 "Negative 'ages' can only be used with 'include = TRUE'")
    expect_error(subset_age(x, c(-1, 2)),
                 "Either supply all positive or all negative values for 'ages'")
    })


test_that("'subset_sexes' works as expected", {
    x <- demog_change_component_df(S3_demog_change_component_time_age_sex_test_df,
                              dimensions = c("time", "age", "sex"))

    expect_equal(sexes(subset_sex(x, "male")), "male")
    expect_equal(sexes(subset_sex(x, "male", include = FALSE)), "female")
    expect_error(subset_sex(x, c("female", "male"), include = FALSE),
                 "'x' does not have any entries; you have excluded all rows.")
    })


test_that("'subset_indicators' works as expected", {
    x <- demog_change_component_df(rbind(
        data.frame(demog_change_component_df(S3_demog_change_component_time_age_sex_test_df,
                              dimensions = c("time", "age", "sex")), indicator = "test_1"),
        data.frame(demog_change_component_df(S3_demog_change_component_time_age_sex_test_df,
                              dimensions = c("time", "age", "sex")), indicator = "test_2")))

    expect_equal(indicators(subset_indicator(x, "test_1")), "test_1")
    expect_equal(indicators(subset_indicator(x, "test_1", include = FALSE)), "test_2")
    expect_error(subset_indicator(x, c("test_1", "test_2"), include = FALSE),
                 "'x' does not have any entries; you have excluded all rows.")
    })

