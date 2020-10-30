context("Test methods for S3 class 'fert_rate_age_f'")

### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS (already tested)

fert_rate_input_df_time_age <-
    fert_rate_age_f(S3_fert_rate_time_age_df)



test_that("valid member created", {
    expect_s3_class(fert_rate_input_df_time_age,
                    "fert_rate_age_f")
    })


test_that("subsetting drops class", {

    y <- fert_rate_input_df_time_age

    ## NB Warning only issued if run from top level
    expect_not_s3_class(y[, "age_start"], "fert_rate_age_f")
    expect_not_s3_class(y$age_start, "fert_rate_age_f")
    expect_not_s3_class(y[["age_start"]], "fert_rate_age_f")

    expect_not_s3_class(y[1:nrow(y), colnames(y)], "fert_rate_age_f")
})


test_that("subset-replacement drops class", {
    z <- fert_rate_input_df_time_age
    z[, "age_start"] <- z$age_start
    expect_not_s3_class(z, "fert_rate_age_f")
    z <- fert_rate_input_df_time_age
    z$age_start <- z$age_start
    expect_not_s3_class(z, "fert_rate_age_f")
    z <- fert_rate_input_df_time_age
    z[["age_start"]] <- z$age_start
    expect_not_s3_class(z, "fert_rate_age_f")
})


test_that("printing returns an object of class 'fert_rate_age_f'", {
    capture.output(x <- print(fert_rate_input_df_time_age),
                   file = OS_null_file_string)
    expect_s3_class(x, "fert_rate_age_f")

    capture.output(x <- print(fert_rate_input_df_time_age, print_what = "info"),
                   file = OS_null_file_string)
    expect_s3_class(x, "fert_rate_age_f")

    capture.output(x <- print(fert_rate_input_df_time_age, print_what = "table"),
                   file = OS_null_file_string)
    expect_s3_class(x, "fert_rate_age_f")
    })


test_that("printing works if there are no non-zero fertility rate ages", {
    y <- fert_rate_input_df_time_age
    non_zero_fert_ages(y) <- numeric(0)
    capture.output(y <- print(y),
                   file = OS_null_file_string)
    expect_s3_class(y, "fert_rate_age_f")
    })


test_that("printing works if there are no zero fertility rate ages", {
    y <- fert_rate_input_df_time_age
    expect_warning(non_zero_fert_ages(y) <- ages(y),
                   "The non-zero fertility age range is being expanded beyond the previous range")

    capture.output(x <- print(y), file = OS_null_file_string)
    expect_s3_class(x, "fert_rate_age_f")

    capture.output(x <- print(y, print_what = "info"),
                   file = OS_null_file_string)
    expect_s3_class(x, "fert_rate_age_f")

    capture.output(x <- print(y, print_what = "table"),
                   file = OS_null_file_string)
    expect_s3_class(x, "fert_rate_age_f")
    })

