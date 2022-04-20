

### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS

pop_count_base_input_df_time_age_sex <-
    pop_count_age_sex_base(wpp_input_example$pop_count_age_sex_base)


test_that("subsetting drops class", {

    y <- pop_count_base_input_df_time_age_sex

    ## NB Warning only issued if run from top level
    expect_not_s3_class(y[, "age_start"], "pop_count_age_sex_base")
    expect_not_s3_class(y$age_start, "pop_count_age_sex_base")
    expect_not_s3_class(y[["age_start"]], "pop_count_age_sex_base")

    expect_not_s3_class(y[1:nrow(y), colnames(y)], "pop_count_age_sex_base")
})


test_that("subset-replacement drops class", {
    z <- pop_count_base_input_df_time_age_sex
    z[, "age_start"] <- z$age_start
    expect_not_s3_class(z, "pop_count_age_sex_base")
    z <- pop_count_base_input_df_time_age_sex
    z$age_start <- z$age_start
    expect_not_s3_class(z, "pop_count_age_sex_base")
    z <- pop_count_base_input_df_time_age_sex
    z[["age_start"]] <- z$age_start
    expect_not_s3_class(z, "pop_count_age_sex_base")
})

