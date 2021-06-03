
### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS

death_count_input_df_time_age_sex <-
    subset(wpp_input_example$life_table_age_sex,
           indicator == "lt_ndx", select = -indicator)


test_that("valid member created", {
    expect_s3_class(death_count_age_sex(death_count_input_df_time_age_sex),
                    "death_count_age_sex")
})


### MAKE OBJECTS AVAILABLE TO REMAINDER OF TESTS (already tested)

death_count_input_df_time_age_sex <-
    subset(wpp_input_example$life_table_age_sex,
           indicator == "lt_ndx", select = -indicator)

life_table_input_df_indicator_time_age_sex <-
    life_table_age_sex(wpp_input_example$life_table_age_sex)


test_that("successfully extracted from 'life_table...' object", {
    expect_s3_class(death_count_age_sex(
        life_table_input_df_indicator_time_age_sex),
        "death_count_age_sex")
    })


test_that("Non-zero age detected", {
    y <- death_count_input_df_time_age_sex
    z <- subset(y, age_start > 0)
    z <- ccmppWPP:::new_death_count_age_sex(ccmppWPP:::sort_demog_change_component_df(z),
                                     age_span = age_span(death_count_age_sex(y)),
                                     time_span = time_span(death_count_age_sex(y)),
                                     value_scale = 1)
    expect_error(validate_ccmppWPP_object(z),
                 "'age_start' does not start at '0'")
})


test_that("Required dimensions enforced", {
    x <- death_count_input_df_time_age_sex
    expect_error(death_count_age_sex(subset(as.data.frame(x),
                                        select =
                                            -c(time_span, time_start))),
                 "must have columns")
})


test_that("Invalid 'value's are caught", {
    x <- death_count_input_df_time_age_sex
    x[1, "value"]  <- -1
    expect_error(death_count_age_sex(x),
                 "'value' column has elements < 0")
})


test_that("Equal sex values are detected", {
    x <- death_count_input_df_time_age_sex

    y <- x
    y[y$sex == "male", "value"] <- y[y$sex == "female", "value"]
    expect_warning(death_count_age_sex(y),
                   "Female and male death counts are identical")

    y <- x
    y[y$sex == "male", "value"] <-
        y[y$sex == "female", "value"] + runif(n = nrow(y[y$sex == "female",]),
                                              min = 1e-6, max = 1e-5)
    expect_warning(death_count_age_sex(y),
                   "Female and male death counts are very similar")

    })
