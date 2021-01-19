context("Test methods for S3 class 'death_probability_age_sex'")

### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS

death_probability_input_df_time_age_sex <-
    subset(wpp_input_example$life_table_age_sex,
           indicator == "lt_nqx", select = -indicator)


test_that("valid member created", {
    expect_s3_class(death_probability_age_sex(death_probability_input_df_time_age_sex),
                    "death_probability_age_sex")
})


### MAKE OBJECTS AVAILABLE TO REMAINDER OF TESTS (already tested)

death_probability_input_df_time_age_sex <-
    subset(wpp_input_example$life_table_age_sex,
           indicator == "lt_nqx", select = -indicator)

life_table_input_df_indicator_time_age_sex <-
    life_table_age_sex(wpp_input_example$life_table_age_sex)


test_that("successfully extracted from 'life_table...' object", {
    expect_s3_class(death_probability_component(
        life_table_input_df_indicator_time_age_sex),
        "death_probability_age_sex")
    })


test_that("Non-zero age detected", {
    y <- death_probability_input_df_time_age_sex
    z <- subset(y, age_start > 0)
    z <- ccmppWPP:::new_death_probability_age_sex(ccmppWPP:::sort_demog_change_component_df(z),
                                     age_span = age_span(death_probability_age_sex(y)),
                                     time_span = time_span(death_probability_age_sex(y)))
    expect_error(validate_ccmpp_object(z),
                 "'age_start' does not start at '0'")
})


test_that("Required dimensions enforced", {
    x <- death_probability_input_df_time_age_sex
    expect_error(death_probability_age_sex(subset(as.data.frame(x),
                                        select =
                                            -c(time_span, time_start))),
                 "must have columns")
})


test_that("Invalid 'value's are caught", {
    x <- death_probability_input_df_time_age_sex
    x[1, "value"]  <- -1
    expect_error(death_probability_age_sex(x),
                 "'value_type' is 'proportion' but values less than 0 or greater than 1 are present")

    x <- death_probability_input_df_time_age_sex
    x[1, "value"]  <- 2
    expect_error(death_probability_age_sex(x),
                 "'value_type' is 'proportion' but values less than 0 or greater than 1 are present")
})


test_that("Equal sex values are detected", {
    x <- death_probability_input_df_time_age_sex

    y <- x
    y[y$sex == "male", "value"] <- y[y$sex == "female", "value"]
    expect_warning(death_probability_age_sex(y),
                   "Female and male mortality probabilities are identical")

    y <- x
    y[y$sex == "male", "value"] <-
        y[y$sex == "female", "value"] + runif(n = nrow(y[y$sex == "female",]),
                                              min = 1e-6, max = 1e-5)
    y[y$sex == "male" & y$value < 0, "value"] <- 0
    y[y$sex == "male" & y$value > 1, "value"] <- 1
    expect_warning(death_probability_age_sex(y),
                   "Female and male mortality probabilities are very similar")

    })
