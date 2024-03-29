

test_that("valid member created", {
    expect_s3_class(life_table_age_sex(wpp_input_example$life_table_age_sex),
                    "life_table_age_sex")
    })

### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS (already tested)

life_table_input_df_indicator_time_age_sex <-
    life_table_age_sex(wpp_input_example$life_table_age_sex)



test_that("Non-zero age detected", {
    y <- life_table_input_df_indicator_time_age_sex
    z <- y[y$age_start > 0,]
    z <- ccmppWPP:::new_life_table_age_sex(z, value_scale = 1,
                                     age_span = age_span(y),
                                     time_span = time_span(y))
    expect_error(validate_ccmppWPP_object(z))
})


test_that("Required dimensions enforced", {
    x <- life_table_input_df_indicator_time_age_sex
    expect_error(life_table_age_sex(subset(as.data.frame(x),
                                        select =
                                            -c(time_span, time_start))),
                 "must have columns")
})


test_that("Value categories enforced", {
    x <- life_table_input_df_indicator_time_age_sex
    x[x$indicator == "lt_ex", "indicator"] <- "foo"
    expect_error(life_table_age_sex(x),
                 "'indicator' column must contain all of the following")
})


test_that("Invalid 'value's are caught", {
    x <- life_table_input_df_indicator_time_age_sex
    x[1, "value"]  <- -1
    expect_error(life_table_age_sex(x,
                                    value_scale = value_scale(
                                        life_table_input_df_indicator_time_age_sex)),
                 "'value' column has negative elements")
})


test_that("Equal sex values are detected", {
    x <- life_table_input_df_indicator_time_age_sex
    y <- x
    y[y$sex == "male", "value"] <- y[y$sex == "female", "value"]
    expect_warning(life_table_age_sex(y),
                   "Female and male life table quantities are identical")

    y <- x
    y[y$sex == "male", "value"] <- y[y$sex == "female", "value"] + 1e-5
    expect_warning(life_table_age_sex(y),
                   "Female and male life table quantities are very similar")
})


test_that("'death_count_age_sex' component can be changed", {
    z <- life_table_input_df_indicator_time_age_sex
    x <- death_count_age_sex(z)
    expect_s3_class(x, "death_count_age_sex")
    expect_error(death_count_age_sex(z) <- x, NA)
})


test_that("'death_probability_age_sex' component can be changed", {
    z <- life_table_input_df_indicator_time_age_sex
    x <- death_probability_age_sex(z)
    expect_s3_class(x, "death_probability_age_sex")
    expect_error(death_probability_age_sex(z) <- x, NA)
})


test_that("'mort_rate_age_sex' component can be changed", {
    z <- life_table_input_df_indicator_time_age_sex
    x <- mort_rate_age_sex(z)
    expect_s3_class(x, "mort_rate_age_sex")
    expect_error(mort_rate_age_sex(z) <- x, NA)
})


test_that("'survival_ratio_age_sex' component can be changed", {
    z <- life_table_input_df_indicator_time_age_sex
    x <- survival_ratio_age_sex(z)
    expect_s3_class(x, "survival_ratio_age_sex")
    expect_error(survival_ratio_age_sex(z) <- x, NA)
})
