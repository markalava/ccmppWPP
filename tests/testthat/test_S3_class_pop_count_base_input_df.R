context("Test methods for S3 class 'pop_count_base_input_df'")

test_that("valid member created", {
    expect_s3_class(pop_count_base_input_df_time_age_sex,
                    "pop_count_base_input_df")
    })


test_that("Non-zero age detected", {
    y <- pop_count_base_input_df_time_age_sex
    z <- subset(y, age_start > 0)
    z <- new_pop_count_base_input_df(z,
                                     age_span = age_span(y),
                                     time_span = time_span(y),
                                     dimensions = demog_change_component_dimensions(y))
    expect_error(validate_ccmpp_object(z),
                 "'age_start' does not start at '0'")
})


test_that("More than one time period detected", {
    y <- pop_count_base_input_df_time_age_sex
    w <- transform(y, time_start = times(y) + 1)
    z <- new_pop_count_base_input_df(sort_demog_change_component_df(rbind(y, w)),
                                     time_span = time_span(y),
                                     age_span = age_span(y),
                                     dimensions = demog_change_component_dimensions(y))
    expect_error(validate_ccmpp_object(z),
                 "has more than one unique value")
    })
