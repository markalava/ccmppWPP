context("Test methods for S3 class 'pop_count_age_sex_base'")

test_that("valid member created", {
    expect_s3_class(pop_count_base_input_df_time_age_sex,
                    "pop_count_age_sex_base")
    })


test_that("Non-zero age detected", {
    y <- pop_count_base_input_df_time_age_sex
    z <- subset(y, age_start > 0)
    z <- ccmppWPP:::new_pop_count_age_sex_base(z,
                                     age_span = age_span(y),
                                     time_span = time_span(y))
    expect_error(validate_ccmpp_object(z),
                 "'age_start' does not start at '0'")
})


test_that("More than one time period detected", {
    y <- pop_count_base_input_df_time_age_sex
    w <- transform(y, time_start = times(y) + 1)
    z <- ccmppWPP:::new_pop_count_age_sex_base(sort_demog_change_component_df(rbind(y, w)),
                                     time_span = time_span(y),
                                     age_span = age_span(y))
    expect_error(validate_ccmpp_object(z),
                 "has more than one unique value")
    })
