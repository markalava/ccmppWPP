context("Test methods for S3 class 'life_table_input_df'")

test_that("valid member created", {
    expect_s3_class(life_table_input_df_indicator_time_age_sex,
                    "life_table_input_df")
    })


test_that("Non-zero age detected", {
    y <- life_table_input_df_indicator_time_age_sex
    z <- subset(y, age_start > 0)
    z <- new_life_table_input_df(z,
                                     age_span = age_span(y),
                                     time_span = time_span(y))
    expect_error(validate_ccmpp_object(z),
                 "'age_start' does not start at '0'")
})


test_that("Required dimensions enforced", {
    x <- life_table_input_df_indicator_time_age_sex
    expect_error(life_table_input_df(subset(as.data.frame(x),
                                        select =
                                            -c(time_span, time_start))),
                 "must have columns")
})


test_that("Value categories enforced", {
    x <- life_table_input_df_indicator_time_age_sex
    x[x$indicator == "lt_ex", "indicator"] <- "foo"
    expect_error(life_table_input_df(x),
                 "only values allowed in the 'indicator' column")
})
