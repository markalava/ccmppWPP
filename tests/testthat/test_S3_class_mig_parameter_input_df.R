context("Test methods for S3 class 'mig_parameter_input_df'")

test_that("valid member created", {
    expect_s3_class(mig_parameter_input_df_indicator_time,
                    "mig_parameter_input_df")
    })


test_that("Required dimensions enforced", {
    x <- mig_parameter_input_df_indicator_time
    expect_error(mig_parameter_input_df(subset(as.data.frame(x),
                                        select =
                                            -c(time_span, time_start))),
                 "must have columns")
})


test_that("Indicator categories enforced", {
    x <- mig_parameter_input_df_indicator_time
    x[x$indicator == "mig_type", "indicator"] <- "foo"
    expect_error(mig_parameter_input_df(x),
                 "only values allowed in the 'indicator' column")
})


test_that("Value categories enforced", {
    x <- mig_parameter_input_df_indicator_time
    x[x$value == "counts", "value"] <- "foo"
    expect_error(mig_parameter_input_df(x),
                 "only values allowed in the 'value' column")
})

