

test_that("valid member created", {
    expect_s3_class(mig_parameter(wpp_input_example$mig_parameter),
                    "mig_parameter")
})


### MAKE OBJECT AVAILABLE TO REMAINDER OF TESTS

mig_parameter_input_df_indicator_time <-
    mig_parameter(wpp_input_example$mig_parameter)


test_that("Required dimensions enforced", {
    x <- mig_parameter_input_df_indicator_time
    expect_error(mig_parameter(subset(as.data.frame(x),
                                        select =
                                            -c(time_span, time_start))),
                 "must have columns")
})


test_that("Indicator categories enforced", {
    x <- mig_parameter_input_df_indicator_time
    x[x$indicator == "mig_type", "indicator"] <- "foo"
    expect_error(mig_parameter(x),
                 "'indicator' column must contain all of the following")
})


test_that("Value categories enforced", {
    x <- mig_parameter_input_df_indicator_time
    x[x$value == "counts", "value"] <- "foo"
    expect_error(mig_parameter(x),
                 "only values allowed in the 'value' column")

    x <- mig_parameter_input_df_indicator_time
    x[x$value == "counts", "value"] <- "even"
    expect_s3_class(mig_parameter(x), "mig_parameter")

})

