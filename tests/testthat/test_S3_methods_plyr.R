context("Methods for plyr functions")

test_that("Classes and attributes are dropped by plyr functions", {
    ## Only a few .. too many to test all!
    if (!requireNamespace("plyr", quietly = TRUE)) skip("plyr not installed.")

    x <- plyr::arrange(dcc_df_time_age_sex, time_start)
                                # no warning bc plyr fns already drop
                                # class so no S3 method.
    expect_not_s3_class(x, "demog_change_component_df")

    x <- plyr::mutate(dcc_df_time_age_sex, time_start = time_start)
    expect_not_s3_class(x, "demog_change_component_df")

    x <- plyr::ddply(dcc_df_time_age_sex,
                     .variables = "sex", .fun = "transform", mean_age = mean(age_start))
    expect_not_s3_class(x, "demog_change_component_df")
    })
