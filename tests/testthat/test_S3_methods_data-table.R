context("Methods for data.table functions")

test_that("Classes and attributes are dropped by data.table functions", {
    ## Only a few .. too many to test all!
    if (!requireNamespace("data.table", quietly = TRUE)) skip("data.table not installed.")

    x <- data.table::setDT(dcc_df_time_age_sex)
                                # no warning bc data.table fns already drop
                                # class so no S3 method.
    expect_not_s3_class(x, "demog_change_component_df")

    x <- data.table::as.data.table(dcc_df_time_age_sex)
                                # no warning bc data.table fns already drop
                                # class so no S3 method.
    expect_not_s3_class(x, "demog_change_component_df")
    })
