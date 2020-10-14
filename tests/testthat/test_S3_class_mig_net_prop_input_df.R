context("Test methods for S3 class 'mig_net_prop_age_sex'")

## test_that("valid member created", {
##     skip("This is actually done in 'test_S3_class_mig_net_count_input.R'")

## })

test_that("subsetting works", {
    x <- ccmpp_input_list_example
    x <- mig_net_prop_age_sex(x)

    y <- subset_sex(x, "female")
    expect_s3_class(y, "mig_net_prop_age_sex")
    expect_identical(demog_change_component_dims(y),
                     c("time", "sex", "age"))

    ## Cannot work because the class 'mig_net_prop_age_sex' has to have a 'sex' dimension.
    expect_error(subset_sex(x, "female", drop = TRUE),
                 "'x' must have columns 'time_start', 'sex', 'age_start', 'value'; some or all are missing")
    })
