context("Test methods for S3 class 'mig_net_prop_age_sex'")

test_that("valid member created", {
    x <- ccmpp_input_list_example
    expect_s3_class(mig_net_prop_age_sex(x),
                    "mig_net_prop_age_sex")

    pop_count_age_sex <-
        rbind(x$pop_count_age_sex_base,
              data_reshape_ccmpp_output(
                  project_ccmpp_loop_over_time(indata = x))$pop_count_age_sex)
    pop_count_age_sex <-
        pop_count_age_sex[pop_count_age_sex$sex %in% c("male", "female"),]

    expect_s3_class(mig_net_prop_age_sex(mig_net_count_input_df_time_age_sex,
                                         pop_count_age_sex),
                    "mig_net_prop_age_sex")

})

