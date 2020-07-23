context("Conversion to age time matrices")


test_that("'age_time_matrix's are created properly", {
    expect_error(as_age_time_matrix(dcc_df_time_age_sex),
                 "'x' has dimension \"sex\"; select a single sex")
    expect_is(as_age_time_matrix(subset_sex(dcc_df_time_age_sex, sexes = "female", drop = TRUE)),
              "matrix")
    })


test_that("age * time matrices are produced", {
    expect_is(as_age_time_matrix(mig_net_count_tot_input_df_time),
                 "matrix")
    expect_is(as_age_time_matrix(fert_rate_input_df_time_age),
                 "matrix")
    expect_is(as_age_time_matrix(fert_rate_input_df_time_age,
                                 drop_zero_fert_ages = TRUE),
              "matrix")

    expect_error(as_age_time_matrix(ccmpp_input_df_time_age_sex),
                 "select a single sex")
    expect_is(as_age_time_matrix(subset_sex(ccmpp_input_df_time_age_sex,
                                            "female", drop = TRUE)),
              "matrix")

    expect_error(as_age_time_matrix(mig_parameter_input_df_indicator_time),
                 "select a single indicator")
})

test_that("age * time matrix lists are produced", {
    expect_is(as_age_time_matrix_list(ccmpp_input_list_example),
              "list")
    })
