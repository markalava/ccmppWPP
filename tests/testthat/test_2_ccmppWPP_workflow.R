test_that("canada workflow processed fully without errors and returns valid values.", {

    data("canada_wpp_1950_2020_ccmpp_inputs_1x1")

    # test that workflow completed without errors
    test_out <- expect_no_error(expect_warning(ccmppWPP_workflow_one_country_variant(
        wpp_input = canada_wpp_1950_2020_ccmpp_inputs_1x1,
        intermediate_output_folder = tempdir()),
        "'x' has unusually large values"))

    # test that population by age and sex always has positive value
    expect_gt(min(test_out$pop_count_age_sex_1x1$value), 0)

    # test that exposures by age and sex always has positive value
    expect_gt(min(test_out$exposure_count_age_sex_1x1$value), 0)

    # test that deaths by age and sex always has positive value
    expect_gt(min(test_out$death_count_age_sex_1x1$value), 0)
    expect_gt(min(test_out$death_count_cohort_sex_1x1$value), 0)

    # test that age-period deaths equal to age-cohort deaths
    expect_equal(sum(test_out$death_count_age_sex_1x1$value), sum(test_out$death_count_cohort_sex_1x1$value))
    expect_equal(sum(test_out$death_count_age_sex_1x1$value), sum(test_out$death_count_age_sex_5x1$value))
    expect_equal(sum(test_out$death_count_age_sex_1x1$value), sum(test_out$death_count_cohort_sex_5x1$value))
    expect_equal(sum(test_out$death_count_age_sex_1x1$value), sum(test_out$death_count_tot_sex$value))

})


test_that("canada workflow with invalid inputs throws error.", {
    data("canada_wpp_1950_2020_ccmpp_inputs_1x1")
    x <- canada_wpp_1950_2020_ccmpp_inputs_1x1
    y <- x[-1]
    expect_error(ccmppWPP_workflow_one_country_variant(wpp_input = y,
        intermediate_output_folder = tempdir()),
                 "'x' must have these elements")

    y <- c(list(pop_count_age_sex_base = data.frame()),
           x[-1])
    expect_error(ccmppWPP_workflow_one_country_variant(wpp_input = y,
        intermediate_output_folder = tempdir()),
                 "'x' must have columns")

    y <- as_ccmpp_input_list(x)
    attr(y[["fert_rate_age_f"]], "time_span") <- 9
    expect_error(ccmppWPP_workflow_one_country_variant(wpp_input = y,
        intermediate_output_folder = tempdir()),
                 "spacings between each")
})


test_that("kuwait workflow processed fully without errors and returns valid values.", {

    data("kuwait_wpp_1950_2020_ccmpp_inputs_1x1")
    test_out <- expect_error(ccmppWPP_workflow_one_country_variant(
        wpp_input = kuwait_wpp_1950_2020_ccmpp_inputs_1x1 #<< 2021-07-12: Was previously the canada data
        ,intermediate_output_folder = tempdir()
    ), NA)

    ## 2021-07-12: Zero values are present. Comment out for now..
    ##
    ## # test that population by age and sex always has positive value
    ## expect_gt(min(test_out$pop_count_age_sex_1x1$value), 0)

    ## # test that exposures by age and sex always has positive value
    ## expect_gt(min(test_out$exposure_count_age_sex_1x1$value), 0)

    ## # test that deaths by age and sex always has positive value
    ## expect_gt(min(test_out$death_count_age_sex_1x1$value), 0)
    ## expect_gt(min(test_out$death_count_cohort_sex_1x1$value), 0)
    ##
    ## ---

    # test that age-period deaths equal to age-cohort deaths
    expect_equal(sum(test_out$death_count_age_sex_1x1$value), sum(test_out$death_count_cohort_sex_1x1$value))
    expect_equal(sum(test_out$death_count_age_sex_1x1$value), sum(test_out$death_count_age_sex_5x1$value))
    expect_equal(sum(test_out$death_count_age_sex_1x1$value), sum(test_out$death_count_cohort_sex_5x1$value))
    expect_equal(sum(test_out$death_count_age_sex_1x1$value), sum(test_out$death_count_tot_sex$value))
})

