context("Test that ccmppWPP_workflow_one_country_variant runs without errors and produces valid values.")

test_that("canada workflow processed fully without errors and returns valid values.", {

    data("canada_wpp_1950_2020_ccmpp_inputs_1x1")
    
    # test that workflow completed without errors
    test_out <- expect_error(ccmppWPP_workflow_one_country_variant(wpp_input = canada_wpp_1950_2020_ccmpp_inputs_1x1), NA)
    
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


test_that("kuwait workflow processed fully without errors and returns valid values.", {

    data("kuwait_wpp_1950_2020_ccmpp_inputs_1x1")
    test_out <- expect_error(ccmppWPP_workflow_one_country_variant(wpp_input = canada_wpp_1950_2020_ccmpp_inputs_1x1), NA)
    
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

