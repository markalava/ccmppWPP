context("Test that project_abacus_z_by_z function returns WPP 2019 5x5 results")

test_that("Function returns Mexico 2000-2005 projection with end-period migration assumption", {

    data(mexico_wpp_2000_2005)

    abacus_r_mig <-
        project_abacus_z_by_z(z = 5, 
                       pop_count_age_m_begin = mexico_wpp_2000_2005$pop_count_age_m_begin, 
                       pop_count_age_f_begin = mexico_wpp_2000_2005$pop_count_age_f_begin,
                       survival_ratio_age_m = mexico_wpp_2000_2005$survival_ratio_age_m,
                       survival_ratio_age_f = mexico_wpp_2000_2005$survival_ratio_age_f,
                       fert_rate_age_f = mexico_wpp_2000_2005$fert_rate_age_f,
                       srb = mexico_wpp_2000_2005$srb,
                       mig_net_count_age_m = mexico_wpp_2000_2005$mig_net_count_age_m,
                       mig_net_count_age_f = mexico_wpp_2000_2005$mig_net_count_age_f,
                       mig_assumption = "end")
    expect_that(max(abs(abacus_r_mig$pop_count_age_f_end-mexico_wpp_2000_2005$pop_count_age_f_end)), is_less_than(30))
    expect_that(max(abs(abacus_r_mig$pop_count_age_m_end-mexico_wpp_2000_2005$pop_count_age_m_end)), is_less_than(30))
    }) # we don't expect results to match to the person because of differences in rounding

test_that("Function returns Kuwait 2010-2015 projection with even-over-period migration assumption", {
    
    data(kuwait_wpp_2010_2015)
    
    abacus_r_mig <-
        project_abacus_z_by_z(z = 5, 
                              pop_count_age_m_begin = kuwait_wpp_2010_2015$pop_count_age_m_begin, 
                              pop_count_age_f_begin = kuwait_wpp_2010_2015$pop_count_age_f_begin,
                              survival_ratio_age_m = kuwait_wpp_2010_2015$survival_ratio_age_m,
                              survival_ratio_age_f = kuwait_wpp_2010_2015$survival_ratio_age_f,
                              fert_rate_age_f = kuwait_wpp_2010_2015$fert_rate_age_f,
                              srb = kuwait_wpp_2010_2015$srb,
                              mig_net_count_age_m = kuwait_wpp_2010_2015$mig_net_count_age_m,
                              mig_net_count_age_f = kuwait_wpp_2010_2015$mig_net_count_age_f,
                              mig_assumption = "even")
    expect_that(max(abs(abacus_r_mig$pop_count_age_f_end-kuwait_wpp_2010_2015$pop_count_age_f_end)), is_less_than(3))
    expect_that(max(abs(abacus_r_mig$pop_count_age_m_end-kuwait_wpp_2010_2015$pop_count_age_m_end)), is_less_than(3))
}) # we don't expect results to match to the person because of differences in rounding



