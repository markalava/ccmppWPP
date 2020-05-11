context("Compare R and Cpp versions")

test_that("the female and male projected populations are equal under 'NULL' projection conditions.", {

data(sweden_1993, package = "ccmppWPP")

    ccmpp_r_mig_even <-
        project_z_by_z(z=sweden_1993$n, P0M=sweden_1993$P0M, P0F=sweden_1993$P0F,
                       SxM=sweden_1993$SxM, SxF=sweden_1993$SxF, asfr=sweden_1993$asfr,
                       NMxM=sweden_1993$NMxM, NMxF=sweden_1993$NMxF, srb=sweden_1993$srb,
                       mig_assumption = "even")

    ccmpp_cpp_mig_even <-
        proj_pop_cpp(step_size = sweden_1993$n,
                      pop_count_age_m_t0 = sweden_1993$P0M,
                      pop_count_age_f_t0 = sweden_1993$P0F,
                      surv_prop_age_m = sweden_1993$SxM,
                      surv_prop_age_f = sweden_1993$SxF,
                      fert_rate_age = sweden_1993$asfr,
                      net_mig_count_age_m = sweden_1993$NMxM,
                      net_mig_count_age_f = sweden_1993$NMxF,
                     srb_tot = sweden_1993$srb,
                     mig_assumption = "even")

    expect_equal(round(ccmpp_r_mig_even$Bx, 0), round(ccmpp_cpp_mig_even$birth_count_age_b, 0))
    expect_equal(round(ccmpp_r_mig_even$PzF, 0), round(ccmpp_cpp_mig_even$pop_count_age_f_t1, 0))
    expect_equal(round(ccmpp_r_mig_even$PzM, 0), round(ccmpp_cpp_mig_even$pop_count_age_m_t1, 0))

})
