context("Test that project_ccmpp_z_by_z function returns values known to be correct.")

test_that("the female and male projected populations are equal under 'NULL' projection conditions.", {

    data(sweden_1993_Preston_5x5)

    ## 'NULL' versions
    srb_0 <- 1
    fert_rate_age_f_0 <- rep(0, length(sweden_1993_Preston_5x5$pop_count_age_f_begin))
    mig_net_count_age_f_0 <- mig_net_count_age_m_0 <- rep(0, length(sweden_1993_Preston_5x5$pop_count_age_f_begin))
    survival_ratio_age_f_0 <- survival_ratio_age_m_0 <- rep(1, length(sweden_1993_Preston_5x5$pop_count_age_f_begin))

    ccmpp_r_mig_even <-
        project_ccmpp_z_by_z(z=sweden_1993_Preston_5x5$age_width,
                             pop_count_age_m_start=sweden_1993_Preston_5x5$pop_count_age_m_begin,
                             pop_count_age_f_start=sweden_1993_Preston_5x5$pop_count_age_f_begin,
                             survival_ratio_age_m = survival_ratio_age_m_0,
                             survival_ratio_age_f = survival_ratio_age_f_0,
                             fert_rate_age_f = fert_rate_age_f_0,
                             srb=srb_0,
                             mig_net_count_age_m = mig_net_count_age_m_0,
                             mig_net_count_age_f = mig_net_count_age_f_0,
                             mig_assumption = "even")
    expect_equal(ccmpp_r_mig_even$pop_count_age_m_end, ccmpp_r_mig_even$pop_count_age_f_end)

    ccmpp_r_mig_end <-
        project_ccmpp_z_by_z(z=sweden_1993_Preston_5x5$age_width,
                             pop_count_age_m_start=sweden_1993_Preston_5x5$pop_count_age_m_begin,
                             pop_count_age_f_start=sweden_1993_Preston_5x5$pop_count_age_f_begin,
                             survival_ratio_age_m = survival_ratio_age_m_0,
                             survival_ratio_age_f = survival_ratio_age_f_0,
                             fert_rate_age_f = fert_rate_age_f_0,
                             srb=srb_0,
                             mig_net_count_age_m = mig_net_count_age_m_0,
                             mig_net_count_age_f = mig_net_count_age_f_0,
                             mig_assumption = "end")
    expect_equal(ccmpp_r_mig_end$pop_count_age_m_end, ccmpp_r_mig_end$pop_count_age_f_end)

})

test_that("the baseline population is returned under 'NULL' projection conditions.", {

    data(sweden_1993_Preston_5x5)

    ## 'NULL' versions
    srb_0 <- 1
    fert_rate_age_f_0 <- rep(0, length(sweden_1993_Preston_5x5$pop_count_age_f_begin))
    mig_net_count_age_f_0 <- mig_net_count_age_m_0 <- rep(0, length(sweden_1993_Preston_5x5$pop_count_age_f_begin))
    survival_ratio_age_f_0 <- survival_ratio_age_m_0 <- rep(1, length(sweden_1993_Preston_5x5$pop_count_age_f_begin))

    ccmpp_r_mig_even <-
        project_ccmpp_z_by_z(z=sweden_1993_Preston_5x5$age_width,
                             pop_count_age_m_start=sweden_1993_Preston_5x5$pop_count_age_m_begin,
                             pop_count_age_f_start=sweden_1993_Preston_5x5$pop_count_age_f_begin,
                             survival_ratio_age_m = survival_ratio_age_m_0,
                             survival_ratio_age_f = survival_ratio_age_f_0,
                             fert_rate_age_f = fert_rate_age_f_0,
                             srb=srb_0,
                             mig_net_count_age_m = mig_net_count_age_m_0,
                             mig_net_count_age_f = mig_net_count_age_f_0,
                             mig_assumption = "even")
    expect_equal(ccmpp_r_mig_even$pop_count_age_f_end[2:(length(sweden_1993_Preston_5x5$pop_count_age_f_begin) - 1)],
                 sweden_1993_Preston_5x5$pop_count_age_f_begin[1:(length(sweden_1993_Preston_5x5$pop_count_age_f_begin) - 2)])
    expect_equal(ccmpp_r_mig_even$pop_count_age_m_end[2:(length(sweden_1993_Preston_5x5$pop_count_age_f_begin) - 1)],
                 sweden_1993_Preston_5x5$pop_count_age_m_begin[1:(length(sweden_1993_Preston_5x5$pop_count_age_f_begin) - 2)])

    ## Add other migration assumption variants.

})

test_that("All functions reproduce Preston et al. (2011) Box 6.2", {

    data(sweden_1993_Preston_5x5)

    ccmpp_r_mig_even <-
        project_ccmpp_z_by_z(z = sweden_1993_Preston_5x5$age_width,
                       pop_count_age_m_start = sweden_1993_Preston_5x5$pop_count_age_m_begin,
                       pop_count_age_f_start = sweden_1993_Preston_5x5$pop_count_age_f_begin,
                       survival_ratio_age_m = sweden_1993_Preston_5x5$survival_ratio_age_m,
                       survival_ratio_age_f = sweden_1993_Preston_5x5$survival_ratio_age_f,
                       fert_rate_age_f = sweden_1993_Preston_5x5$fert_rate_age_f,
                       srb = sweden_1993_Preston_5x5$srb,
                       mig_net_count_age_m = sweden_1993_Preston_5x5$mig_net_count_age_m,
                       mig_net_count_age_f = sweden_1993_Preston_5x5$mig_net_count_age_f,
                       mig_assumption = "even")
    expect_equal(round(ccmpp_r_mig_even$pop_count_age_f_end,0), sweden_1993_Preston_5x5$pop_count_age_f_end)
    expect_equal(round(ccmpp_r_mig_even$pop_count_age_m_end,0), sweden_1993_Preston_5x5$pop_count_age_m_end)
    })


