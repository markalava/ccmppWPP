
test_that("the female and male projected populations are equal under 'NULL' projection conditions.", {

    data(sweden_1993, package = "ccmppWPP")

    ## 'NULL' versions
    srb_0 <- 1
    asfr_0 <- rep(0, length(sweden_1993$P0F))
    NMxF_0 <- NMxM_0 <- rep(0, length(sweden_1993$P0F))
    SxF_0 <- SxM_0 <- rep(1, length(sweden_1993$P0F))

    ccmpp_r_mig_even <-
        project_z_by_z(z=sweden_1993$n, P0M=sweden_1993$P0M, P0F=sweden_1993$P0F,
                       SxM=SxM_0, SxF=SxF_0, asfr=asfr_0,
                       NMxM=NMxM_0, NMxF=NMxF_0, srb=srb_0,
                       mig_assumption = "even")
    expect_type(ccmpp_r_mig_even$PzM, "double")
    expect_type(ccmpp_r_mig_even$PzF, "double")
    expect_equal(ccmpp_r_mig_even$PzM, ccmpp_r_mig_even$PzF)

    ccmpp_cpp_mig_even <-
        proj_pop_cpp(step_size = sweden_1993$n,
                      pop_count_age_m_t0 = sweden_1993$P0M,
                      pop_count_age_f_t0 = sweden_1993$P0F,
                      surv_prop_age_m = SxM_0,
                      surv_prop_age_f = SxF_0,
                      fert_rate_age = asfr_0,
                      net_mig_count_age_m = NMxM_0,
                      net_mig_count_age_f = NMxF_0,
                     srb_tot = srb_0,
                     mig_assumption = "even")
    expect_type(ccmpp_cpp_mig_even$pop_count_age_m_t1, "double")
    expect_type(ccmpp_cpp_mig_even$pop_count_age_f_t1, "double")
    expect_equal(ccmpp_cpp_mig_even$pop_count_age_m_t1,
                 ccmpp_cpp_mig_even$pop_count_age_f_t1)

    ## Add other migration assumption variants.

})

test_that("the baseline population is returned under 'NULL' projection conditions.", {

    data(sweden_1993, package = "ccmppWPP")

    nages <- length(sweden_1993$P0F)

    ## 'NULL' versions
    srb_0 <- 1
    asfr_0 <- rep(0, nages)
    NMxF_0 <- NMxM_0 <- rep(0, nages)
    SxF_0 <- SxM_0 <- rep(1, nages)

    ccmpp_r_mig_even <-
        project_z_by_z(z=sweden_1993$n, P0M=sweden_1993$P0M, P0F=sweden_1993$P0F,
                       SxM=SxM_0, SxF=SxF_0, asfr=asfr_0,
                       NMxM=NMxM_0, NMxF=NMxF_0, srb=srb_0,
                       mig_assumption = "even")
    expect_type(ccmpp_r_mig_even$PzM, "double")
    expect_type(ccmpp_r_mig_even$PzF, "double")
    expect_equal(ccmpp_r_mig_even$PzF[2:(nages - 1)],
                 ccmpp_r_mig_even$P0F[1:(nages - 2)])
    expect_equal(ccmpp_r_mig_even$PzM[2:(nages - 1)],
                 ccmpp_r_mig_even$P0M[1:(nages - 2)])

    ccmpp_cpp_mig_even <-
        proj_pop_cpp(step_size = sweden_1993$n,
                      pop_count_age_m_t0 = sweden_1993$P0M,
                      pop_count_age_f_t0 = sweden_1993$P0F,
                      surv_prop_age_m = SxM_0,
                      surv_prop_age_f = SxF_0,
                      fert_rate_age = asfr_0,
                      net_mig_count_age_m = NMxM_0,
                      net_mig_count_age_f = NMxF_0,
                     srb_tot = srb_0,
                     mig_assumption = "even")
    expect_type(ccmpp_cpp_mig_even$pop_count_age_m_t1, "double")
    expect_type(ccmpp_cpp_mig_even$pop_count_age_f_t1, "double")
    expect_type(ccmpp_cpp_mig_even$pop_count_age_m_t0, "double")
    expect_type(ccmpp_cpp_mig_even$pop_count_age_f_t0, "double")
    expect_equal(ccmpp_cpp_mig_even$pop_count_age_m_t1[2:(nages - 1)],
                 ccmpp_cpp_mig_even$pop_count_age_m_t0[1:(nages - 2)])
    expect_equal(ccmpp_cpp_mig_even$pop_count_age_f_t1[2:(nages - 1)],
                 ccmpp_cpp_mig_even$pop_count_age_f_t0[1:(nages - 2)])

    ## Add other migration assumption variants.

})

test_that("All functions reproduce Preston et al. (2011) Box 6.2", {

    data(sweden_1993, package = "ccmppWPP")

    ccmpp_r_mig_even <-
        project_z_by_z(z=sweden_1993$n, P0M=sweden_1993$P0M, P0F=sweden_1993$P0F,
                       SxM=sweden_1993$SxM, SxF=sweden_1993$SxF, asfr=sweden_1993$asfr,
                       NMxM=sweden_1993$NMxM, NMxF=sweden_1993$NMxF, srb=sweden_1993$srb,
                       mig_assumption = "even")
    expect_equal(round(ccmpp_r_mig_even$PzF,0), sweden_1993$PzF)
    expect_equal(round(ccmpp_r_mig_even$PzM,0), sweden_1993$PzM)
    expect_equal(round(ccmpp_r_mig_even$Bx,0), sweden_1993$Bx)

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
    expect_equal(round(ccmpp_cpp_mig_even$birth_count_age_b,0), sweden_1993$Bx)
    expect_equal(round(ccmpp_cpp_mig_even$pop_count_age_f_t1,0), sweden_1993$PzF)
    expect_equal(round(ccmpp_cpp_mig_even$pop_count_age_m_t1,0), sweden_1993$PzM)
    })
