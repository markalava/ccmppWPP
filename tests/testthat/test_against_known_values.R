context("Test that projection functions return values known to be correct.")

test_that("the female and male projected populations are equal under 'NULL' projection conditions.", {

    data(sweden_1993)

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
    expect_equal(ccmpp_r_mig_even$PzM, ccmpp_r_mig_even$PzF)

    ## Add other migration assumption variants.

})

test_that("the baseline population is returned under 'NULL' projection conditions.", {

    data(sweden_1993)

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
    expect_equal(ccmpp_r_mig_even$PzF[2:(nrow(ccmpp_r_mig_even) - 1)],
                 ccmpp_r_mig_even$P0F[1:(nrow(ccmpp_r_mig_even) - 2)])
    expect_equal(ccmpp_r_mig_even$PzM[2:(nrow(ccmpp_r_mig_even) - 1)],
                 ccmpp_r_mig_even$P0M[1:(nrow(ccmpp_r_mig_even) - 2)])

    ## Add other migration assumption variants.

})

test_that("All functions reproduce Preston et al. (2011) Box 6.2", {

    data(sweden_1993)

    ccmpp_r_mig_even <-
        project_z_by_z(z=sweden_1993$n, P0M=sweden_1993$P0M, P0F=sweden_1993$P0F,
                       SxM=sweden_1993$SxM, SxF=sweden_1993$SxF, asfr=sweden_1993$asfr,
                       NMxM=sweden_1993$NMxM, NMxF=sweden_1993$NMxF, srb=sweden_1993$srb,
                       mig_assumption = "even")
    expect_equal(round(ccmpp_r_mig_even$PzF,0), sweden_1993$PzF)
    expect_equal(round(ccmpp_r_mig_even$PzM,0), sweden_1993$PzM)
    })
