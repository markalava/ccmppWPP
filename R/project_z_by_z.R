# algebra for cohort component projection with flexible migration assumption
# also flexible number and width of age groups and flexible open age group
# Sara Hertog
# 16 April 2020
# modified 29 April 2020 to remove intermediate rounding and hybrid migration option

# Inputs
# z width of age interval/length of projection period
# P0M males at time t=0 by age group of width z
# P0F females at time t=0 by age group of width z
# SxM male survival ratios for period t to t+z by age group of width z
# SxF female survial ratios for period t to t+z by age group of width z
# asfr age specific fertility rates for period t to t+z by age group of width z
# NMxM net male migrants (absolute numbers) for period t to t+z by age group of width z
# NMxF net female migrants (absolute numbers) for period t to t+z by age group of width z
# requires that above listed intputs are all the same length
# srb sex ratio at birth (males/females)
# mig_assumption controls whether migration is accounted at end of period or evenly over period

# Outputs
# data frame with columns
# DxM male deaths during period t to t+z by age group of width z
# DxF female deaths during period t to t+z by age group of width z
# Bx births by age of mother by age group of width z
# PzM male population at time t=z by age group of width z
# PzF female population at time t=z by age group of width z
#' @export
project_z_by_z <- function(z=1, P0M, P0F, SxM, SxF, asfr, NMxM, NMxF, srb,
                        mig_assumption = c("end", "even")) {

  # check that lengths of inputs agree
  check_length <- min(length(P0M),length(P0F),length(SxM),length(SxF),length(asfr),length(NMxM),length(NMxF)) ==
    max(length(P0M),length(P0F),length(SxM),length(SxF),length(asfr),length(NMxM),length(NMxF))
  if (isFALSE(check_length)) { stop("Input columns are not all the same length")}

  nage <- length(P0M) # number of age groups

### Two possible migration assumptions: end of period or evenly over period
  # here we set up some interim vectors according to the migration assumptions
  # pxm and pxf are population at time 0 unaltered from the input if mig_assumption is end of period
  # or with half of net migration added if mig_assumption is evenly over period
  # migm_end and migf_end is the net migration to be added at the end of the period *before*
  # births are computed using mid-year female population and asfr.  These are set to 0 if
  # mig_assumption is end of period and to half of net migration if mig_assumption is evenly over period.
  # When mig_assumption is end of period, net migration is added only after all deaths and births
  # have been computed.

  if (mig_assumption == "end") {

    # all migrants are added/removed at end of period and thus have no effect on births and deaths
    pxm <- P0M
    pxf <- P0F
    migm_end <- rep(0,nage)
    migf_end <- rep(0,nage)

  } else if (mig_assumption == "even") { # add/remove half of migrants at beginning of period

    # half of increments between age x and x+1 are added at end of period and do not affect births or deaths in period
    # half of increments between age x-1 and x are added at beginning of period and survived to age x to x+1
    pxm <- P0M + NMxM/2
    pxf <- P0F + NMxF/2
    migm_end <- NMxM/2
    migf_end <- NMxF/2

  }

    # Remove dependency on dplyr's lag() (and pre-compute some things)
    lag_pxm <- c(NA, head(pxm, -1))
    lag_pxf <- c(NA, head(pxf, -1))

  # compute deaths from year 0 population and survival ratios
    DxM <- (1-SxM)*lag_pxm
    DxM[nage] <- (1-SxM[nage]) * (pxm[nage-1]+pxm[nage])
    DxF <- (1-SxF)*lag_pxf
    DxF[nage] <- (1-SxF[nage]) * (pxf[nage-1]+pxf[nage])

  # project population by age at year +1 from year 0 population and deaths
    PzM <- lag_pxm - DxM + migm_end
    PzM[nage] <- pxm[nage-1] + pxm[nage] - DxM[nage] + migm_end[nage]
    PzF <- lag_pxf - DxF + migf_end
    PzF[nage] <- pxf[nage-1] + pxf[nage] - DxF[nage] + migf_end[nage]

  # compute births from year 0 female population, asfr and srb
    Bx <- z * asfr * (pxf + PzF)/2
    Bx[c(1,nage)] <- 0
    Btot <- sum(Bx)
    BtotF <- Btot * (1/(1+srb))
    BtotM <- Btot - BtotF

  # compute infant deaths from total births by sex and survival ratio
    DxM[1] <- (1 - SxM[1]) * BtotM
    DxF[1] <- (1 - SxF[1]) * BtotF

  # project infant population at year +1 from births and infant deaths
    PzM[1] <- BtotM - DxM[1] + migm_end[1]
    PzF[1] <- BtotF - DxF[1] + migf_end[1]

  # if end-of-period assumption then add migrants
    if (mig_assumption == "end") {
      PzM <- PzM + NMxM
      PzF <- PzF + NMxF
    }

  # ensure no negative population by age and sex (0.0005 is same as Abacus)
    PzM[which(PzM<0)] <- 0.0005
    PzF[which(PzF<0)] <- 0.0005

  # assemble into data frame and return
    df <- data.frame(Age=c(seq(0,(nage-1)*z, z)),
                        P0M = P0M,
                        P0F = P0F,
                        DxM = DxM,
                        DxF = DxF,
                        Bx = Bx,
                        PzM = PzM,
                        PzF = PzF)

    return(df)

    }


    ## need to look closely at first and last age group
    ## also need to determine whether lead(migm_end) should be used in P1M calc instead of just migm_end



