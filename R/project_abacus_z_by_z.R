

#------------------------------------------------------------

#' R implementation of the one-step cohort component population projection approach from legacy Abacus
#'
#' @description This function takes a set of base population and demographic inputs for the period base year to base year+z years with age groups of width z and projects population forward z years according to the logic implemented in Abacus for WPP revisions 2019 and earlier.
#'
#' @author Sara Hertog
#'
#' @param z width of age groups = length of one-step projection horizon (Abacus was 5 years only)
#' @param pop_count_age_m_start a vector of male population counts at the base year (time 0) by age
#' @param pop_count_age_f_start a vector of female population counts at the base year (time 0) by age
#' @param survival_ratio_age_m a vector of survival ratios by age for males from a life table referencing the period 0 to 0+z
#' @param survival_ratio_age_f a vector of survival ratios by age for females from a life table referencing the period 0 to 0+z
#' @param fert_rate_age_f a vector of age-specific fertility rates referencing the period 0 to 0+z
#' @param srb a scalar for the sex ratio at birth for the period 0 to 0+z (male births/female births)
#' @param mig_net_count_age_m a vector of net migration counts for males by age referencing the period 0 to 0+z
#' @param mig_net_count_age_f a vector of net migration counts for females by age referencing the period 0 to 0+z
#' @param mig_assumption a control for the migration assumption. This is what differentiates the Abacus implementation here from the
#' implementation in the project_ccmpp_z_by_z function.  In Abacus, "end" means migration is accounted at end of projection period, but 
#' there is still some exposure of female migrants to fertility and thus births are affected by the level of net migration.
#' "even" in Abacus accounts for migration evenly distributed over the period, but 
#' excludes beginning of period migrants from exposure to fertility. This is because Abacus first projects population and applies
#' the migration assumption and then computes births by exposing (beginning period females + end of period females)/2 to the age specific
#' fertility rates of the period.  We have deemed this to be not the ideal implementation of the migration assumption and have thus
#' adopted the cleaner approach in project_ccmpp_z_by_z for WPP 2021 
#' #'
#' @details This function will accept any width of age group and projection horizon as long as those are equal
#' e.g., use z=1 for a 1x1 projection of population by single year of age and 1 year projection horizon
#' or use z=5 for a 5x5 projection of population by 5-year age groups and 5-year projection horizon (this is what Abacus does)
#'
#' @return a list of objects including projected population by age and sex, deaths by age and sex,
#' births by mother's age, total births, and births by sex
#' 
#' @export


project_abacus_z_by_z <- function(z=5, 
                           pop_count_age_m_start, 
                           pop_count_age_f_start, 
                           survival_ratio_age_m, 
                           survival_ratio_age_f,
                           fert_rate_age_f, 
                           srb,
                           mig_net_count_age_m,
                           mig_net_count_age_f, 
                           mig_assumption = c("end", "even")) {

  # check that lengths of inputs agree
  check_length <- min(length(pop_count_age_m_start),length(pop_count_age_f_start),
                      length(survival_ratio_age_m),length(survival_ratio_age_f),
                      length(fert_rate_age_f),
                      length(mig_net_count_age_m),length(mig_net_count_age_f)) ==
    max(length(pop_count_age_m_start),length(pop_count_age_f_start),
        length(survival_ratio_age_m),length(survival_ratio_age_f),
        length(fert_rate_age_f),length(mig_net_count_age_m),length(mig_net_count_age_f))
  if (isFALSE(check_length)) { stop("Input columns are not all the same length")}

  nage <- length(pop_count_age_m_start) # number of age groups

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

    # all migrants are added/removed at end of period but before births are computed
    # migration-adjusted beginning of period population by age and sex
    pop_count_age_m_start_mig_adj <- pop_count_age_m_start
    pop_count_age_f_start_mig_adj <- pop_count_age_f_start
    # net migration to be added to end of period population by age and sex *before* period births are computed
    mig_net_count_age_m_end <- mig_net_count_age_m
    mig_net_count_age_f_end <- mig_net_count_age_f

  } else if (mig_assumption == "even") { # add/remove half of migrants at beginning of period

    # half of increments between age x and x+1 are added at end of period but before births are computed
    # half of increments between age x-1 and x are added at beginning of period and survived to age x to x+1, but are not exposed to fertility in the period
    # migration-adjusted beginning of period population by age and sex
    pop_count_age_m_start_mig_adj <- pop_count_age_m_start + mig_net_count_age_m/2
    pop_count_age_f_start_mig_adj <- pop_count_age_f_start + mig_net_count_age_f/2
    # net migration to be added to end of period population by age and sex *before* period births are computed
    mig_net_count_age_m_end <- mig_net_count_age_m/2
    mig_net_count_age_f_end <- mig_net_count_age_f/2

  }

  # Pre-compute lagged variables
    lag_pop_count_age_m_start_mig_adj <- c(NA, head(pop_count_age_m_start_mig_adj, -1))
    lag_pop_count_age_f_start_mig_adj <- c(NA, head(pop_count_age_f_start_mig_adj, -1))

  # compute deaths from year 0 population and survival ratios
    death_count_cohort_m <- (1-survival_ratio_age_m)*lag_pop_count_age_m_start_mig_adj
    death_count_cohort_m[nage] <- (1-survival_ratio_age_m[nage]) * (pop_count_age_m_start_mig_adj[nage-1]+pop_count_age_m_start_mig_adj[nage])
    
    death_count_cohort_f <- (1-survival_ratio_age_f)*lag_pop_count_age_f_start_mig_adj
    death_count_cohort_f[nage] <- (1-survival_ratio_age_f[nage]) * (pop_count_age_f_start_mig_adj[nage-1]+pop_count_age_f_start_mig_adj[nage])

  # project population by age at year +z from year 0 population and deaths
    pop_count_age_m_end <- lag_pop_count_age_m_start_mig_adj - death_count_cohort_m + mig_net_count_age_m_end
    pop_count_age_m_end[nage] <- pop_count_age_m_start_mig_adj[nage-1] + pop_count_age_m_start_mig_adj[nage] - death_count_cohort_m[nage] + mig_net_count_age_m_end[nage]
    
    pop_count_age_f_end <- lag_pop_count_age_f_start_mig_adj - death_count_cohort_f + mig_net_count_age_f_end
    pop_count_age_f_end[nage] <- pop_count_age_f_start_mig_adj[nage-1] + pop_count_age_f_start_mig_adj[nage] - death_count_cohort_f[nage] + mig_net_count_age_f_end[nage]

  # compute births from female population, age-specific fertility rates and sex ratio at birth
    birth_count_age_b <- z * fert_rate_age_f * (pop_count_age_f_start + pop_count_age_f_end)/2 #begin period exposure is *not* migration-adjusted in Abacus
    birth_count_age_b[c(1,nage)] <- 0
    birth_count_tot_b <- sum(birth_count_age_b)
    birth_count_tot_f <- birth_count_tot_b * (1/(1+srb))
    birth_count_tot_m <- birth_count_tot_b - birth_count_tot_f

  # compute infant deaths from total births by sex and survival ratio
    death_count_cohort_m[1] <- (1 - survival_ratio_age_m[1]) * birth_count_tot_m
    death_count_cohort_f[1] <- (1 - survival_ratio_age_f[1]) * birth_count_tot_f

  # project infant population at year +1 from births and infant deaths
    pop_count_age_m_end[1] <- birth_count_tot_m - death_count_cohort_m[1] + mig_net_count_age_m_end[1]
    pop_count_age_f_end[1] <- birth_count_tot_f - death_count_cohort_f[1] + mig_net_count_age_f_end[1]

  # ensure no negative population by age and sex 
    pop_count_age_m_end[which(pop_count_age_m_end<0)] <- 0.0005
    pop_count_age_f_end[which(pop_count_age_f_end<0)] <- 0.0005

  # assemble list of outputs and return

    proj_list <- list(age=c(seq(0,(nage-1)*z, z)),
                      pop_count_age_m_end = pop_count_age_m_end,
                      pop_count_age_f_end = pop_count_age_f_end,
                      death_count_cohort_m = death_count_cohort_m,
                      death_count_cohort_f = death_count_cohort_f,
                      birth_count_age_b = birth_count_age_b,
                      birth_count_tot_b = birth_count_tot_b,
                      birth_count_tot_m = birth_count_tot_m,
                      birth_count_tot_f = birth_count_tot_f)

    return(proj_list)

    }


