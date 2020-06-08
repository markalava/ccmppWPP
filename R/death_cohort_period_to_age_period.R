#' Compute period death by age from period deaths by cohort output from ccmpp
#'
#' @description This function computes the age-specific period deaths from age-specific person-years of exposure
#' and the age-specific mortality rates for the period.  It further makes slight adjustments to deaths, as 
#' necessary, to ensure that total period deaths by age are equal to total period deaths by cohort computed by the 
#' project_ccmpp_z_by_z function.
#'
#' @author Sara Hertog
#'
#' @param mort_rate_age_period numeric. vector of mortality rates by age.
#' @param exposure_age_period numeric. vector of person-years of exposure by age.
#' @param distribute_residual logical. controls whether residual difference between total deaths by age and total
#' deaths by cohort should be distributed over deaths by age proportional to the age distribution of deaths.
#' @param death_cohort_period numeric. vector of deaths by cohort computed by project_ccmpp_z_by_z.
#'
#' @return a numeric vector period deaths by age.
#' @export
death_cohort_period_to_age_period <- function(mort_rate_age_period, 
                                              exposure_age_period,
                                              distribute_residual = TRUE,
                                              death_cohort_period) {
  
  # compute deaths from period mortality rates by age and exposures
  death_age_period <- exposure_age_period * mort_rate_age_period
  
  # sum of age-period deaths may differ slightly from cohort-period due to rounding
  # distribute the residual according to the distribution of deaths by age
  if (distribute_residual == TRUE) {
    
    death_residual     <- sum(death_cohort_period) - sum(death_age_period)
    death_residual_age <- death_residual * (death_age_period / sum(death_age_period))
    death_age_period   <- death_age_period + death_residual_age
    
  }
  
  return(death_age_period)
  
}