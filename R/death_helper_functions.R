#' Compute period death by age from mx and expousres
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


#' Loop over time to compute age-period deaths from mortality rates and exposures
#'
#' @description Loops over time to implement the death_cohort_period_to_age_period function for multiple
#' periods of time.
#'
#' @author Sara Hertog
#'
#' @param mx data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the age-specific mortality rates from the wpp inputs life table data frame
#' @param exp data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the sex- and age-specific person years of exposure output by the exposure_age_sex_loop_over_time function
#' @param distribute_residual logical. controls whether residual difference between total deaths by age and total
#' deaths by cohort should be distributed over deaths by age proportional to the age distribution of deaths.
#' @param dth_cohort data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the cohort-specific death counts output from the project_ccmpp_loop_over_time function
#'
#' @return a data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the sex- and age-specific death counts 
#' @export
death_age_sex_loop_over_time <- function(mx, exp, distribute_residual = TRUE, dth_cohort) {
  
  # initialize output list
  death_output_list <- list()
  n <- 0
  
  time_span              <- mx$time_span[1]
  time_start             <- min(mx$time_start)
  time_end               <- max(mx$time_start + time_span)
  age_start              <- unique(mx$age_start) 
  nage                  <- length(age_start)
  
  for (time in seq(time_start, time_end-time_span, time_span)) {
    for (sex in c("female", "male")) {
      
      n   <- n+1
      
      dth <- death_cohort_period_to_age_period(mort_rate_age_period = mx$value[which(mx$time_start == time & mx$sex == sex)], 
                                               exposure_age_period = exp$value[which(exp$time_start == time & exp$sex == sex)],
                                               distribute_residual = distribute_residual,
                                               death_cohort_period = dth_cohort$value[which(dth_cohort$time_start == time & dth_cohort$sex == sex)])
      
      death_output_list[[n]] <- data.frame(time_start = time,
                                           time_span  = time_span,
                                           sex        = sex,
                                           age_start  = age_start,
                                           age_span   = c(rep(time_span, nage-1), 1000),
                                           value      = dth)
    }
  }
  death_count_age_sex <- do.call(rbind, death_output_list)
  
  return(death_count_age_sex)
  
}
