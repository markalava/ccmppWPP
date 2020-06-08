#' 
#' Compute age-specific person-years of exposure to fertility/mortality in the period 
#'
#' @description This function takes beginning and end period population by age and computes person-years of exposure, 
#' removing migrants from exposure if migration assuption is "end" of period and adding half of migrants to beginning 
#' of perod if migration assumption is "even" over period.
#'
#' @author Sara Hertog
#'
#' @param pop_age_begin numeric. vector of population by single year of age at the beginning of the year.
#' @param pop_age_end numeric. vector of population by single year of age at the end of the year.
#' @param migration_assumption character. field indicating whether migration is treated as "end" of year or "even" over year.
#' @param mig_net_count numeric. vector of net migrants by single year of age between begin year and end year.
#'
#' @details For "even" migration assumption, half of net migrants are added to beginning of year population for computing exposures;
#' for "end" migration assumption, net migrants are subtracted from end of year population for computing exposures.
#'
#' @return a numeric vector of person-years of exposure by single year of age.
#' @export
exposure_age_period <- function(pop_age_begin, pop_age_end, migration_assumption=c("even","end"), mig_net_count=0) {
  
  if (migration_assumption == "end") {
    pop_age_end <- pop_age_end - mig_net_count
  }
  exposure_age_period <- (pop_age_begin + pop_age_end) / 2
  return(exposure_age_period)
  
}