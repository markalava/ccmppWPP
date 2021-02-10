#' 
#' Compute age-specific person-years of exposure to fertility/mortality in the period 
#'
#' @description This function takes beginning and end period population by age and computes person-years of exposure, 
#' removing migrants from exposure if migration assuption is "end" of period 
#'
#' @author Sara Hertog
#'
#' @param pop_age_start numeric. vector of population by age at the beginning of the period.
#' @param pop_age_end numeric. vector of population by age at the end of the period.
#' @param mig_assumption character. field indicating whether migration is treated as "end" of year or "even" over year.
#' @param mig_net_count numeric. vector of net migrants age between begin time_start and time_end.
#'
#' @details for "end" migration assumption, net migrants are subtracted from end of year population for computing exposures.
#'
#' @return a numeric vector of person-years of exposure by single year of age.
#' @export
exposure_age <- function(pop_age_start, pop_age_end, mig_assumption=c("even","end"), mig_net_count=0) {

  if (mig_assumption == "end") {
    pop_age_end <- pop_age_end - mig_net_count
  }
  # age-specific constant growth rate
  r_age <- log(pop_age_end/pop_age_start)
  exposure_age <- (pop_age_end - pop_age_start)/ r_age
  return(exposure_age)
  
}

#' Loop over time to compute age-specific person years of exposure
#'
#' @description Loops over time to implement the exposure_age function for multiple
#' periods of time.
#'
#' @author Sara Hertog
#'
#' @param pop data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the sex- and age-specific population counts output by the project_ccmpp_loop_over_time function.
#' @param mig_assumption character vector. field indicating whether migration is treated as "end" of period or "even" over period.
#' @param mig data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the sex- and age-specific net migrant counts output by the project_ccmpp_loop_over_time function.
#'
#' @return a data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the sex- and age-specific person years of exposure counts 
#' @export
exposure_age_sex_loop_over_time <- function(pop, mig_assumption, mig) {
  
  # initialize output list
  exposure_output_list <- list()
  n <- 0
  
  time_span              <- pop$time_span[1]
  time_start             <- min(pop$time_start)
  time_end               <- max(pop$time_start)
  age_start              <- unique(pop$age_start) 
  nage                   <- length(age_start)
  
  for (time in seq(time_start, time_end-time_span, time_span)) {
    for (sex in c("female", "male")) {
      
      n   <- n+1
      
      exp <- exposure_age(pop_age_start        = pop$value[which(pop$time_start == time & pop$sex == sex)],
                          pop_age_end          = pop$value[which(pop$time_start == time+time_span & pop$sex == sex)],
                          mig_assumption       = mig_assumption$value[which(mig_assumption$time_start == time)],
                          mig_net_count        = mig$value[which(mig$time_start == time & mig$sex == sex)])
      
      exposure_output_list[[n]] <- data.frame(time_start = time,
                                              time_span  = time_span,
                                              sex        = sex,
                                              age_start  = age_start,
                                              age_span   = c(rep(time_span, nage-1), 1000),
                                              value      = exp,
                                              stringsAsFactors = FALSE)
    }
  }
  exposure_count_age_sex <- do.call(rbind, exposure_output_list)
  
  return(exposure_count_age_sex)
  
}