#' 
#' Approximate age-specific person-years of exposure to fertility/mortality in the period 
#'
#' @description This function takes cohort-period deaths and age-specific nMx to approximate age-specific person years of exposure
#' Useful for aggregating life tables across time, age, geographies
#'
#' @author Sara Hertog
#'
#' @param death_age_period numeric. vector of age-period deaths
#' @param nmx_age_period numeric. vector of life table nMx values
#'
#' @return a numeric vector of person-years of exposure by single year of age.
#' @export
exposure_age <- function(death_age_period, nmx_age_period) {

  exposure_age <- death_age_period/nmx_age_period
  
  return(exposure_age)
  
}

#' Loop over time to approximate age-specific person years of exposure
#'
#' @description Loops over time to implement the exposure_age function for multiple
#' periods of time.
#'
#' @author Sara Hertog
#'
#' @param dth_age data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the sex- and age-specific death counts.
#' @param nmx data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the nMx values from the wpp inputs life table data frame
#' 
#' @return a data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the sex- and age-specific person years of exposure counts 
#' @export
exposure_age_sex_loop_over_time <- function(dth_age, nmx) {
  
  # initialize output list
  exposure_output_list <- list()
  n <- 0
  
  time_span              <- nmx$time_span[1]
  time_start             <- min(nmx$time_start)
  time_end               <- max(nmx$time_start)
  age_start              <- unique(nmx$age_start) 
  nage                   <- length(age_start)
  
  for (time in seq(time_start, time_end, time_span)) {
    for (sex in c("female", "male")) {
      
      n   <- n+1
      
      exp <- exposure_age(death_age_period  = dth_age$value[which(dth_age$time_start == time &dth_age$sex == sex)],
                          nmx_age_period    = nmx$value[which(nmx$time_start == time & nmx$sex == sex)])
      
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



#' Loop over time to adjust age-specific person years of exposure
#' THIS FUNCTION IS NOT LONGER USED.  NEED TO UPDATE DOCUMENTATION TO REMOVE IT FROM NAMESPACE AND PREVENT ERRORS
#' ON PACKAGE INSTALLATION
#' WHEN I TRY TO UPDATE DOCUMENTION, IT GIVES ME A BUNCH OF ERRORS WRT DOCUMENATION OF S3 CLASSES
#' NEED TO CHECK WITH MARK ABOUT WHAT IS GOING WRONG HERE
#'
#' @description Loops over time to re-compute age-specific exposure from input age_specific mortality and age-specific deaths
#' periods of time.
#'
#' @author Sara Hertog
#'
#' @param death_age_sex_period data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the sex- and age-specific death counts output by the death_age_sex_loop_over_time function.
#' @param mx data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the original sex- and age-specific mortality rates on the ccmpp_input file.
#'
#' @return a data frame with columns time_start, time_span, sex, age_start, age_span and value.  Value contains
#' the sex- and age-specific person years of exposure counts
#' @export
exposure_age_sex_adjust_loop_over_time <- function(death_age_sex_period, mx) {

  # initialize output list
  exposure_output_list <- list()
  n <- 0

  time_span              <- death_age_sex_period$time_span[1]
  time_start             <- min(death_age_sex_period$time_start)
  time_end               <- max(death_age_sex_period$time_start)
  age_start              <- unique(death_age_sex_period$age_start)
  nage                   <- length(age_start)

  for (time in seq(time_start, time_end, time_span)) {
    for (sex in c("female", "male")) {

      n   <- n+1

      # exposures as deaths divided by nmx
      exp <- death_age_sex_period$value[which(death_age_sex_period$time_start == time & death_age_sex_period$sex == sex)] /
        mx$value[which(mx$time_start == time & mx$sex == sex)]

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


