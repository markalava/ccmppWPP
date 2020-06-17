#' fertility computation helper functions for WPP
#' 
#' 
#' Compute percentage age-specific fertility rates from age-specific fertility rates 
#'
#' @description This function computes the percentage age-specific fertility rates from the age-specific fertility
#' rates of a given period.
#'
#' @author Sara Hertog
#'
#' @param fert_data_age data frame. column "age" is age group of mother and "value" is age-specific fertility rates 
#' in births per woman.
#' @param byvar character. string of column names in fert_data_age by which pasfr will be computed.
#'
#' @details age/period can be 1x1 or 5x5.
#'
#' @return a data frame with percentage age specific fertility rates in the "value" field, computed by byvar 
#' columns and age. Sums to 100 for each combination of byvar.
#' @export
fert_pasfr               <- function(fert_data_age, byvar) {
  
  bylist <- list()
  for (i in 1:length(byvar)) {
    bylist[[i]] <- fert_data_age[, byvar[i]]
  }
  
  age_width              <- fert_data_age$age_start[2] - fert_data_age$age_start[1]
  fert_rate_total        <- aggregate(fert_data_age$value * age_width,
                                      by = bylist,
                                      sum)
  names(fert_rate_total) <- c(byvar, "tfr")
  pasfr                  <- merge(fert_data_age, fert_rate_total, by=byvar)
  pasfr$value            <- pasfr$value * age_width / pasfr$tfr * 100
  pasfr                  <- pasfr[, c(byvar, "age_start", "age_span", "value")]
  return(pasfr)
  
}

#' Compute mean age at childbearing (mac) from age-specific fertility rates 
#'
#' @description This function computes the mean age at childbearing from the age-specific fertility rates 
#' of a given period.
#'
#' @author Sara Hertog
#'
#' @param fert_data_age data frame. column "age" is age group of mother and "value" is age-specific fertility rates in births per woman.
#' @param byvar character. string of column names in fert_data_age by which pasfr will be computed.
#'
#' @details age/period can be 1x1 or 5x5.
#'
#' @return a data frame with mean age at childbearing in the "value" field, computed by byvar columns and age.
#' @export
fert_mac                  <- function(fert_data_age, byvar) {
  
  bylist <- list()
  for (i in 1:length(byvar)) {
    bylist[[i]] <- fert_data_age[, byvar[i]]
  }
  
  age_width               <- fert_data_age$age_start[2] - fert_data_age$age_start[1]
  sum_fert_rate_times_age <- aggregate(fert_data_age$value * (fert_data_age$age_start + age_width/2),
                                       by = bylist,
                                       sum)
  sum_fert_rate           <- aggregate(fert_data_age$value,
                                       by = bylist,
                                       sum)
  value                   <- sum_fert_rate_times_age$x / sum_fert_rate$x
  mac                     <- as.data.frame(cbind(sum_fert_rate[,1:(ncol(sum_fert_rate)-1)],
                                                 value))
  names(mac)              <- c(byvar, "value")
  return(mac)
  
}