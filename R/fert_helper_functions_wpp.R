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
#' @param fert_data_age data frame. columns "age_start" and "age_span" describe age group of mother
#' and "value" is age-specific fertility rates in births per woman.
#' @param byvar character. string of column names in fert_data_age by which pasfr will be computed.
#'
#' @details accomodates any width of age interval (age_span) so long as it is constant over age.
#'
#' @return a data frame with percentage age specific fertility rates in the "value" field, computed by byvar
#' columns and age_start, age_span. Sums to 100 for each combination of byvar.
#' @export
fert_pasfr               <- function(fert_data_age, byvar) {

  bylist <- list()
  for (i in 1:length(byvar)) {
    bylist[[i]] <- fert_data_age[, byvar[i]]
  }

  age_width              <- fert_data_age$age_span[1]
  fert_rate_total        <- stats::aggregate(fert_data_age$value * age_width,
                                      by = bylist,
                                      sum)
  names(fert_rate_total) <- c(byvar, "tfr")
  pasfr                  <- merge(fert_data_age, fert_rate_total, by = byvar)
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
#' @param fert_data_age data frame. columns "age_start" and "age_span" describe age group of mother
#' and "value" is age-specific fertility rates in births per woman.
#' @param byvar character. string of column names in fert_data_age by which mac will be computed.
#'
#' @details accomodates any width of age interval (age_span) so long as it is constant over age.
#'
#' @return a data frame with mean age at childbearing in the "value" field, computed by byvar columns.
#' @export
fert_mac                  <- function(fert_data_age, byvar) {

  bylist <- list()
  for (i in 1:length(byvar)) {
    bylist[[i]] <- fert_data_age[, byvar[i]]
  }

  age_width               <- fert_data_age$age_span[1]
  sum_fert_rate_times_age <- stats::aggregate(fert_data_age$value * (fert_data_age$age_start + age_width/2),
                                       by = bylist,
                                       sum)
  sum_fert_rate           <- stats::aggregate(fert_data_age$value,
                                       by = bylist,
                                       sum)
  value                   <- sum_fert_rate_times_age$x / sum_fert_rate$x
  mac                     <- as.data.frame(cbind(sum_fert_rate[,1:(ncol(sum_fert_rate)-1)],
                                                 value))
  names(mac)              <- c(byvar, "value")
  return(mac)

}


#' Compute gross reproductive rate from age-specific fertility rates and sex ratio at birth
#'
#' @description This function computes the gross reproductive rate from the age-specific fertility rates
#' and sex ratio at birth of a given period.
#'
#' @author Sara Hertog
#'
#' @param fert_data_age data frame. columns "age_start" and "age_span" describe age group of mother
#' and "value" is age-specific fertility rates in births per woman.
#' @param srb data frame. column "value" is the sex ratio at birth
#' @param byvar character. string of column names in fert_data_age and srb by which GRR will be computed.
#'
#' @details accomodates any width of age interval (age_span) so long as it is constant over age.
#'
#' @return a data frame with gross reproductive rate in the "value" field, computed by byvar columns.
#' @export

fert_gross                  <- function(fert_data_age, srb, byvar) {

  bylist <- list()
  for (i in 1:length(byvar)) {
    bylist[[i]] <- fert_data_age[, byvar[i]]
  }

  # add srb value to fertility data by age and compute maternity rates
  srb$srb                 <- srb$value
  srb                     <- srb[,!names(srb) %in% c("value")]
  maternity_data_age      <- merge(fert_data_age, srb, by = byvar, all.x = TRUE, all.y = FALSE)
  maternity_data_age$value<- maternity_data_age$value / (1 + maternity_data_age$srb) * maternity_data_age$age_span

  # sum over age to get GRR
  gross           <- stats::aggregate(maternity_data_age$value,
                                       by = bylist,
                                       sum)
  names(gross)              <- c(byvar, "value")
  return(gross)

}

#' Compute net reproductive rate from age-specific fertility rates, sex ratio at birth and female life table nLx
#'
#' @description This function computes the net reproductive rate from the age-specific fertility rates
#' and sex ratio at birth and female life table nLx of a given period.
#'
#' @author Sara Hertog
#'
#' @param fert_data_age data frame. columns "age_start" and "age_span" describe age group of mother
#' and "value" is age-specific fertility rates in births per woman.
#' @param srb data frame. column "value" is the sex ratio at birth
#' @param nLx data frame. columns "age_start" and "age_span" describe age group of mother and "value"
#' is the nLx column of the period life table for females
#' @param byvar character. string of column names in fert_data_age, srb and nLx by which NRR will be computed.
#'
#' @details accomodates any width of age interval (age_span) so long as it is constant over age.
#' Assumes radix (l0) = 100000.
#'
#' @return a data frame with net reproductive rate in the "value" field, computed by byvar columns.
#' @export

fert_net                  <- function(fert_data_age, srb, nLx, byvar) {

  bylist <- list()
  for (i in 1:length(byvar)) {
    bylist[[i]] <- fert_data_age[, byvar[i]]
  }

  # add srb value to fertility data by age and compute maternity rates
  srb$srb                 <- srb$value
  srb                     <- srb[,!names(srb) %in% c("value")]
  maternity_data_age      <- merge(fert_data_age, srb, by = byvar, all.x = TRUE, all.y = FALSE)
  maternity_data_age$value<- maternity_data_age$value / (1 + maternity_data_age$srb) * maternity_data_age$age_span

  # add nLx value to maternity data by age and compute maternity rates * nLx
  nLx$nLx                 <- nLx$value
  nLx                     <- nLx[,!names(nLx) %in% c("value")]
  maternity_data_age      <- merge(maternity_data_age, nLx, by = c(byvar, "age_start", "age_span"),
                                                                   all.x = TRUE, all.y = FALSE)
  maternity_data_age$value<- maternity_data_age$value * maternity_data_age$nLx / 100000

  # sum over age to get NRR
  net           <- stats::aggregate(maternity_data_age$value,
                               by = bylist,
                               sum)
  names(net)              <- c(byvar, "value")
  return(net)

}
