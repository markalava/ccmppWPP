#' Example life table and population of Sweden, 1993
#'
#' An example life table and population counts of the population of
#' Sweden in 1993, based on the female life table and counts in
#' \cite{Preston et. al, 2001, Ch. 6, Box 6.2}.
#'
#' @format A list with elements
#' \describe{
#' \item{P0F}{Female population by five-year age group in 1993.}
#' \item{P0M}{Male population by five-year age group in 1993.}
#' \item{LxF}{Female nLx values by five-year age group for the period [1993, 1998].}
#' \item{LxM}{Male nLx values by five-year age group for the period [1993, 1998].}
#' \item{Fx}{Age-specific fertility rates by five-year age group for the period [1993, 1998].}
#' \item{NMxF}{Age-specific net number of international female migrants for the period [1993, 1998].}
#' \item{NMxM}{Age-specific net number of international male migrants for the period [1993, 1998].}
#' \item{SxF}{Female nSx values by five-year age group for the period [1993, 1998].}
#' \item{SxM}{Female nSx values by five-year age group for the period [1993, 1998].}
#' \item{asfr}{Age-specific fertility rates by five-year age group for the period [1993, 1998].}
#' \item{srb}{Sex-ratio at birth for the period [1993, 1998].}
#' \item{n}{Width of the age intervals, in this case it is 5.}
#' \item{PzF}{Projected female population by five-year age group in 1998.}
#' \item{PzM}{Projected male population by five-year age group in 1998.}
#' }
#' @source Preston, S. H., Heuveline, P., and Guillot, M. (2001), \emph{Demography: Measuring and Modeling Population Processes}, Malden, Massachusetts: Blackwell.
"sweden_1993"

#' Example abridged life table and population of Sweden, 1993
#'
#' An example life table and population counts of the population of
#' Sweden in 1993, based on the female life table and counts in
#' \cite{Preston et. al, 2001, Ch. 6, Box 6.2}.
#'
#' @format A list with elements
#' \describe{
#' \item{pop_count_age_f_begin}{Female population by five-year age group in 1993.}
#' \item{pop_count_age_m_begin}{Male population by five-year age group in 1993.}
#' \item{pers_years_age_f}{Female nLx values by five-year age group for the period [1993, 1998].}
#' \item{pers_years_age_m}{Male nLx values by five-year age group for the period [1993, 1998].}
#' \item{fert_rate_age_f}{Age-specific fertility rates by five-year age group for the period [1993, 1998].}
#' \item{mig_net_count_age_f}{Age-specific net number of international female migrants for the period [1993, 1998].}
#' \item{mig_net_count_age_m}{Age-specific net number of international male migrants for the period [1993, 1998].}
#' \item{survival_ratio_age_f}{Female nSx values by five-year age group for the period [1993, 1998].}
#' \item{survival_ratio_age_m}{Female nSx values by five-year age group for the period [1993, 1998].}
#' \item{srb}{Sex-ratio at birth for the period [1993, 1998].}
#' \item{age_width}{Width of the age intervals, in this case it is 5.}
#' \item{birth_count_age_b}{births both sexes by five-year age group of the mother for the period [1993, 1998].}
#' \item{pop_count_age_f_end}{Projected female population by five-year age group in 1998.}
#' \item{pop_count_age_m_end}{Projected male population by five-year age group in 1998.}
#' }
#' @source Preston, S. H., Heuveline, P., and Guillot, M. (2001), \emph{Demography: Measuring and Modeling Population Processes}, Malden, Massachusetts: Blackwell.
"sweden_1993_Preston_5x5"

#' Example inputs file for 1x1 cohort component population projection for Canada 1950-2020
#'
#' All inputs needed to perform a cohort component population projection for Canada
#' by single year of age and in one-year steps for the projection (1x1) from 1950-2020
#'
#' @format A list with elements
#' \describe{
#' \item{variant}{A character string identifying the projection variant, in this case "Estimates".}
#' \item{year_base}{The base (starting) year of the projection, in this case 1950.}
#' \item{year_stop}{The year to stop the projection, in this case 2020.}
#' \item{age}{A vector of ages associated with the input data, in this case 0:100.}
#' \item{age_width}{The width of age groups associated with the input data, in this case 1.}
#' \item{pop_count_age_sex_base}{A data frame with population counts by age and sex in the base year.}
#' \item{life_table_age_sex}{A data frame of life table values by single year of age and sex by year from 1950 to 2019.}
#' \item{fert_rate_age_f}{A data frame of age-specific fertility rates by single year of age by year from 1950 to 2019.}
#' \item{mig_net_count_age_sex}{A data frame of net migrant counts by single year of age and sex by year from 1950-2019.}
#' \item{srb}{A data frame of the sex ratio at birth (males/females) by year from 1950-2019.}
#' \item{mig_assumption}{A data frame with indicator of whether migration should be accounted at "end" of period or "even" over period by year from 1950-2019.}
#' }
#' @source Interpolated from estimates published in United Nations, \emph{World Population Prospects 2019}.
"canada_wpp_1950_2020_ccmpp_inputs_1x1.rda"
