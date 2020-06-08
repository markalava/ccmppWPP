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
#' \item{LocID}{A numeric location identifier for the country, in this case 124".}
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

#' Example outputs file for 1x1 cohort component population projection for Canada 1950-2020
#'
#' All estimates generated for one country-variant by the full cohort component population projection.
#' This example  for Canada by single year of age and in one-year steps (1x1) from 1950-2020
#' Output is from function call 
#' project_ccmppWPP_one_country_variant(ccmpp_input = canada_wpp_1950_2020_ccmpp_inputs_1x1)
#'
#' @format A list with elements
#' \describe{
#' \item{LocID}{A numeric location identifier for the country, in this case 124".}
#' \item{variant}{A character string identifying the projection variant, in this case "Estimates".}
#' \item{pop_count_age_sex_1x1}{A data frame with population counts in the value column, by year, sex and single year of age.}
#' \item{pop_count_age_sex_1x1}{A data frame with population counts in the value column, by year, sex and five year age group.}
#' \item{pop_count_tot_sex}{A data frame with total population counts in the value column, by year and sex.}
#' \item{birth_count_age_b_1x1}{A data frame with birth counts (both sexes) in the value column, by year and single year of age of mother.}
#' \item{birth_count_age_b_5x1}{A data frame with birth counts (both sexes) in the value column, by year and five year age group of mother.}
#' \item{fert_rate_age_f_1x1}{A data frame with females' age-specific fertility rates in the value column, by year and single year of age.}
#' \item{fert_rate_age_f_5x1}{A data frame with females' age-specific fertility rates in the value column, by year and five year age group.}
#' \item{fert_pct_age_f_1x1}{A data frame with females' percentage age-specific fertility rates in the value column, by year and single year of age.}
#' \item{fert_pct_age_f_5x1}{A data frame with females' percentage age-specific fertility rates in the value column, by year and five year age group.}
#' \item{fert_rate_tot_f}{A data frame with females' total fertility rates in the value column, by year.}
#' \item{fert_mean_age_f}{A data frame with females' mean age at childbearing in the value column, by year.}
#' \item{srb}{A data frame with the sex ratio at birth (M/F) in the value column, by year. }
#' \item{death_count_age_sex_1x1}{A data frame with death counts in the value column, by year, sex and single year of age.}
#' \item{death_count_age_sex_5x1}{A data frame with death counts in the value column, by year, sex and five year age group.}
#' \item{death_count_tot_sex}{A data frame with total death counts in the value column, by year and sex.}
#' \item{death_count_cohort_sex_1x1}{A data frame with death counts in the value column, by year, sex and single year age cohort.}
#' \item{exposure_count_age_sex_1x1}{A data frame with person-years of exposure in the value column, by year, sex and single year of age.}
#' \item{exposure_count_age_sex_5x1}{A data frame with person-years of exposure in the value column, by year, sex and five year age group.}
#' \item{lt_complete_age_sex}{A data frame with life table column labels in the indicator column and life table values 
#' in the value column, by year, sex and single year of age.}
#' \item{lt_abridged_age_sex}{A data frame with life table column labels in the indicator column and life table values 
#' in the value column, by year, sex and abridged age groups (0,1,5,10,15....)}
#' \item{lt_summary}{A data frame with life table summary indicator labels in the indicator column and life table values 
#' in the value column, by sex (example labels: lt_1q0, lt_4q1, lt_35q15, lt_e0, lt_e60, etc.).}
#' \item{mig_net_count_age_sex_1x1}{A data frame with net migrant counts in the value column, by year, sex and single year of age.}
#' \item{mig_net_count_age_sex_5x1}{A data frame with net migrant counts in the value column, by year, sex and five year age group.}
#' \item{mig_net_count_tot_sex}{A data frame with total net migrant counts in the value column, by year and sex.}
#' \item{mig_assumption}{A data frame with whether migration is accounted at "end" of period or "even" over period in the value column, by year.}
#' }
"canada_wpp_1950_2020_ccmppWPP_outputs.rda"


